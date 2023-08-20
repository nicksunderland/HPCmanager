#' @title Slurm
#' @description
#' TODO: the description
#'
#' @slot job_name character.
#' @slot account character.
#' @slot partition character.
#' @slot nodes integer
#' @slot tasks_per_node integer
#' @slot cpu_per_task integer
#' @slot mem_per_cpu integer.
#' @slot max_time lubridate duration
#'
#' @return a Slurm object
#' @importFrom methods slot new validObject callNextMethod
#' @importClassesFrom lubridate Duration
#' @export
#'
Slurm <- setClass(
  Class = "Slurm",
  slots = list(
    job_name = "character",
    account = "character",
    partition = "character",
    nodes = "integer",
    tasks_per_node = "integer",
    cpu_per_task = "integer",
    max_time = "Duration",
    mem_per_cpu = "integer"

    # https://slurm.schedmd.com/archive/slurm-16.05.8/sbatch.html
    # --dependency
    # Set example, set this job to only start after another job has finished. This can be useful if you have a complex workflow which has certain parts depending on others.
    # --workdir
    # Set the working directory of your script. By default this will be the directory in which you run sbatch.
    # --output and --error
    # Set the output of the standard output and error streams of your job.
    # --exclusive
    # Make sure that this job has sole use of any nodes it runs on.
    # --gres

  ),
  prototype = list(
    job_name = character(),
    account = character(),
    partition = "cpu",
    nodes = 2L,
    tasks_per_node = 2L,
    cpu_per_task = 1L,
    max_time = lubridate::duration(1, units="hours"),
    mem_per_cpu = 100L
  )
)

setMethod(
  f = "initialize",
  signature = "Slurm",
  definition =function(.Object, job_name, account, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@job_name <- job_name
    .Object@account <- account
    validObject(.Object)
    bash_script_path <- write_r_bash_script(.Object)

    cat("Submitting bash script: ", bash_script_path)
    sbatch_return <- system(paste("sbatch", bash_script_path), intern = TRUE)
    job_id <- sub("Submitted batch job ", "", sbatch_return)
    cat("Job ID: ", job_id)

    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    suppressWarnings({stop()})



    # .Object
})


#' @title write_preamble
#' @description
#' A short description...
#' @param object a Slurm object
#' @return a string, the Slurm preamble
#' @importFrom glue glue
#' @export
#' @rdname write_preamble
#'
setGeneric("write_preamble", function(object){standardGeneric("write_preamble")})

#' @rdname write_preamble
setMethod(
  f = "write_preamble",
  signature = "Slurm",
  definition = function(object) {
    s <- glue::glue(
    "
    #!/bin/bash \n
    #SBATCH --job-name={object@job_name}
    #SBATCH --partition={object@partition}
    #SBATCH --nodes={as.character(object@nodes)}
    #SBATCH --cpus-per-task={as.character(object@cpu_per_task)}
    #SBATCH --time={format_time(object@max_time)}
    #SBATCH --mem={as.character(object@mem_per_cpu)}M
    "
    )
    return(s)
  }
)


#' @title write_r_script
#' @description
#' A short description...
#' @param object a Slurm object
#' @return a string, the r script
#' @importFrom glue glue
#' @export
#' @rdname write_r_script
#'
setGeneric("write_r_script", function(object){standardGeneric("write_r_script")})

#' @rdname write_r_script
setMethod(
  f = "write_r_script",
  signature = "Slurm",
  definition = function(object) {

    r_fp <- get_script_path()
    script_dir <- dirname(r_fp)
    r_script <- readLines(r_fp)

    # adjust and write out
    r_script <- paste0(r_script, collapse="\n")
    run_script <- sub("library[(][\"']?HPCmanager[\"']?[)]", "", r_script)
    run_script <- sub("\n[0-9A-z_. ]*(?:<-|=)?[ ]*Slurm[(][A-z0-9= \"',.\n]*[)]", "", run_script)
    run_script_fp <- sub(".R$", "_run.R", r_fp)
    writeLines(run_script, run_script_fp)

    return(run_script_fp)
  }
)


#' @title write_r_bash_script
#' @description
#' A short description...
#' @param object a Slurm object
#' @return a string, the bash script
#' @importFrom glue glue
#' @export
#' @rdname write_r_bash_script
#'
setGeneric("write_r_bash_script", function(object){standardGeneric("write_r_bash_script")})

#' @rdname write_r_bash_script
setMethod(
  f = "write_r_bash_script",
  signature = "Slurm",
  definition = function(object) {

    #print("write_r_bash_script()")
    slurm_preamble <- write_preamble(object)
    #print(slurm_preamble)

    run_script_path <- write_r_script(object)
    #print(run_script_path)

    # bash
    bash_lines <- glue(
    "
    {slurm_preamble}

    module load R

    Rscript {run_script_path}
    ")

    bash_script_path <- sub("_run.R$", "_bash.sh", run_script_path)
    writeLines(bash_lines, bash_script_path)
    #print(bash_lines)

  return(bash_script_path)
  }
)




