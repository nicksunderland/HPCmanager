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
#' @slot max_time_days integer
#' @slot max_time_hours integer
#' @slot max_time_mins integer
#' @slot max_time_secs integer
#' @slot mem_per_cpu integer.
#' @slot modules character.
#' @slot r_version character.
#' @slot rscript_options character.
#'
#' @return a Slurm object
#' @importFrom methods slot new validObject callNextMethod
#' @importFrom glue glue
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
    max_time_days = "integer",
    max_time_hours = "integer",
    max_time_mins = "integer",
    max_time_secs = "integer",
    mem_per_cpu = "integer",
    modules = "character",
    r_version = "character",
    rscript_options = "character"

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
    mem_per_cpu = 100L,
    max_time_days = 0L,
    max_time_hours = 0L,
    max_time_mins = 1L,
    max_time_secs = 0L,
    modules = character(),
    r_version = "4.2.1",
    rscript_options = ""
  )
)

setMethod(
  f = "initialize",
  signature = "Slurm",
  definition =function(.Object, job_name, account, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@job_name <- job_name
    .Object@account <- account

    # Create the preamble
    slurm_preamble <- glue(
     "#!/bin/bash
      #SBATCH --job-name={.Object@job_name}
      #SBATCH --account={.Object@account}
      #SBATCH --partition={.Object@partition}
      #SBATCH --cpus-per-task={as.character(.Object@cpu_per_task)}
      #SBATCH --nodes={as.character(.Object@nodes)}
      #SBATCH --cpus-per-task={as.character(.Object@cpu_per_task)}
      #SBATCH --time={sprintf('%d-%02d:%02d:%02d',
                              .Object@max_time_days,
                              .Object@max_time_hours,
                              .Object@max_time_mins,
                              .Object@max_time_secs)}
      #SBATCH --mem={as.character(.Object@mem_per_cpu)}M"
    )
    cat("Creating slurm header:\n", slurm_preamble, "\n")

    # Get the path to 'this' R script and get its contents as a string
    script_path <- get_script_path()
    cat("Reading raw R script:\t", script_path, "\n")
    raw_script_lines <- readLines(script_path)

    # create a new R script, remove 'library(HPCmanager) code between   "#SBATCH" flags
    slurm_header_line_idxs <- grep("#SBATCH", raw_script_lines)
    stopifnot(length(slurm_header_line_idxs) == 2)
    start <- slurm_header_line_idxs[1]
    end <- slurm_header_line_idxs[2]
    run_script <- raw_script_lines[-(start:end)]
    run_path <- sub(".R$", "_run.R", script_path)
    cat("Creating new R script:\t", run_path, "\n")
    writeLines(run_script, run_path)

    # Create the bash script
    bash_path <- sub(".R$", "_bash.sh", script_path)
    modules <- ""
    if(length(.Object@modules) > 0) {
      for(mod in .Object@modules) {
        modules <- c(modules, paste0("module load ", mod))
      }
    }
    modules <- paste0(modules, collapse="\n")
    bash_script <- glue(
      "{slurm_preamble}

      module load languages/r/{.Object@r_version}
      {modules}


      Rscript {.Object@rscript_options} {run_path}"
    )
    cat("Creating bash script:\t", bash_path, "\n")
    writeLines(bash_script, bash_path)

    # Submit the bash script
    cat("Submitting bash script.\n")
    sbatch_return <- system(paste("sbatch", bash_path), intern = TRUE)
    job_id <- sub("Submitted batch job ", "", sbatch_return)
    cat("Job ID: ", job_id, "\n")
    cat(sbatch_return, "\n")


    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    suppressWarnings({stop()})

     #.Object
})


#' #' @title write_preamble
#' #' @description
#' #' A short description...
#' #' @param object a Slurm object
#' #' @return a string, the Slurm preamble
#' #' @export
#' #' @rdname write_preamble
#' #'
#' setGeneric("write_preamble", function(object){standardGeneric("write_preamble")})
#'
#' #' @rdname write_preamble
#' setMethod(
#'   f = "write_preamble",
#'   signature = "Slurm",
#'   definition = function(object) {
#'     s <- paste0(
#'     c(
#'     "#!/bin/bash",
#'     "#SBATCH --job-name=", object@job_name,
#'     "#SBATCH --partition=", object@partition,
#'     "#SBATCH --nodes=", as.character(object@nodes),
#'     "SBATCH --cpus-per-task=", as.character(object@cpu_per_task),
#'     "#SBATCH --time=", sprintf('%d-%02d:%02d:%02d',
#'                                object@max_time_days,
#'                                object@max_time_hours,
#'                                object@max_time_mins,
#'                                object@max_time_secs),
#'     "#SBATCH --mem=", as.character(object@mem_per_cpu), "M"
#'     ),
#'     collapse = "\n"
#'     )
#'     return(s)
#'   }
#' )


#' #' @title write_r_script
#' #' @description
#' #' A short description...
#' #' @param object a Slurm object
#' #' @return a string, the r script
#' #' @export
#' #' @rdname write_r_script
#' #'
#' setGeneric("write_r_script", function(object){standardGeneric("write_r_script")})
#'
#' #' @rdname write_r_script
#' setMethod(
#'   f = "write_r_script",
#'   signature = "Slurm",
#'   definition = function(object) {
#'
#'     r_fp <- get_script_path()
#'     script_dir <- dirname(r_fp)
#'     r_script <- readLines(r_fp)
#'
#'     # adjust and write out
#'     r_script <- paste0(r_script, collapse="\n")
#'     run_script <- sub("library[(][\"']?HPCmanager[\"']?[)]", "", r_script)
#'     run_script <- sub("\n[0-9A-z_. ]*(?:<-|=)?[ ]*Slurm[(][A-z0-9= \"',.\n]*[)]", "", run_script)
#'     run_script_fp <- sub(".R$", "_run.R", r_fp)
#'     writeLines(run_script, run_script_fp)
#'
#'     return(run_script_fp)
#'   }
#' )

#'
#' #' @title write_r_bash_script
#' #' @description
#' #' A short description...
#' #' @param object a Slurm object
#' #' @return a string, the bash script
#' #' @export
#' #' @rdname write_r_bash_script
#' #'
#' setGeneric("write_r_bash_script", function(object){standardGeneric("write_r_bash_script")})
#'
#' #' @rdname write_r_bash_script
#' setMethod(
#'   f = "write_r_bash_script",
#'   signature = "Slurm",
#'   definition = function(object) {
#'
#'     #print("write_r_bash_script()")
#'     slurm_preamble <- write_preamble(object)
#'     #print(slurm_preamble)
#'
#'     run_script_path <- write_r_script(object)
#'     .Object@run_path <- run_script_path
#'     #print(run_script_path)
#'
#'     # bash
#'     bash_lines <- paste0(
#'     c(
#'     slurm_preamble,
#'     "module load R",
#'     "Rscript ", run_script_path
#'     )
#'     )
#'
#'     bash_script_path <- sub("_run.R$", "_bash.sh", run_script_path)
#'     writeLines(bash_lines, bash_script_path)
#'     #print(bash_lines)
#'
#'   return(bash_script_path)
#'   }
#' )




