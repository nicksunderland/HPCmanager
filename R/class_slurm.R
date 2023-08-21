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
#' @slot script_path character.
#' @slot run_path character.
#'
#' @return a Slurm object
#' @importFrom methods slot new validObject callNextMethod
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
    script_path = "character",
    run_path = "character"

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
    max_time_hours = 1L,
    max_time_mins = 0L,
    max_time_secs = 0L,
    script_path = character(),
    run_path = character()
  )
)

setMethod(
  f = "initialize",
  signature = "Slurm",
  definition =function(.Object, job_name, account, script_path, ...) {
    print("initialize")
    .Object <- callNextMethod(.Object, ...)
    .Object@job_name <- job_name
    .Object@account <- account
    .Object@script_path <- script_path

    print(.Object@script_path)

    .Object@run_path <- sub(".R$", "_run.R", .Object@script_path)

    print(.Object@run_path)

    writeLines(c("testing"), .Object@run_path)

#
#     # Create the preamble
#     slurm_preamble <- paste0(
#       c(
#         "#!/bin/bash",
#         paste0("#SBATCH --job-name=", .Object@job_name),
#         paste0("#SBATCH --partition=", .Object@partition),
#         paste0("#SBATCH --nodes=", as.character(.Object@nodes)),
#         paste0("SBATCH --cpus-per-task=", as.character(.Object@cpu_per_task)),
#         paste0("#SBATCH --time=", sprintf('%d-%02d:%02d:%02d',
#                                           .Object@max_time_days,
#                                           .Object@max_time_hours,
#                                           .Object@max_time_mins,
#                                           .Object@max_time_secs)),
#         paste0("#SBATCH --mem=", as.character(.Object@mem_per_cpu), "M")
#       ),
#       collapse = "\n"
#     )
#     print(slurm_preamble)
#
#     # Create the R script
#     r_fp <- .Object@script_path  #get_script_path()
# cat("R script path: ", r_fp)
#     script_dir <- dirname(r_fp)
# cat("R script dir: ", script_dir)
#     r_script <- readLines(r_fp)
# cat("The r script")
# print(r_script)
#     r_script <- paste0(r_script, collapse="\n")
#     run_script <- sub("library[(][\"']?HPCmanager[\"']?[)]", "", r_script)
#     run_script <- sub("\n[0-9A-z_. ]*(?:<-|=)?[ ]*Slurm[(][A-z0-9= \"',.\n]*[)]", "", run_script)
# cat("The run script")
# print(run_script)
#     run_script_fp <- sub(".R$", "_run.R", r_fp)
# cat("The run script path: ", run_script_fp)
#     writeLines(run_script, run_script_fp)
#
#
#     # Create the bash script
#     bash_lines <- paste0(
#     c(
#         slurm_preamble,
#         "module load R",
#         paste0("Rscript ", run_script_fp)
#     ),
#     collapse = "\n"
#     )
# cat("The bash script")
# print(bash_lines)
#
#     bash_script_path <- sub("_run.R$", "_bash.sh", run_script_fp)
# cat("The bash script path: ", bash_script_path)
#     writeLines(bash_lines, bash_script_path)
#
#
#     cat("Submitting bash script: ", bash_script_path)
#     sbatch_return <- system(paste("sbatch", bash_script_path), intern = TRUE)
#     job_id <- sub("Submitted batch job ", "", sbatch_return)
#     cat("Job ID: ", job_id)
#     print(sbatch_return)
#
#     opt <- options(show.error.messages = FALSE)
#     on.exit(options(opt))
#     suppressWarnings({stop()})

     .Object
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




