#' @title Stub function
#' @return nothing
#' @noRd
stub <- function() {}

#' @title Here - where am I?
#' @description
#' https://gist.github.com/jasonsychau/ff6bc78a33bf3fd1c6bd4fa78bbf42e7
#'
#' @return file path to where `here()` was called.
#' @importFrom knitr current_input
#' @importFrom rstudioapi getSourceEditorContext isAvailable
#' @export
#'
here <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (length(grep("^-f$", cmdArgs)) > 0) {
    # R console option
    normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1]
  } else if (length(grep("^--file=", cmdArgs)) > 0) {
    # Rscript/R console option
    scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
  } else if (Sys.getenv("RSTUDIO") == "1") {
    if (rstudioapi::isAvailable(version_needed=NULL,child_ok=FALSE)) {
      # RStudio interactive
      dirname(rstudioapi::getSourceEditorContext()$path)
    } else if (is.null(knitr::current_input(dir = TRUE)) == FALSE) {
      # Knit
      knitr::current_input(dir = TRUE)
    } else {
      # R markdown on RStudio
      getwd()
    }
  } else if (is.null(attr(stub, "srcref")) == FALSE) {
    # 'source'd via R console
    dirname(normalizePath(attr(attr(stub, "srcref"), "srcfile")$filename))
  } else {
    stop("Cannot find file path")
  }
}

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
#' @slot test_path string path for the file where Slurm object created
#'
#' @return a Slurm object
#' @importFrom methods slot new validObject
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
    mem_per_cpu = "integer",
    test_path = "ANY"

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
    max_time = lubridate::duration(1, units="days"),
    mem_per_cpu = 100L,
    test_path = character()
  )
)

setGeneric("initialize", function(object){standardGeneric("initialize")})
setMethod(
  f = "initialize",
  signature = "Slurm",
  definition =function(object) {
    #object@test_path <- here()
    validObject(object)
    object
})



#' @title queue
#' @description
#' A short description...
#' @param object a Slurm object
#' @return side effects, plus the Slurm object
#' @export
#' @rdname queue
#'
setGeneric("queue", function(object){standardGeneric("queue")})

#' @rdname queue
setMethod(
  f = "queue",
  signature = "Slurm",
  definition = function(object) {

    cat("I'm a slurm object.", object@job_name, object@account, sep = "\n" )

  }
)

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
      #SBATCH --nodes={object@nodes}
      #SBATCH --cpus-per-task={object@cpu_per_task}
      #SBATCH --time={format_time(object@max_time)}
      #SBATCH --mem={object@mem_per_cpu}M
    "
    )
  }
)

#' format_time
#'
#' @param duration lubridate duration object
#'
#' @return a formated string e.g. '4-00:01:00' 4 days 1 hour
#' @importFrom lubridate duration day minute second seconds_to_period
#' @noRd
#'
format_time <- function(duration) {
  secs = lubridate::duration(duration, units="seconds")
  period = lubridate::seconds_to_period(secs)
  text = sprintf('%d-%02d:%02d:%02d',
                 lubridate::day(period),
                 period@hour,
                 lubridate::minute(period),
                 lubridate::second(period))
}



