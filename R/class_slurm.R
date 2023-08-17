

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
#' @importFrom methods slot new
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
  ),
  prototype = list(
    job_name = character(),
    account = character(),
    partition = "test",
    nodes = 1L,
    tasks_per_node = integer(),
    cpu_per_task = integer(),
    max_time = lubridate::duration(1, units="days")  ,
    mem_per_cpu = 100L
  )
)


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
  signature = c(object = "Slurm"),
  definition = function(object) {

    cat("I'm a slurm object.", object@job_name, object@account, sep = "\n" )

  }
)

