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
#' @slot directory character.
#' @slot modules character.
#' @slot r_version character.
#' @slot rscript_options character.
#' @slot array integer array c(1,2,3,4) etc
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
    directory = "character",
    array = "integer",
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
    # slurm header things
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
    directory = ".",
    array = integer(),
    # bash script things
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
      #SBATCH --chdir={.Object@directory}
      #SBATCH --mem={as.character(.Object@mem_per_cpu)}M"
    )

    # Optional flags
    array <- if(length(.Object@array) > 0) glue("#SBATCH --array={paste0(.Object@array, collapse=',')}") else ""
    array_arg <- if(length(.Object@array) > 0) "--SLURM_ARRAY_TASK_ID=\"${SLURM_ARRAY_TASK_ID}\"" else ""
    optional_flags <- c(array)

    # Add the optional flags
    slurm_preamble <- paste0(c(slurm_preamble, optional_flags[optional_flags!=""]), collapse="\n")
    cat("Creating slurm header:\n", slurm_preamble, "\n")

    # Get the path to 'this' R script and get its contents as a string
    script_path <- get_script_path()
    cat("Reading raw R script:\t", script_path, "\n")
    raw_script_lines <- readLines(script_path)

    # create a new R script, remove the Slurm() object code between "#SBATCH" flags
    slurm_header_line_idxs <- grep("#SBATCH", raw_script_lines)
    stopifnot(length(slurm_header_line_idxs) == 2)
    start <- slurm_header_line_idxs[1]
    end <- slurm_header_line_idxs[2]
    run_script <- raw_script_lines[-(start:end)]


    run_path <- create_path(script_path, "run", .Object@directory)
    cat("Creating new R script:\t", run_path, "\n")
    writeLines(run_script, run_path)

    # Create the bash script
    bash_path <- create_path(script_path, "bash", .Object@directory)
    modules <- ""
    if(length(.Object@modules) > 0) {
      for(mod in .Object@modules) {
        modules <- c(modules, paste0("module load ", mod))
      }
    }
    modules <- paste0(modules, collapse="\n")

    # Build the bash script
    bash_script <- glue(
      "{slurm_preamble}

      module load languages/r/{.Object@r_version}
      {modules}

      Rscript {.Object@rscript_options} {run_path} {array_arg}"
    )
    cat("Creating bash script:\t", bash_path, "\n")
    writeLines(bash_script, bash_path)

    # Submit the bash script
    cat("Submitting bash script.\n")
    sbatch_return <- system(paste("sbatch", bash_path), intern = TRUE)
    job_id <- sub("Submitted batch job ", "", sbatch_return)
    cat(sbatch_return, "\n")
    cat("Enter: ", paste0("sacct -j ", job_id), "to view progress\n")

    # exit and don't process the rest of the script in this R session
    # the newly generated R script above will be submitted to the HPC
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    suppressWarnings({stop()})

    # don't return anything as this process just terminates the script
    #.Object
})

