#' #' format_time
#' #'
#' #' @param duration lubridate duration object
#' #'
#' #' @return a formated string e.g. '4-00:01:00' 4 days 1 hour
#' #' @importFrom lubridate duration day minute second seconds_to_period
#' #' @noRd
#' #'
#' format_time <- function(duration) {
#'   secs = lubridate::duration(duration, units="seconds")
#'   period = lubridate::seconds_to_period(secs)
#'   text = sprintf('%d-%02d:%02d:%02d',
#'                  lubridate::day(period),
#'                  period@hour,
#'                  lubridate::minute(period),
#'                  lubridate::second(period))
#'   return(text)
#' }

#' @title Get script path
#' @description Find the path to the script calling this function
#' http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script#comment12780420_1815606
#' @return a string, the file path
#' @noRd
#'
get_script_path <- function() {
  cmdargs <- commandArgs(trailingOnly = FALSE)

  if (length(grep("^-f$", cmdargs)) > 0) {
    # R console
    # print("R console")
    # print(cmdargs)
    return(normalizePath(cmdargs[grep("^-f", cmdargs) + 1])[1])
  } else  if (length(grep("--file=", cmdargs)) > 0) {
    # Rscript
    # print("cmd line rscript")
    # print(cmdargs)
    return(normalizePath(gsub("--file=", "", cmdargs[grep("--file=", cmdargs)])))
  } else {
    # 'source'd via R console
    # print("source'd via R console")
    # print(cmdargs)
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}

#' @title get_argument
#' @description
#' credit to https://gist.github.com/abelcallejo/f557a7d4ce7e37bbe5306dae864d0f0f#command-line
#' @param parameter_name the parameter to look for
#' @return a string. argument passed
#' @export
#'
get_argument <- function(parameter_name) {

  # Set the valid run parameters
  valid.run.parameters <- c( "SLURM_ARRAY_TASK_ID" = as.integer )

  # make sure parameter is valid
  stopifnot("Not a valid parameter_name" = parameter_name %in% names(valid.run.parameters))

  # Get the run arguments
  run.arguments <- commandArgs(TRUE)

  # Loop each argument if and only if there are arguments
  if( length( run.arguments ) > 0 ) {

    for ( i in 1:length( run.arguments ) ) {

      # Validate if it has the --parameter=argument structure
      if (substr(run.arguments[i], 1, 2) == "--" && grepl("=", run.arguments[i], fixed=TRUE)) {

        # extract the key value pairs
        key.pair <- strsplit(run.arguments[i], "=", fixed = TRUE)[[1]]

        # remove the '--' prefix from the parameter
        run.parameter <- substring(key.pair[1], first=3)

        run.argument <- key.pair[2]

        # convert the argument to the correct type
        run.argument <- valid.run.parameters[[parameter_name]](run.argument)

        # return
        return(run.argument)
      }
    }
  }
}


#' Title
#' @description
#' Resolve the path in the same way that Slurm deals with the 'directory 'chdir' flag
#' https://slurm.schedmd.com/sbatch.html
#' 1. If directory starts with ".", then path is the current directory
#' 2. If directory starts with a "/", then path is considered absolute and the file is located there
#' 3. If directory starts with neither then a subdirectory is create below the raw .R script
#' @param path a string path to the raw .R script
#' @param type string. type of path to return, either "run" or "bash"
#' @param directory the directory path
#'
#' @return a path to either a "_run.R" or "_bash.sh" file
#' @export
#'
create_path <- function(path, type, directory) {

  # Check inputs
  valid.file.types = c("run", "bash")
  stopifnot("Not a valid type" = type %in% valid.file.types,
            "Not a valid directory" = is.character(directory) & length(directory)>0,
            "Not a valid file path" = file.exists(path),
            "Not a valid .R file" = grepl(".R$", path))

  # Get the file name
  raw_r_file_name <- sub(".R$", "", basename(path))
  raw_r_file_dir  <- dirname(path)

  # Work out where the file should go
  if(directory[1] == ".") {
    out_dir = raw_r_file_dir
  } else if(directory[1] == "/" | directory[1] == "\\") {
    stopifnot("Full directory path provided, but doesn't exist" = dir.exists(directory))
    out_dir = directory
  } else {
    out_dir = file.path(raw_r_file_dir, directory)
    if(!dir.exists(out_dir)) {
      dir.create(out_dir)
    }
  }

  # Add the file name and type
  if(type == "bash") {
    out_path = file.path(out_dir, paste0(raw_r_file_name, "_bash.sh"))
  } else if(type == "run") {
    out_path = file.path(out_dir, paste0(raw_r_file_name, "_run.R"))
  }

  return(out_path)
}
