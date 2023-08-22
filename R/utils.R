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
  print(run.arguments)

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
