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
