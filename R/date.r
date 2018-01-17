#' Transform yearmon date in # of month since the start of the experiment  
#' 
#' @param date a yearmon object  
#' @param starting a yearmon object
#'
#' @details This function is a wrapper of lubridate functions to transform a
#' yearmonth object to a number of month since the start of the experiment. The
#' starting parameter is the starting month, defined by default to the month of
#' sapling transplantation.  
#'
#' @seealso zoo::as.yearmon lubridate::interval  lubridate::duration
#'
#' @return Returns a integer vector corresponding to a number of month since the
#' start of the experiment. 
#'
#' @examples
#'
#' @export
yearmon2month <- function(date, starting = zoo::as.yearmon("nov. 2015")) {

    time_span <- lubridate::interval(starting, date) / lubridate::duration(1, units = "weeks")
    res  <- time_span/4 # Convert in  month
    as.integer(res)
    return(res)
}
