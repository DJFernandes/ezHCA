#' Annotate time with day or night
#'
#' Converts a vector of POSIXct or POSIXt into day or night
#' @param posix_time vector of times given in POSIXct or POSIXt format
#' @param daystart time when the light phase starts
#' @param nightstart time when the dark phase starts
#' @return vector of day or night
#' @keywords internal
#' @import chron
#' @examples
#' \dontrun{
#'   test = tvec_to_dayvec(asdfsadf)
#' }

tvec_to_dayvec = function(posix_time , daystart = times("07:00:01") , nightstart = times('19:00:00') ) {
    tv = times(sapply(strsplit(as.character(posix_time),split=" "),function(x) x[2]))
    dayvec = factor(
                    ifelse( tv > daystart & tv < nightstart , 'day' , 'night' ) , 
                    levels=c('day','night'))
    return(dayvec)
}
