#' Find mode
#'
#' Determine the most frequent value
#' @param x input vector
#' @param breaktie if TRUE (default), then tie is broken with the first value. If FALSE, all tied values are returned.
#' @return most frequent value in x
#' @keywords internal
#' @examples
#' x = c(1,1,1,2,2,2,3,4)
#' Mode(x) 
#' Mode(x,breaktie=F) 


Mode = function(x , breaktie = T) {
  y = unique(x)
  my = match(x, y)
  ty = tabulate(my)
  if (breaktie) {
     ret = y[which.max(ty)]
  } else {
     ret = y[ty == max(ty)]
  }
  return(ret)
}
