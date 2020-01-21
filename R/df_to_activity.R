#' Home cage activity summary
#'
#' Computes activity (number of transitions) for each timebin of a HCAdf object
#' @param HCAdf HCAdf object
#' @param aggre.time timebins in seconds
#' @param norm If TRUE, antenna breaks per hour will be returned. If FALSE, total number of breaks in timebin is returned. 
#' @param strict.input Error if input is not of class HCAdf
#' @return activity summary
#' @export
#' @examples
#' \dontrun{
#'   test = HCAdf_to_activity(asdfsadf)
#' }


HCAdf_to_activity = function(HCAdf,aggre.time=900,norm=TRUE,strict.input=F) {
   
   if ( !('HCAdf' %in% class(HCAdf)) ) {
      if (strict.input) {
         stop('Object is not of class HCAdf. Stopping...')
      } else {
         warning('Object is not of class HCAdf. Proceeding anyways but be careful...')
      }
   }

    nc = ncol(HCAdf) ; cn = colnames(HCAdf)
    HCAdf$dayvec = tvec_to_dayvec(HCAdf$t)
    tvec = HCAdf$t
    dounsampledtimevals = as.numeric(tvec)%/%aggre.time
    ld = list(dounsampledtimevals)
    nd = 1:length(dounsampledtimevals)
    statemat = HCAdf[, !(colnames(HCAdf) %in% c("t", "dayvec"))]

    t_aggre = aggregate(tvec, ld, mean)[, -1]
    activity = aggregate(nd, ld, function(x) 
                   homeCageActivity(x, statemat))[, -1]

    if (norm) {
       tw = aggregate(tvec, ld, function(x) diff(range(x)))[, -1] / 3600
       if (is.null(dim(activity))) {
          activity = activity/tw
       } else {
          for (i in 1:ncol(activity)) {
             activity[,i] = activity[,i]/tw
          }
       }
    }
    colnames(activity) = gsub("_xd", "", colnames(activity))
    aggre_summary = cbind( data.frame(t = t_aggre) , activity )
    aggre_summary$dayvec = tvec_to_dayvec(aggre_summary$t)
    class(aggre_summary) = c("HCAactivity", class(aggre_summary))
    return(aggre_summary)
}

homeCageActivity = function (idx, statemat) 
{
    if (is.null(dim(statemat))) {
        x = statemat[idx]
        retvec = sum( diff(x) != 0 )
    }
    else {
        retvec = apply(statemat[idx, ], 2, function(x) sum( diff(x) != 0 ))
    }
    return(retvec)
}
