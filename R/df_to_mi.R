#' Mutual Information summary
#'
#' Computes nutual information for each timebin of a HCAdf object
#' @param HCAdf HCAdf object
#' @param aggre.time timebins in seconds
#' @param norm flag to compute entropy
#' @param strict.input Error if input is not of class HCAdf
#' @return mutual information summary
#' @export
#' @examples
#' \dontrun{
#'   test = HCAdf_to_entropy(asdfsadf)
#' }


HCAdf_to_MI = function(HCAdf,aggre.time=900,norm=TRUE,strict.input=F) {
   
   if ( !('HCAdf' %in% class(HCAdf)) ) {
      if (strict.input) {
         stop('Object is not of class HCAdf. Stopping...')
      } else {
         warning('Object is not of class HCAdf. Proceeding anyways but be careful...')
      }
   }
   nc = ncol(HCAdf) ; cn = colnames(HCAdf)
   HCAdf$dayvec=tvec_to_dayvec(HCAdf$t)
   tvec = HCAdf$t
   dounsampledtimevals = as.numeric(tvec) %/% aggre.time
   ld = list(dounsampledtimevals)
   nd = 1:length(dounsampledtimevals)
   statemat = HCAdf[,!(colnames(HCAdf) %in% c('t','dayvec'))]
   aggre_summary = data.frame(
        t=aggregate(tvec , ld , mean )[,-1],
        aggregate(nd , ld, function(x) condMI(x,statemat,norm=norm))[,-1]
      )
   colnames(aggre_summary)[-1]=gsub('_xd','',colnames(statemat))   

   aggre_summary$dayvec = tvec_to_dayvec(aggre_summary$t)
   class(aggre_summary) = c('HCAmi',class(aggre_summary))
   return(aggre_summary)   
}
