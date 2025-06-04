#' Home cage entropy summary
#'
#' Computes entropy for each timebin of a HCAdf object
#' @param HCAdf HCAdf object
#' @param dim number of dimensions to consider. Can only be 1 or 2. If 1, then only mouse position along the length of the cage is analysed
#' @param aggre.time timebins in seconds
#' @param norm flag to compute entropy
#' @param strict.input Error if input is not of class HCAdf
#' @return entropy summary
#' @export
#' @examples
#' \dontrun{
#'   test = HCAdf_to_entropy(asdfsadf)
#' }


HCAdf_to_entropy = function(HCAdf,dim=1,aggre.time=900,norm=TRUE,strict.input=F) {
   
   if ( !('HCAdf' %in% class(HCAdf)) ) {
      if (strict.input) {
         stop('Object is not of class HCAdf. Stopping...')
      } else {
         warning('Object is not of class HCAdf. Proceeding anyways but be careful...')
      }
   }
   
   if (!(dim %in% c(1,2))) {
      stop('dim must only be 1 or 2')
   }
   
   
   HCAdf = position_to_state(HCAdf,dim)

    nc = ncol(HCAdf) ; cn = colnames(HCAdf)
#    HCAdf$dayvec = tvec_to_dayvec(HCAdf$t)
    tvec = HCAdf$t
    dounsampledtimevals = as.numeric(tvec)%/%aggre.time
    ld = list(dounsampledtimevals)
    nd = 1:length(dounsampledtimevals)
    statemat = HCAdf[, !(colnames(HCAdf) %in% c("t", "dayvec"))]
    aggre_summary = data.frame(t = aggregate(tvec, ld, mean)[, 
        -1], aggregate(nd, ld, function(x) homeCageEntropy(x, 
        statemat, norm = norm))[, -1])
    colnames(aggre_summary) = gsub("_state$", "", cn)

    aggre_summary$dayvec = tvec_to_dayvec(aggre_summary$t)
    class(aggre_summary) = c("HCAentropy", class(aggre_summary))
    return(aggre_summary)
}

#' Convert positions to state
#'
#' Take discrete x and y positions and generate the state. 
#' If dim is 1, then only the x-position is used for the state. 
#'
#' @param HCAdf HCAdf object
#' @param dim number of dimensions to consider. Can only be 1 or 2. If 1, then only mouse position along the length of the cage is analysed
#' @return dataframe with positions converted to states
#' @keywords internal
#' @noRd
position_to_state = function(HCAdf,dim) {
   if (dim == 1) {
      # subset only x_positions and rename to state
      HCAdf = HCAdf[,c('t',grep('_xd',colnames(HCAdf),value=T))]
      colnames(HCAdf) = gsub('_xd$','_state',colnames(HCAdf))
   } else {
      # get all IDs
      IDs = gsub('_xd$','',grep('_xd',colnames(HCAdf),value=T))
      newdat = do.call('cbind',lapply(IDs, function(ID) {
           xd = HCAdf[,paste0(ID,'_xd')]
           yd = HCAdf[,paste0(ID,'_yd')]
           state = (xd-1)*3 + (yd - 1) + 1
      }))
      colnames(newdat) = paste0(IDs,'_state')
      newdat = as.data.frame(newdat)
      HCAdf = cbind(t = HCAdf$t,newdat)
   }
   return(HCAdf)
}
