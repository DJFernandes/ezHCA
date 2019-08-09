#' Transfer entropy summary
#'
#' Computes transfer entropy for each timebin of a HCAdf object
#' @param HCAdf HCAdf object
#' @param lagtime lag time for transfer entropy in seconds
#' @param aggre.time timebins in seconds
#' @param norm flag to compute entropy
#' @param int.pairs If true, it will compute the outgoing (affTE) and incoming entropy (effTE) for each individual. If false, it will be TE between all pairs.
#' @param strict.input Error if input is not of class HCAdf
#' @return transfer entropy summary
#' @export
#' @examples
#' \dontrun{
#'   test = HCAdf_to_TE(asdfsadf)
#' }

HCAdf_to_TE = function(HCAdf,lagtime=900,aggre.time=900,norm=TRUE,int.pairs=F,strict.input=F) {
   if ( !('HCAdf' %in% class(HCAdf)) ) {
      if (strict.input) {
         stop('Object is not of class HCAdf. Stopping...')
      } else {
         warning('Object is not of class HCAdf. Proceeding anyways but be careful...')
      }
   }

   # Lag time vector
   bool = !( ( HCAdf$t - HCAdf$t[1] )  < lagtime )
   lag_t = HCAdf$t[ bool ]

   # Cropped xd
   lag_xd = HCAdf[ 1:(nrow(HCAdf) - sum(!bool)) , colnames(HCAdf) != 't' ]
   colnames(lag_xd) = gsub('_xd','_lxd',colnames(lag_xd))

   lag_df = data.frame(t=lag_t , lag_xd)
   colnames(lag_df)[-1] = colnames(lag_xd)

   # Merge Positions and lagged positions
   HCAdf = inner_join(HCAdf,lag_df,by='t')

   nc = ncol(HCAdf) ; cn = colnames(HCAdf)
   HCAdf$dayvec=tvec_to_dayvec(HCAdf$t)
   tvec = HCAdf$t
   dounsampledtimevals = as.numeric(tvec) %/% aggre.time
   ld = list(dounsampledtimevals)
   nd = 1:length(dounsampledtimevals)
   statemat = HCAdf[,!(colnames(HCAdf) %in% c('t','dayvec'))]
   aggre_summary = data.frame(
        t=aggregate(tvec , ld , mean )[,-1],
        aggregate(nd , ld, function(x) TransEnt(x,statemat,norm=norm))[,-1]
      )
   colnames(aggre_summary)[-1]=gsub('^X','',colnames(aggre_summary)[-1])   

   aggre_summary$dayvec = tvec_to_dayvec(aggre_summary$t)

   if (!int.pairs) {
      ids = gsub('_xd','',colnames(HCAdf)[grep('_xd',colnames(HCAdf))])
      aggre_summary = lapply(ids, function(i) {
           afftevals = aggre_summary[,grep(paste0('_by_',i),colnames(aggre_summary))]
           if (!is.null(dim(afftevals))) {
              afftevals = rowSums(afftevals)
           }
           
           efftevals = aggre_summary[,grep(paste0(i,'_by_'),colnames(aggre_summary))]
           if (!is.null(dim(efftevals))) {
              efftevals = rowSums(efftevals)
           }

           dirTE = data.frame(
              affTE = afftevals,
              effTE = efftevals)

           data.frame(stringsAsFactors=F,select(aggre_summary,t,dayvec),dirTE,ID=i)
         }) %>% do.call(what = 'rbind')
   }

   class(aggre_summary) = c('HCAte',class(aggre_summary))
   return(aggre_summary)
}

