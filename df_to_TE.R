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

   # sort HCAdf by time to avoid shenanigans
   HCAdf = arrange(HCAdf,t)

   # aggregate HCAdf data by second (which is the minimum time interval)
   # take mode if needed
   getmode = function(v) { as.numeric(names(tail(sort(table(v)),1))) }
   HCAdf = aggregate(as.integer(HCAdf$t),HCAdf,getmode) %>% 
               select(-x) %>% arrange(t)
   data_duration = difftime(max(HCAdf$t),min(HCAdf$t),units = "secs")
   t_origin = min(HCAdf$t)
   
   # Lag time df 
   lag_df = HCAdf
   lag_df$t = HCAdf$t + lagtime
   cbool = ( colnames(HCAdf) != 't' )
   colnames(lag_df)[cbool] = gsub('_xd','_lxd',colnames(HCAdf)[cbool])
   
   # Merge Positions and lagged positions
   HCAdf = inner_join(HCAdf,lag_df,by='t')
   cbool = ( colnames(HCAdf) != 't' )

   # check if data is empty (i.e. time lag is longer than data duration) 
   short_duration_flag = FALSE
   if (nrow(HCAdf) == 0) {
      short_duration_flag = TRUE
      warning_msg = paste0(
         "Lag Time (",lagtime,") ",
         "is longer than ",
         "data duration (",data_duration,")"
      )
      warning(warning_msg)
      
      HCAdf[1,cbool] = 0
      HCAdf[1,!cbool] = t_origin
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
        aggregate(nd , ld, function(x) TransEnt(x,statemat,norm=norm))[,-1]
      )
   colnames(aggre_summary)[-1]=gsub('^X','',colnames(aggre_summary)[-1])   

   aggre_summary$dayvec = tvec_to_dayvec(aggre_summary$t)
   cbool = !( colnames(aggre_summary) %in% c('t','dayvec') )

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
   
   if (short_duration_flag) {
      if (!int.pairs) {
         aggre_summary[,c('affTE','effTE')] = NA
      } else {
         aggre_summary[,cbool] = NA
      }
   }

   class(aggre_summary) = c('HCAte',class(aggre_summary))
   return(aggre_summary)
}



# older version of the code to create lag time that didn't work with Chien's data
#   # Lag time vector
#   bool = !( ( HCAdf$t - HCAdf$t[1] )  < lagtime )
#   lag_t = HCAdf$t[ bool ]
#
#   # Cropped xd
#   lag_xd = HCAdf[ 1:(nrow(HCAdf) - sum(!bool)) , colnames(HCAdf) != 't' ]
#   colnames(lag_xd) = gsub('_xd','_lxd',colnames(lag_xd))
#
#   lag_df = data.frame(t=lag_t , lag_xd)
#   colnames(lag_df)[-1] = colnames(lag_xd)
#
#   # Merge Positions and lagged positions
#   HCAdf = inner_join(HCAdf,lag_df,by='t')
