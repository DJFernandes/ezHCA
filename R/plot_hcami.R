#' Plot HCA MI
#'
#' Plot method for HCAmi object
#' @param hcamidf HCAmi object
#' @param idxs Which subject is plotted. If NULL, all plots are made.
#' @return Plot of subjects' mutual information timeseries
#' @import ggplot2
#' @import grid
#' @method plot HCAmi
#' @export

plot.HCAmi = function( hcamidf , idxs = NULL ) {
   bool = colnames(hcamidf) %in% c('t','dayvec')
   attrdf = hcamidf[,bool]   ;   statedf = hcamidf[,!bool]
   if (!is.null(idxs))   {   
      statedf = statedf[,idxs]
      if (length(idxs) == 1) {
         subjnm = colnames(statedf)[idxs]
      }
   }
   if (is.null(ncol(statedf))) {
      only.one.subject = T
      pld = 1
   } else {
      only.one.subject = F
      pld = 1:ncol(statedf)
   }

   pllist = lapply(pld , function(x) {
      if (only.one.subject) {
          pldf = data.frame(attrdf,MI = statedf)
      } else {
          pldf = data.frame(attrdf,MI = statedf[,x])
          subjnm = colnames(statedf)[x]
      }
      ggplot(pldf, aes(t,MI))+geom_point()+ggtitle(subjnm)+
        theme(plot.title = element_text(hjust = 0.5))
   })

   # TODO: night shades
   gridExtra::grid.arrange(grobs = pllist , ncol = length(pld))
}
