#' Plot HCA entropy
#'
#' Plot method for HCAentropy object
#' @param hcaentdf HCAentropy object
#' @param idxs Which subject is plotted. If NULL, all plots are made.
#' @return Plot of subjects' entropy timeseries
#' @import ggplot2
#' @import grid
#' @method plot HCAentropy
#' @export

plot.HCAentropy = function( hcaentdf , idxs = NULL ) {
   bool = colnames(hcaentdf) %in% c('t','dayvec')
   attrdf = hcaentdf[,bool]   ;   statedf = hcaentdf[,!bool]
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
          pldf = data.frame(attrdf,entropy = statedf)
      } else {
          pldf = data.frame(attrdf,entropy = statedf[,x])
          subjnm = colnames(statedf)[x]
      }
      ggplot(pldf, aes(t,entropy))+geom_point()+ggtitle(subjnm)+
        theme(plot.title = element_text(hjust = 0.5))
   })

   # TODO: night shades
   gridExtra::grid.arrange(grobs = pllist , ncol = length(pld))
}
