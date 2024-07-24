#' Plot HCA
#'
#' Plot method for HCA object
#' @param subjectdatalist HCA object
#' @param idxs Which index is plotted. If NULL, all plots are made.
#' @return Plot of subjects horizontal position
#' @import ggplot2
#' @import grid
#' @method plot HCA
#' @export


plot.HCA = function( subjectdatalist , idxs = NULL ) {
   if (!is.null(idxs))   subjectdatalist = subjectdatalist[idxs]

   pllist = lapply(1:length(subjectdatalist) , function(x) {
      pldf = subjectdatalist[[x]] %>% 
              mutate(xposition = ifelse( bx > 224.5 , 224.5 , bx) / 249 )
      plttl = names(subjectdatalist)[x]
      ggplot(pldf, aes(t,xposition))+geom_point()+ggtitle(plttl)+xlab('x')+
        coord_cartesian(ylim = c(0,1)) +
        theme(plot.title = element_text(hjust = 0.5))
   })

   # TODO: night shades
   gridExtra::grid.arrange(grobs = pllist , ncol = length(subjectdatalist))
}
