#' Convert HCA object to HCA dataframe
#'
#' Convert HCA object to HCA dataframe with column for each subject's 1D position
#' @param subjectdatalist input HCA object
#' @param dim number of dimensions to consider. Can only be 1 or 2. If 1, then only mouse position along the length of the cage is returned.
#' @param strict.input Error if input is not of class HCA
#' @return Dataframes with columns for time and subject's position
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#'   hca_to_hcadf(subjectdatalist)
#' }

hca_to_hcadf = function(subjectdatalist,dim=1,strict.input=F) {
   if ( !('HCA' %in% class(subjectdatalist)) ) {
      if (strict.input) {
         stop('Object is not of class HCA. Stopping...')
      } else {
         warning('Object is not of class HCA. Proceeding anyways but be careful...')
      }
   }
   if (!(dim %in% c(1,2))) {
      stop('dim must only be 1 or 2')
   }
   
   counter = 0
   for (i in 1:length(subjectdatalist)) {
      if (is.null(subjectdatalist[[i]])) {
         cat(paste('No Entries found for RFID:',names(subjectdatalist)[i],'\n'))
         next
      }

      xd = (( subjectdatalist[[i]][,'antenna'] -1) %/% 3 )+1
      dftojoin = data.frame(t=subjectdatalist[[i]][,"t"], xd=xd )
      colnames(dftojoin)[2]=paste0(names(subjectdatalist)[i],"_xd")

      if (dim != 1) {
         yd = (( subjectdatalist[[i]][,'antenna'] -1) %% 3 )+1
         dftojoin$yd = yd
         colnames(dftojoin)[3]=paste0(names(subjectdatalist)[i],"_yd")
      }
      
      counter = counter + 1
      if ( counter==1 )  { outdf=dftojoin ; next }
      outdf=inner_join(outdf,dftojoin,by='t')
   }
   class(outdf) = c('HCAdf',class(outdf))
   return(outdf)
}

#' Obtain distance and speed
#'
#' Obtain distance and speed from HCA object
#' @param subjectdatalist input HCA object
#' @param strict.input Error if input is not of class HCA
#' @return Dataframes with columns for interval distance travelled (mm) and interval speed (mm/s)
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#'   hca_to_dist_df(subjectdatalist)
#' }
hca_to_dist_df = function(subjectdatalist,strict.input=F) {
   if ( !('HCA' %in% class(subjectdatalist)) ) {
      if (strict.input) {
         stop('Object is not of class HCA. Stopping...')
      } else {
         warning('Object is not of class HCA. Proceeding anyways but be careful...')
      }
   }
   
   counter = 0 ; outdf_list = list()
   for (i in 1:length(subjectdatalist)) {
      if (is.null(subjectdatalist[[i]])) {
         cat(paste('No Entries found for RFID:',names(subjectdatalist)[i],'\n'))
         next
      }
      
      x = subjectdatalist[[i]]$bx
      y = subjectdatalist[[i]]$by
      t = subjectdatalist[[i]]$t
      a = subjectdatalist[[i]]$antenna
      
      dx = diff(x)
      dy = diff(y)
      dt = as.numeric(diff(t))
      
      ds = sqrt(dx^2 + dy^2)
      v = ds/dt
      
      counter = counter + 1
      outdf_list[[counter]] = data.frame(
         ID = names(subjectdatalist)[i],
         t = t[-1],
         bx = x[-1],
         by = y[-1],
         antenna = a[-1],
         ds = ds,
         v = v)
   }
   outdf = do.call('rbind',outdf_list)

   class(outdf) = c('HCAdistdf',class(outdf))
   return(outdf)
   
}
