#' Convert HCA object to HCA dataframe
#'
#' Convert HCA object to HCA dataframe with column for each subject's 1D position
#' @param subjectdatalist input HCA object
#' @param strict.input Error if input is not of class HCA
#' @return Dataframes with columns for time and subject's position
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#'   subjectdatalist_to_df(subjectdatalist)
#' }

hca_to_hcadf = function(subjectdatalist,strict.input=F) {
   if ( !('HCA' %in% class(subjectdatalist)) ) {
      if (strict.input) {
         stop('Object is not of class HCA. Stopping...')
      } else {
         warning('Object is not of class HCA. Proceeding anyways but be careful...')
      }
   }
   
   i2 = 0
   for (i in 1:length(subjectdatalist)) {
      if (is.null(subjectdatalist[[i]])) {
         cat(paste('No Entries found for RFID:',names(subjectdatalist)[i],'\n'))
         next
      }
      xd = (( subjectdatalist[[i]][,'antenna'] -1) %/% 3 )+1
      dftojoin = data.frame(t=subjectdatalist[[i]][,"t"], xd=xd )
      colnames(dftojoin)[2]=paste0(names(subjectdatalist)[i],"_xd")
      i2 = i2 + 1
      if ( i2==1 )  { outdf=dftojoin ; next }
      outdf=inner_join(outdf,dftojoin,by='t')
   }
   class(outdf) = c('HCAdf',class(outdf))
   return(outdf)
}
