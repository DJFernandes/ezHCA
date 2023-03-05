#' Read many HCA HDF5 files
#'
#' Read a series of HCA HDF5 files and return a list of dataframes, one for each subjects
#' @param hdf5files input HDF5 files
#' @param subjectIDs List of subjects IDs. If NULL, the subjects are automatically determined.
#' @param parallel number of local cores to use. If NULL, process is run in serial.
#' @return List of dataframes, one for each subjects.
#' @import rhdf5
#' @import pbapply
#' @export
#' @examples
#' \dontrun{
#'   hdf5files = c(
#'                  '1547743375205_000093_arbC8123198@LT-GREEN_0000083700200.hdf5'
#'                  '1547743375205_000094_arbC8123198@LT-GREEN_0000084600200.hdf5',
#'                  '1547743375205_000095_arbC8123198@LT-GREEN_0000085500200.hdf5',
#'                )
#'   hcaGetHDF5series(hdf5files)
#' }

hcaReadHDF5series = function( hdf5files , subjectIDs = NULL , parallel = NULL ) {

   if (is.null(subjectIDs)) {
     cat('Finding RFIDs in files\n')
     # function to open and read all subject informations
     readsubjfunc = function(i) { 
            subset(no_message_h5ls(i),group=="/subjects")[,'name'] }
  
     # read and find subjects in all files
     subjectIDs=unlist(pblapply(hdf5files,readsubjfunc,cl=parallel))
  
     #Detected IDs
     subjectIDs=sort(names(sort(table(subjectIDs[subjectIDs!="?"]),decreasing=TRUE)))
     
     cat('The following RFIDs were detected\n')
     cat('---------------------------------\n')
     cat(subjectIDs,sep='\n')
     cat('---------------------------------\n')
   }
   # read all the data
   filedatalist_tree = pblapply(hdf5files, function(i) hcaReadHDF5(i,subjectIDs),cl=parallel)
   
   # create subject datalist
   subjectdatalist = base::lapply(subjectIDs , function(s) {
      do.call('rbind',
        base::lapply(1:length(filedatalist_tree) , function(i)
           filedatalist_tree[[i]][[s]]
        ))
   }) 

   names(subjectdatalist) = subjectIDs
   class(subjectdatalist) = 'HCA'
   return(subjectdatalist)
}

