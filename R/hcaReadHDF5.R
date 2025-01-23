#' Read a HCA HDF5 file using rhdf5
#'
#' Read an HCA HDF5 file and return a list of dataframes, one for each subjects
#' @param i input HDF5 file
#' @param subjectIDs List of subjects IDs. If NULL, the subjects are automatically determined
#' @param start.time The start time of HCA HDF5 file. If NULL, it is automatically determined.
#' @param downsample If TRUE, Aggregate data by second (Default). If FALSE, don't aggregate. 
#' @return List of dataframes, one for each subjects.
#' @import rhdf5
#' @export
#' @examples
#' \dontrun{
#'   hcaReadHDF5('1547743375205_000095_arbC8123198@LT-GREEN_0000085500200.hdf5')
#' }

hcaReadHDF5=function(i,subjectIDs = NULL,start.time = NULL, downsample = T) {

   if (is.null(start.time)) {
      start.time=find_start_time(i)
   }
   
   subjects_in_this_file = subset(no_message_h5ls(i),group=="/subjects")[,'name']

   if (is.null(subjectIDs)) { subjectIDs = subjects_in_this_file }

   filedatalistsubject=list()     
   for (j in subjectIDs) {
     if (!j %in% subjects_in_this_file) {next}
     
     #address in hdf5file for subject
     subjaddress = paste0('/subjects/',j,'/antenna/ant1/')  
     
     #extract info
     subjdat = suppressWarnings({h5read(i,subjaddress)})[,c("bx","by",'t','antenna')]   
     
     # convert time from seconds to milliseconds 
     subjdat[,'t'] = subjdat[,'t']/1000

     if (downsample) {
        tds = floor(subjdat$t)      # unionize by seconds
        subjdatds = data.frame( 
           t = aggregate(subjdat[,'t'],list(tds),mean)[,'x'],   #time in seconds
           bx = aggregate(subjdat[,'bx'],list(tds),mean)[,'x'],
           by = aggregate(subjdat[,'by'],list(tds),mean)[,'x'],
           antenna = aggregate(subjdat[,'antenna'],list(tds),Mode)[,'x']
         )
        subjdat = subjdatds
     }
     
     subjdat$t = subjdat$t + start.time   #add starttime
     
     filedatalistsubject[j] = list(subjdat)
   }
#   H5close()
   return(filedatalistsubject)
}

#' suppress console messages from rhdf5::h5ls 
#'
#' Read an HCA HDF5 file and return a list of dataframes, one for each subjects
#' @param ... arguments to be passed to h5ls
#' @return Return from h5ls
#' @import rhdf5
no_message_h5ls = function(...) {
      sink(nullfile()) ; ret = h5ls(...) ; sink()
      return(ret)
   }
