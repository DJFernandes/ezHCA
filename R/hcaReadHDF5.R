#' Read a HCA HDF5 file
#'
#' Read an HCA HDF5 file and return a list of dataframes, one for each subjects
#' @param i input HDF5 file
#' @param subjectIDs List of subjects IDs. If NULL, the subjects are automatically determined
#' @param start.time The start time of HCA HDF5 file. If NULL, it is automatically determined.
#' @return List of dataframes, one for each subjects.
#' @import rhdf5
#' @export
#' @examples
#' \dontrun{
#'   hcaReadHDF5('1547743375205_000095_arbC8123198@LT-GREEN_0000085500200.hdf5')
#' }

hcaReadHDF5=function(i,subjectIDs = NULL,start.time = NULL) {

   filedatalistsubject=list()

   if (is.null(start.time)) {
      start.time=find_start_time(i)
   }

   subjects_in_this_file = subset(h5ls(i),group=="/subjects")[,'name']

   if (is.null(subjectIDs)) { subjectIDs = subjects_in_this_file }
     
   for (j in subjectIDs) {
     if (!j %in% subjects_in_this_file) {next}
     subjaddress = paste0('/subjects/',j,'/antenna/ant1/')  #address in hdf5file for subject
     
     subjdat = suppressWarnings({h5read(i,subjaddress)})[,c("bx","by",'t','antenna')]   #extract info

     tds = subjdat$t %/% 1000   # unionize by seconds
     subjdatds = data.frame( t = aggregate(subjdat[,'t'],list(tds),mean)[,'x']/1000,   #time in seconds
                             bx = aggregate(subjdat[,'bx'],list(tds),mean)[,'x'],
                             by = aggregate(subjdat[,'by'],list(tds),mean)[,'x'],
                             antenna = aggregate(subjdat[,'antenna'],list(tds),Mode)[,'x']
                           )
     subjdatds$t = subjdatds$t + start.time   #add starttime
     filedatalistsubject[j] = list(subjdatds)
   }
#   H5close()
   return(filedatalistsubject)
}
