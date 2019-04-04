#' Find start time for HCA file
#'
#' Finds the start time for an HCA HDF5 file using the filename string
#' @param x input file
#' @return Start time in POSIXct format
#' @keywords internal
#' @examples
#' infile = '1547228190839_000001_arbC8350154@LT-PINK_0000003600000.hdf5'
#' find_start_time(infile)


find_start_time = function(x) {
   y=strsplit(basename(x),'_')[[1]] 
   studystart=as.numeric(y[1])/1000
   segmentstart=as.numeric(gsub('.hdf5$','',y[length(y)]))/1000

   origin = '1970-01-01 00:00.00 UTC'   #DEFAULT ORIGIN OF POSIXct OBJECTS
   return(as.POSIXct(studystart + segmentstart,origin=origin))
}
