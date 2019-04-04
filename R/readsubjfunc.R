#' Subjects in HDF5 files
#'
#' List subjects in HCA HDF5 files
#' @param i HDF5 filename
#' @return List of subjects
#' @keywords internal
#' @import rhdf5
#' @examples
#' \dontrun{
#'   readsubjfunc('1547743375205_000095_arbC8123198@LT-GREEN_0000085500200.hdf5')
#' }


readsubjfunc = function(i) { 
       subset(h5ls(i),group=="/subjects")[,'name'] }
