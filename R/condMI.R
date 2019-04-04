#' Calculate conditional mutual information
#'
#' Calculate the conditional mutual information for a subset of the state matrix
#' @param idx rows of the state matrix for which to compute mutual information
#' @param statemat state matrix where each individual is a column and elements are observed states
#' @param norm Should the overall result be normalized to the total entropy
#' @return mutual information for each individual in nats
#' @keywords internal
#' @import infotheo
#' @examples
#' \dontrun{
#'   test = condMI(idx,statemat)
#' }

condMI = function(idx,statemat,norm=TRUE) {
   statemat = statemat[idx,]
   jointent=entropy( statemat )
   entvec = c()
   for (cl in 1:ncol(statemat)) {
      condvar=!( 1:ncol(statemat) %in% cl )
      selfent = entropy( statemat[,cl] )
      neighent = entropy( statemat[,condvar] )
      totent = selfent + neighent
      entvec[cl] = totent - jointent
      if (norm) { if (selfent == 0 ) {entvec[cl] ==0 } else {entvec[cl] = entvec[cl]/selfent} }
   }
   return(entvec)
}

