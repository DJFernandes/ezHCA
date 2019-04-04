#' Calculate home cage entropy
#'
#' Calculate the home cage entropy for a subset of the state matrix
#' @param idx rows of the state matrix for which to compute entropy
#' @param statemat state matrix where each individual is a column and elements are observed states
#' @param norm Should the overall result be normalized to the total number of states (6)
#' @return entropy for each individual in nats
#' @keywords internal
#' @import infotheo
#' @examples
#' \dontrun{
#'   test = homeCageEntropy(idx,statemat)
#' }

homeCageEntropy = function(idx,statemat,norm=TRUE) {
   if (is.null(dim(statemat))) { retvec=entropy(statemat[idx]) 
   } else {                      retvec=apply(statemat[idx,],2,entropy)  }
   if (norm) { retvec=retvec/log(6) }
   return(retvec)
}

