#' Calculate transfer entropy
#'
#' Calculate the transfer entropy for a subset of the state matrix
#' @param idx rows of the state matrix for which to compute mutual information
#' @param statemat state matrix where each individual is a column and elements are observed states
#' @param norm Should the overall result be normalized to the total entropy
#' @return transfer entropy for each pairwise-individuals in nats
#' @keywords internal
#' @import infotheo
#' @examples
#' \dontrun{
#'   test = TransEnt(idx,statemat)
#' }

TransEnt = function(idx,statemat,norm=TRUE) {

   statemat = statemat[idx,]

   lag_statemat = statemat[,grep('_lxd',colnames(statemat))]
   colnames(lag_statemat) = gsub("_lxd","",colnames(lag_statemat))
   statemat = statemat[,grep('_xd',colnames(statemat))]
   colnames(statemat) = gsub("_xd","",colnames(statemat))

   # Order them properly just incase
   lag_statemat=lag_statemat[,colnames(statemat)]

   LENiY = ncol(statemat)
   LENiX = LENiY

   TEvec=c() ; namevec = c() ; count = 0
   for (iY in 1:LENiY) { for (iX in 1:LENiX) { iZ = which( (1:LENiY) != iX )
      
      if (iY == iX) {next}
      count = count + 1

      Y = statemat[,iY]  ; Xl = lag_statemat[ , iX ] ; Zl = lag_statemat[ , iZ ]
      Yl = lag_statemat[,iY]

   C1 = entropy( cbind(Y,Yl,Zl) )
   C2 = entropy( cbind(Yl,Zl) )
   C3 = entropy( cbind(Y,Yl,Xl,Zl) )
   C4 = entropy( cbind(Yl,Xl,Zl) )
   
   TEvec[count] = ( C1 - C2 - C3 + C4 ) 
   if (norm) { 
        selfent = entropy(Y)
        if (selfent == 0 ) {TEvec[count] == 0 } else {TEvec[count] = TEvec[count]/selfent} }
    namevec[count] = paste( colnames(statemat)[c(iY,iX)] , collapse = '_by_' )
   }}
   names(TEvec) = namevec

   return(TEvec)
}

