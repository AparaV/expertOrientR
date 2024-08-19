# require(pcalg)

#' @export
generate_mag_pag <- function(size, prob, L_fraction, seed=NA) {
    
    if (!is.na(seed)) {
        set.seed(seed)
    }
    
    # Generate a topologically ordered random DAG
    d <- pcalg::randomDAG(size, prob, lB=1, uB=1)
    d.amat <- as(d, "matrix")
    
    # Choose latent variables
    V <- d@nodes
    V.source <- names(which(colSums(d.amat) == 0))
    V.confounders <- names(which(rowSums(d.amat) == 2))
    L.candidates <- union(V.source, V.confounders)
    L.size <- as.integer(max(1, L_fraction * length(L.candidates)))
    if (L.size == length(L.candidates)) {
        # next
        L <- L.candidates
        # print(L.candidates)
        # print(L.size)
    }
    L <- sample(L.candidates, L.size)
    
    # Get MAG and PAG
    m <- edag2mag(d, V, L)
    p <- edag2pag(d, V, L)
    mag <- m@amat
    pag <- p@amat
    
    num_nodes <- nrow(mag)
    rownames(mag) <- colnames(mag) <- c(1:num_nodes)
    rownames(pag) <- colnames(pag) <- c(1:num_nodes)
    
    return(list("mag"=mag, "pag"=pag))
}






## dag 2 pag function 
## need g - graphnel object (your dag)
## g MUST be TOPOLOGICALLY ORDERED (that is no edges j -> i, where label j comes after  label i in V )
## V - list of names of variables in the dag
## L - list of names of latent variables to marginalize out
edag2pag <- function(g,V,L,verbose=FALSE)
{
  covar <- pcalg::trueCov(g)
  true.corr1 <- stats::cov2cor(covar)
  suffStat1 <- list(C = true.corr1, n = 10^9)
  
  indepTest1 <- pcalg::gaussCItest
  amat.dag <- as(g,"matrix")
  n <- nrow(amat.dag)
  rownames(amat.dag) <- colnames(amat.dag) <- c(1:n)
  
  L1 <- which(V%in%L)
  # ?dag2pag
  newpag <- pcalg::dag2pag(suffStat=suffStat1, indepTest=indepTest1, graph=g,L=L1,alpha = 0.9999,verbose=verbose)
  amat <- newpag@amat
  colnames(amat) <- rownames(amat) <- setdiff(V,L)
  #names.new <- setdiff(V,L)
  return(gplus1 <- new("fciAlgo", amat=amat))
  
  
}


## DAG to MAG function
## need g - graphnel object (your dag)
## g MUST be TOPOLOGICALLY ORDERED (that is no edges j -> i, where label j comes after  label i in V )
## V - list of names of variables in the dag
## L - list of names of latent variables to marginalize out
edag2mag <- function(g,V,L)
{
  covar <- pcalg::trueCov(g)
  true.corr1 <- stats::cov2cor(covar)
  suffStat1 <- list(C = true.corr1, n = 10^9)
  
  indepTest1 <- pcalg::gaussCItest
  amat.dag <- as(g,"matrix")
  n <- nrow(amat.dag)
  rownames(amat.dag) <- colnames(amat.dag) <- c(1:n)
  
  L1 <- which(V%in%L)
  
  newpag <- pcalg::dag2pag(suffStat=suffStat1, indepTest=indepTest1, graph=g,L=L1,alpha = 0.9999)
  amat <- newpag@amat
  
  names.new <- setdiff(V,L)
  
  ## find directions
  for ( i in 1: nrow(amat)){
    for (j in 1:nrow(amat)){
      if (amat[j,i]==1){
        old.name.i <- which(V == names.new[i])
        old.name.j <- which(V == names.new[j])
        if (old.name.i %in% pcalg::possDe(amat.dag,old.name.j,type="dag")){
          amat[j,i] <- 3
        }else{
          amat[j,i] <- 2
        }
      }
    }
  }
  colnames(amat) <- rownames(amat) <- setdiff(V,L)
  return(gplus1 <- new("fciAlgo", amat=amat))
}

