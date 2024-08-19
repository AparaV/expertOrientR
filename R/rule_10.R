#
# Rule 10
#

# library(pcalg)
# library(graph)


rule.10 <- function(pag, unfVect=NULL, verbose=FALSE) {
    p <- as.numeric(dim(pag)[1])
    
    # Find all o-> edges in pag
    ind <- which((pag == 2 & t(pag) == 1), arr.ind = TRUE)
    # If at least one o-> in pag
    while (length(ind) > 0) {
        # Here a o-> c. a is alpha and c is gamma
        a <- ind[1, 1]
        c <- ind[1, 2]
        # Update list of indices
        ind <- ind[-1, , drop = FALSE]
        
        # Find all edges b --> c
        indB <- which((pag[c, ] == 3 & pag[, c] == 2))
        # There needs to be at least two edges --> c for a v-structure
        if (length(indB) >= 2) {
            counterB <- 0
            
            # Make sure we still have a o-* c
            while (counterB < length(indB) && (pag[c,a] == 1)) {
                counterB <- counterB + 1
                
                # Here b -> c
                b <- indB[counterB]
                # Find all d such that b -> c <- d
                indD <- setdiff(indB, b)
                counterD <- 0
                
                # Make sure we still have a o-* c
                while ((counterD < length(indD)) && (pag[c,a] == 1)) {
                    counterD <- counterD + 1
                    d <- indD[counterD]
                    
                    # First, easy case:
                    # Check for a potentially directed edge from a to b and a to c
                    # Ex: b <-o a o-o c or b o-o a -> c
                    # Also make sure b and d are not connected
                    if ((pag[a, b] == 1 || pag[a, b] == 2) &&
                        (pag[b, a] == 1 || pag[b, a] == 3) &&
                        (pag[a, d] == 1 || pag[a, d] == 2) &&
                        (pag[d, a] == 1 || pag[d, a] == 3) &&
                        pag[d, b] == 0 && pag[b, d] == 0) {
                        if (length(unfVect) == 0) {
                            # Orient a --> c
                            pag[c, a] <- 3
                            if (verbose)
                                cat("\nRule 10 [easy]", "\nOrient:",
                                    a, "->", c, "\n")
                        }
                        # else if (!any(unfVect == triple2numb(p,
                        #                                      b, a, d), na.rm = TRUE) && !any(unfVect ==
                        #                                                                      triple2numb(p, d, a, b), na.rm = TRUE)) {
                        #     pag[c, a] <- 3
                        #     if (verbose)
                        #         cat("\nRule 10 [easy]", "\nConservatively orient:",
                        #             a, "->", c, "\n")
                        # }
                    }
                    
                    # Next, hard case:
                    # Look for longer p.d. paths from a to b and a to d
                    else {
                        
                        # Find all X s.t. there is a p.d. edge from a to X
                        indX <- which((pag[a, ] == 1 | pag[a, ] == 2) &
                                          (pag[, a] == 1 | pag[, a] == 3),
                                      arr.ind = TRUE)
                        indX <- setdiff(indX, c)
                        
                        # Any p.d. path from a to b or a to d needs to have X
                        # We need two distinct such X (TODO: Verify)
                        if (length(indX >= 2)) {
                            counterX1 <- 0
                            while (counterX1 < length(indX) && pag[c, a] == 1) {
                                counterX1 <- counterX1 + 1
                                
                                # First X will be the mu or omega
                                first.pos <- indX[counterX1]
                                indX2 <- setdiff(indX, first.pos) 
                                counterX2 <- 0
                                while (counterX2 < length(indX2) &&  pag[c, a] == 1) {
                                    
                                    # Second X will be the mu or omega
                                    counterX2 <- counterX2 + 1
                                    sec.pos <- indX2[counterX2]
                                    
                                    # Check for a uncovered p.d. path from a to b
                                    # This must pass through first.pos
                                    if (first.pos == b) {
                                        t1 <- c(a,b)
                                    } else {
                                        t1 <- pcalg:::minUncovPdPath(
                                            p, pag, a, first.pos, b, unfVect = unfVect,
                                            verbose = verbose)
                                    }
                                    
                                    if (length(t1) > 1) {     
                                        
                                        # Check for a uncovered p.d. path from a to d
                                        # This must pass through sec.pos
                                        if (sec.pos == d) {
                                            t2 <- c(a,d)
                                        }
                                        else {
                                            t2 <- pcalg:::minUncovPdPath(p, pag, a, sec.pos, d, unfVect = unfVect,
                                                                         verbose = verbose)
                                        }
                                        
                                        # Make sure first.pos and sec.pos are not connected
                                        # Then orient a --> c
                                        if (length(t2) > 1 && first.pos !=sec.pos &&
                                            pag[first.pos,sec.pos] == 0) {
                                            if (length(unfVect) == 0) {
                                                pag[c, a] <- 3
                                                if (verbose)
                                                    cat("\nRule 10", "\nOrient:",
                                                        a, "->", c, "\n") 
                                            } 
                                            # else if (!any(unfVect == triple2numb(p, first.pos, a, sec.pos), na.rm = TRUE) &&
                                            #          !any(unfVect == triple2numb(p, sec.pos, a, first.pos), na.rm = TRUE)) {    
                                            #     pag[c, a] <- 3
                                            #     if (verbose)
                                            #         cat("\nRule 10", "\nConservatively orient:",
                                            #             a, "->", c, "\n")
                                            # }
                                        } 
                                    } 
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return(pag)
}

