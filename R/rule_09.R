#
# Rule 09
#

rule.9 <- function(pag, verbose=FALSE) {
    p <- as.numeric(dim(pag)[1])
    ind <- which((pag == 2 & t(pag) == 1), arr.ind = TRUE)
    while (length(ind) > 0) {
        a <- ind[1, 1]
        c <- ind[1, 2]
        ind <- ind[-1, , drop = FALSE]
        indB <- which((pag[a, ] == 2 | pag[a, ] == 1) & (pag[, a] == 1 | pag[, a] == 3) & (pag[c, ] == 0 & pag[, c] == 0))
        indB <- setdiff(indB, c)
        while ((length(indB) > 0) && (pag[c, a] == 1)) {
            b <- indB[1]
            indB <- indB[-1]
            upd <- pcalg:::minUncovPdPath(p, pag, a, b, c, unfVect = NULL, verbose = FALSE)
            if (length(upd) > 1) {
                pag[c, a] <- 3
                if (verbose)
                    cat("\nRule 9", "\nThere exists an uncovered potentially directed path between",
                        a, "and", c, ". Orient:", a, " ->", c, "\n")
            }
        }
    }
    return(pag)
}
