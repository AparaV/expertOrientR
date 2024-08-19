#
# Rule 08
#

# library(pcalg)
# library(graph)

rule.8 <- function(pag, unfVect=NULL, verbose=FALSE) {
    ind <- which((pag == 2 & t(pag) == 1), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        a <- ind[i, 1]
        c <- ind[i, 2]
        indB <- which(pag[, a] == 3 & (pag[a, ] ==
                                           2 | pag[a, ] == 1) & pag[c, ] == 3 & pag[,
                                                                                    c] == 2)
        if (length(indB) > 0) {
            pag[c, a] <- 3
            if (verbose)
                cat("\nRule 8", "\nOrient:", a, "->", indB,
                    "->", c, "or", a, "-o", indB, "->", c,
                    "with", a, "o->", c, "as", a, "->", c,
                    "\n")
        }
    }
    return(pag)
}
