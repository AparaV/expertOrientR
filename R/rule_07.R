#
# Rule 07
#

rule.7 <- function(pag, unfVect=NULL, verbose=FALSE) {
    ind <- which((pag != 0 & t(pag) == 1), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        b <- ind[i, 1]
        c <- ind[i, 2]
        indA <- which((pag[b, ] == 3 & pag[, b] ==
                           1) & (pag[c, ] == 0 & pag[, c] == 0))
        indA <- setdiff(indA, c)
        if (length(indA) > 0) {
            pag[c, b] <- 3
            if (verbose) {
                cat("\nRule 7", "\nOrient:", indA, "-o", b, "o-*", c, "as", b, "-*", c, "\n")
            }
        }
    }
    return(pag)
}
