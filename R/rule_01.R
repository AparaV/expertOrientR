#
# Rule 1
#

rule.1 <- function(pag, verbose=FALSE) {
    ind <- which((pag == 2 & t(pag) != 0), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        a <- ind[i, 1]
        b <- ind[i, 2]
        indC <- which((pag[b, ] != 0 & pag[, b] ==
                           1) & (pag[a, ] == 0 & pag[, a] == 0))
        indC <- setdiff(indC, a)
        if (length(indC) > 0) {
            if (any(pag[b, indC] == 3) && verbose) {
                cat("Contradiction in Rule 1!\n")
            }
            pag[b, indC] <- 2
            pag[indC, b] <- 3
            if (verbose) {
                cat("\nRule 1", "\nOrient:", a, "*->", b, "o-*", indC,
                    "as:", b, "->", indC, "\n")
            }
        }
    }
    return(pag)
}