#
# Rule 03
#

rule.3 <- function(pag, verbose=FALSE) {
    ind <- which((pag != 0 & t(pag) == 1), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        b <- ind[i, 1]
        d <- ind[i, 2]
        indAC <- which((pag[b, ] != 0 & pag[, b] ==
                            2) & (pag[, d] == 1 & pag[d, ] != 0))
        if (length(indAC) >= 2) {
            counter <- 0
            while ((counter < (length(indAC) - 1)) &&
                   (pag[d, b] != 2)) {
                counter <- counter + 1
                ii <- counter
                while (ii < length(indAC) && pag[d, b] != 2) {
                    ii <- ii + 1
                    if (pag[indAC[counter], indAC[ii]] == 0 && pag[indAC[ii], indAC[counter]] == 0) {
                        if (verbose) {
                            cat("\nRule 3", "\n")
                            cat("Orient:", d, "*->", b, "\n")
                        }
                        pag[d, b] <- 2
                    }
                }
            }
        }
    }
    return(pag)
}