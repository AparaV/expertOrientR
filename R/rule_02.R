#
# Rule 2
#

rule.2 <- function(pag, verbose=FALSE) {
    ind <- which((pag == 1 & t(pag) != 0), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        a <- ind[i, 1]
        c <- ind[i, 2]
        indB <- which((pag[a, ] == 2 & pag[, a] ==
                           3 & pag[c, ] != 0 & pag[, c] == 2) | (pag[a,
                           ] == 2 & pag[, a] != 0 & pag[c, ] == 3 &
                               pag[, c] == 2))
        if (length(indB) > 0) {
            pag[a, c] <- 2
            if (verbose) {
                cat("\nRule 2", "\n")
                cat("Orient:", a, "->", indB, "*->", c,
                    "or", a, "*->", indB, "->", c, "with",
                    a, "*-o", c, "as:", a, "*->", c, "\n")
            }
        }
    }
    return(pag)
}