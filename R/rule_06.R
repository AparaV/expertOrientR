#
# Rule 06
#

# library(pcalg)
# library(graph)


rule.6 <- function(pag, unfVect=NULL, verbose=FALSE) {
    ind <- which((pag != 0 & t(pag) == 1), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        b <- ind[i, 1]
        c <- ind[i, 2]
        if (any(pag[b, ] == 3 & pag[, b] == 3)) {
            pag[c, b] <- 3
            if (verbose)
                cat("\nRule 6", "\nOrient:", b, "o-*",
                    c, "as", b, "-*", c, "\n")
        }
    }
    return(pag)
}