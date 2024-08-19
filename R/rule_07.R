#
# Rule 07
#

# library(pcalg)
# library(graph)

rule.7 <- function(pag, unfVect=NULL, verbose=FALSE) {
    ind <- which((pag != 0 & t(pag) == 1), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        b <- ind[i, 1]
        c <- ind[i, 2]
        indA <- which((pag[b, ] == 3 & pag[, b] ==
                           1) & (pag[c, ] == 0 & pag[, c] == 0))
        indA <- setdiff(indA, c)
        if (length(indA) > 0) {
            if (length(unfVect) == 0) {
                pag[c, b] <- 3
                if (verbose)
                    cat("\nRule 7", "\nOrient:", indA, "-o",
                        b, "o-*", c, "as", b, "-*", c, "\n")
            }
            else for (a in indA) if (!any(unfVect ==
                                          triple2numb(p, a, b, c), na.rm = TRUE) &&
                                     !any(unfVect == triple2numb(p, c, b, a),
                                          na.rm = TRUE)) {
                pag[c, b] <- 3
                if (verbose)
                    cat("\nRule 7", "\nConservatively orient:",
                        a, "-o", b, "o-*", c, "as", b, "-*",
                        c, "\n")
            }
        }
    }
    return(pag)
}
