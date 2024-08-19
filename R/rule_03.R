#
# Rule 03
#

# library(pcalg)
# library(graph)

rule.3 <- function(pag, unfVect=NULL, verbose=FALSE) {
    ind <- which((pag != 0 & t(pag) == 1), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        b <- ind[i, 1]
        d <- ind[i, 2]
        indAC <- which((pag[b, ] != 0 & pag[, b] ==
                            2) & (pag[, d] == 1 & pag[d, ] != 0))
        if (length(indAC) >= 2) {
            if (length(unfVect) == 0) {
                counter <- 0
                while ((counter < (length(indAC) - 1)) &&
                       (pag[d, b] != 2)) {
                    counter <- counter + 1
                    ii <- counter
                    while (ii < length(indAC) && pag[d, b] !=
                           2) {
                        ii <- ii + 1
                        if (pag[indAC[counter], indAC[ii]] ==
                            0 && pag[indAC[ii], indAC[counter]] ==
                            0) {
                            if (verbose) {
                                cat("\nRule 3", "\n")
                                cat("Orient:", d, "*->", b, "\n")
                            }
                            pag[d, b] <- 2
                        }
                    }
                }
            }
            # else {
            #     comb.indAC <- combn(indAC, 2)
            #     for (j in seq_len(dim(comb.indAC)[2])) {
            #         a <- comb.indAC[1, j]
            #         c <- comb.indAC[2, j]
            #         if (pag[a, c] == 0 && pag[c, a] == 0 &&
            #             c != a) {
            #             if (!any(unfVect == triple2numb(p,
            #                                             a, d, c), na.rm = TRUE) && !any(unfVect ==
            #                                                                             triple2numb(p, c, d, a), na.rm = TRUE)) {
            #                 pag[d, b] <- 2
            #                 if (verbose) {
            #                     cat("\nRule 3", "\n")
            #                     cat("Conservatively orient:", d,
            #                         "*->", b, "\n")
            #                 }
            #             }
            #         }
            #     }
            # }
        }
    }
    return(pag)
}