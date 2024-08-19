#
# Rule 04
#

# library(pcalg)
library(graph)


rule.4.zhang <- function(pag, sepset, verbose=FALSE) {
    ind <- which((pag != 0 & t(pag) == 1), arr.ind = TRUE)
    while (length(ind) > 0) {
        b <- ind[1, 1]
        c <- ind[1, 2]
        ind <- ind[-1, , drop = FALSE]
        indA <- which((pag[b, ] == 2 & pag[, b] !=
                           0) & (pag[c, ] == 3 & pag[, c] == 2))
        while (length(indA) > 0 && pag[c, b] == 1) {
            a <- indA[1]
            indA <- indA[-1]
            Done <- FALSE
            while (!Done && pag[a, b] != 0 && pag[a,
                                                  c] != 0 && pag[b, c] != 0) {
                md.path <- pcalg:::minDiscrPath(pag, a, b, c, verbose = verbose)
                if ((N.md <- length(md.path)) == 1) {
                    Done <- TRUE
                }
                else {
                    if ((b %in% sepset[[md.path[1]]][[md.path[N.md]]]) ||
                        (b %in% sepset[[md.path[N.md]]][[md.path[1]]])) {
                        if (verbose)
                            cat("\nRule 4", "\nThere is a discriminating path between",
                                md.path[1], "and", c, "for", b,
                                ",and", b, "is in Sepset of", c,
                                "and", md.path[1], ". Orient:",
                                b, "->", c, "\n")
                        pag[b, c] <- 2
                        pag[c, b] <- 3
                    }
                    else {
                        if (verbose)
                            cat("\nRule 4", "\nThere is a discriminating path between",
                                md.path[1], "and", c, "for", b,
                                ",and", b, "is not in Sepset of",
                                c, "and", md.path[1], ". Orient:",
                                a, "<->", b, "<->", c, "\n")
                        pag[b, c] <- pag[c, b] <- 2
                        if (pag[a, b] == 3) {
                            if (verbose)
                                cat("\nContradiction in Rule 4b!\n")
                            if (jci == "0") {
                                pag[a, b] <- 2
                            }
                        }
                        else {
                            pag[a, b] <- 2
                        }
                    }
                    Done <- TRUE
                }
            }
        }
    }
    return(pag)
}
