#
# Rule 05
#

# library(pcalg)
# library(graph)


rule.5 <- function(pag, unfVect=NULL, verbose=FALSE) {
    p <- as.numeric(dim(pag)[1])
    ind <- which((pag == 1 & t(pag) == 1), arr.ind = TRUE)
    while (length(ind) > 0) {
        a <- ind[1, 1]
        b <- ind[1, 2]
        ind <- ind[-1, , drop = FALSE]
        indC <- which((pag[a, ] == 1 & pag[, a] ==
                           1) & (pag[b, ] == 0 & pag[, b] == 0))
        indC <- setdiff(indC, b)
        indD <- which((pag[b, ] == 1 & pag[, b] ==
                           1) & (pag[a, ] == 0 & pag[, a] == 0))
        indD <- setdiff(indD, a)
        if (length(indC) > 0 && length(indD) > 0) {
            counterC <- 0
            while ((counterC < length(indC)) && pag[a,
                                                    b] == 1) {
                counterC <- counterC + 1
                c <- indC[counterC]
                counterD <- 0
                while ((counterD < length(indD)) && pag[a,
                                                        b] == 1) {
                    counterD <- counterD + 1
                    d <- indD[counterD]
                    if (pag[c, d] == 1 && pag[d, c] == 1) {
                        if (length(unfVect) == 0) {
                            pag[a, b] <- pag[b, a] <- 3
                            pag[a, c] <- pag[c, a] <- 3
                            pag[c, d] <- pag[d, c] <- 3
                            pag[d, b] <- pag[b, d] <- 3
                            if (verbose)
                                cat("\nRule 5", "\nThere exists an uncovered circle path between",
                                    a, "and", b, ". Orient:", a,
                                    "-", b, "and", a, "-", c, "-",
                                    d, "-", b, "\n")
                        }
                        # else {
                        #     path2check <- c(a, c, d, b)
                        #     if (faith.check(path2check, unfVect,
                        #                     p)) {
                        #         pag[a, b] <- pag[b, a] <- 3
                        #         pag[a, c] <- pag[c, a] <- 3
                        #         pag[c, d] <- pag[d, c] <- 3
                        #         pag[d, b] <- pag[b, d] <- 3
                        #         if (verbose)
                        #             cat("\nRule 5", "\nThere exists a faithful uncovered circle path between",
                        #                 a, "and", b, ". Conservatively orient:",
                        #                 a, "-", b, "and", a, "-", c,
                        #                 "-", d, "-", b, "\n")
                        #     }
                        # }
                    }
                    else {
                        ucp <- pcalg:::minUncovCircPath(p, pag = pag,
                                                        path = c(a, c, d, b), unfVect = unfVect,
                                                        verbose = verbose)
                        if (length(ucp) > 1) {
                            n <- length(ucp)
                            pag[ucp[1], ucp[n]] <- pag[ucp[n],
                                                       ucp[1]] <- 3
                            for (j in seq_len((length(ucp) -
                                               1))) pag[ucp[j], ucp[j + 1]] <- pag[ucp[j +
                                                                                           1], ucp[j]] <- 3
                        }
                    }
                }
            }
        }
    }
    return(pag)
}
