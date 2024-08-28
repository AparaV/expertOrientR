#
# Rule 11
#

rule.11 <- function(pag, verbose=FALSE) {
    # Rule 11 general
    # Find: a *--* b, a *--* c, a o--* d
    #       b *--> c
    #       c ---> d
    # Turn: a ---> d
    
    # We should not have an arrowhead at a
    # indAB <- which(pag != 2 & t(pag) != 0, arr.ind = TRUE)
    indAB <- which(pag != 0, arr.ind = TRUE)
    for (i in seq_len(nrow(indAB))) {
        a <- indAB[i, 1]
        b <- indAB[i, 2]
        
        # We have: a o--> b, a ---> b, or a o--o b
        # Next we look at a *--* c
        nbrA.any2any <- which(pag[a, ] != 0 & pag[, a] != 0, arr.ind = TRUE)
        if (length(nbrA.any2any) < 2) {
            next
        }
        for (j in seq_len(length(nbrA.any2any))) {
            # We have a *--* c
            c <- nbrA.any2any[j]
            # If not: b *--> c, we move on
            # This ensures that b != c
            if (pag[b, c] != 2) {
                next
            }
            for (k in seq_len(length(nbrA.any2any))) {
                d <- nbrA.any2any[k]
                # Ensure a o--> d or a o--o d
                if (pag[d, a] != 1) {
                    next
                }
                # Ensure c --> d
                # This ensures c != d and b != d (if b == d, pag[b, c] = 2)
                if (pag[c, d] == 2 && pag[d, c] == 3 && pag[b, d] == 0) {
                    # Orient a ---> d
                    pag[a, d] = 2
                    pag[d, a] = 3
                    if (verbose) {
                        cat("\nRule 11", "\n")
                        cat("Orient:", a, "-->", d, "\n")
                    }
                }
            }
        }
    }
    return(pag)
}


check.11.orientation <- function(pag) {
    output <- data.frame(matrix(ncol=4, nrow=0))
    colnames(output) <- c("A", "B", "C", "D")
    indAB <- which(pag == 1 & t(pag) == 1, arr.ind = TRUE)
    for (i in seq_len(nrow(indAB))) {
        a <- indAB[i, 1]
        b <- indAB[i, 2]
        nbrA.circle <- which(pag[a, ] == 1 & pag[, a] == 1, arr.ind = TRUE)
        if (length(nbrA.circle) < 2) {
            next
        }
        for (j in seq_len(length(nbrA.circle))) {
            # We have a o--o c
            c <- nbrA.circle[j]
            # If not: b o--o c, we move on
            # This ensures that b != c
            if (pag[b, c] != 1 || pag[c, b] != 1) {
                next
            }
            for (k in seq_len(length(nbrA.circle))) {
                # We have a o--o d
                d <- nbrA.circle[k]
                if (b == d) {
                    next
                }
                # Ensure c --> d or c o--o d
                # This ensures c != d
                if (pag[c, d] == 2 && pag[d, c] == 3 && pag[b, d] == 0) {
                    output <- output %>% add_row(A=a, B=b, C=c, D=d)
                }
                else if (pag[c, d] == 1 && pag[d, c] == 1 && pag[b, d] == 0) {
                    output <- output %>% add_row(A=a, B=b, C=c, D=d)
                }
            }
        }
    }
    return(output)
}