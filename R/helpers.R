# library(pcalg)
# library(graph)



directed.path.exists <- function(pag, source, destination, avoid=c(),
                                 allow.almost.directed=TRUE, verbose=FALSE) {
    
    # Find all nodes d such that source --> d or source <-> d
    indD <- which(pag[source, ] == 2 & pag[, source] != 1)
    
    while(length(indD) > 0) {
        next.allow.almost.directed <- allow.almost.directed
        
        d <- indD[1]
        d.idx <- indD[[1]]
        indD <- indD[-1]
        
        # If source <-> d and we don't want almost directed paths, skip
        # Otherwise, don't allow almost directed paths moving forward
        almost.directed <- pag[d, source] == 2
        if (!allow.almost.directed & almost.directed) {
            next
        }
        else if (almost.directed) {
            next.allow.almost.directed <- FALSE
        }
        
        # Did we reach destination?
        if (d.idx == destination) {
            return(TRUE)
        }
        
        # We've already seen this node. So skip
        if (d.idx %in% avoid) {
            next
        }
        
        # Else recursively search
        if (directed.path.exists(pag, d.idx, destination, c(avoid, d.idx),
                                 next.allow.almost.directed,
                                 verbose)) {
            return(TRUE)
        }
        
    }
    
    # We did not find such a path
    return(FALSE)
}


uncovered.possible.descendants <- function(pag, x, verbose=FALSE) {
    
    p <- nrow(pag)
    # Vector containing truth values for possible uncovered descendents
    is.de <- rep.int(FALSE, p)
    is.de[x] <- TRUE # x is its own descendent
    
    # Find all nodes d such that x o-o d or x o-> d
    indD <- which(pag[x, ] != 0 & pag[, x] == 1 & !is.de)
    # Predecessors of d in the path from x to d.
    # Initially this is just x for all d
    i.pr <- rep(x, length(indD))
    paths <- list()
    num_paths <- length(paths)
    
    while (length(indD) > 0) {
        # Get d, mark it as a descendant, and get its predecessor
        # So pred o-o d or pred o-> d
        d <- indD[1]
        is.de[d] <- TRUE
        pred <- i.pr[1]
        
        # Store the actual paths
        num_paths <- num_paths + 1
        if (pred != x) {
            for (i in seq_len(length(paths))) {
                path.i <- paths[[i]]
                if (path.i[length(path.i)] == pred) {
                    path <- path.i
                    break
                }
            }
        }
        else {
            path <- c(x)
        }
        paths[[num_paths]] <- c(path, indD[[1]])
        
        # Update descendant and predecessor lists
        indD <- indD[-1]
        i.pr <- i.pr[-1]
        
        a.d <- pag[, d]
        a.d.p <- a.d[pred]
        # Find all nodes r such that d o-o r or d o-> r
        indR <- which(pag[d, ] != 0 & pag[, d] == 1 & !is.de)
        for (j in seq_along(indR)) {
            # Ensure that pred and r are not connected i.e., uncovered triplet
            r <- indR[j]
            if (pag[pred, r] == 0) {
                indD <- c(indD, r)
                i.pr <- c(i.pr, d)
            }
        }
    }
    return(paths)
}

rule_13_paths <- function(pag, C, D, verbose=FALSE) {
    
    node_names <- colnames(pag)
    
    # V_pag <- seq(nrow(pag))
    # colnames(pag) <- rownames(pag) <- V_pag
    
    for (i in seq_len(length(node_names))) {
        if (node_names[i] == C) {
            x <- i
        }
        else if (node_names[i] == D) {
            y <- i
        }
    }
    
    p <- nrow(pag)
    # Vector containing truth values for possible uncovered descendents
    is.de <- rep.int(FALSE, p)
    is.de[x] <- TRUE # x is its own descendent
    
    # Find all nodes d such that x o-o d or x o-> d
    indD <- which(pag[x, ] == 1 & pag[, x] == 2 & !is.de)
    
    # Predecessors of d in the path from x to d.
    # Initially this is just x for all d
    i.pr <- rep(x, length(indD))
    
    while (length(indD) > 0) {
        # Get d, mark it as a descendant, and get its predecessor
        # So pred o-o d or pred o-> d
        d <- indD[1]
        is.de[d] <- TRUE
        pred <- i.pr[1]
        
        
        # Update descendant and predecessor lists
        indD <- indD[-1]
        i.pr <- i.pr[-1]
        
        a.d <- pag[, d]
        a.d.p <- a.d[pred]
        # Find all nodes r such that d o-o r or d o-> r
        indR <- which(pag[d, ] != 0 & pag[, d] == 1 & !is.de)
        for (j in seq_along(indR)) {
            # Ensure that pred and r are not connected i.e., uncovered triplet
            r <- indR[j]
            
            if (r == y) {
                if (pag[d, r] == 2 && pag[r, d] == 1) {
                    return(TRUE)
                }
            }
            if (pag[pred, r] == 0) {
                indD <- c(indD, r)
                i.pr <- c(i.pr, d)
            }
        }
    }
    return(FALSE)
}



rule_13_paths_old <- function(pag, C, D, verbose=FALSE) {
    
    p <- nrow(pag)
    # Vector containing truth values for possible uncovered descendents
    # is.de <- rep.int(FALSE, p)
    # is.de[x] <- TRUE # x is its own descendent
    
    # Find all nodes V1 such that C <-o V1
    ind_V1 <- which(pag[C, ] == 1 & pag[, C] == 2, arr.ind=T)
    
    for (i in seq_len(length(ind_V1))) {
        V1 <- ind_V1[i]
        
        uncovered_paths_from_V1 <- uncovered.possible.descendants(pag, V1, verbose=FALSE)
        
    }
    
    
    
    # return(paths)
}