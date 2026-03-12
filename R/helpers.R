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



# ==============================================================================
# Helper functions for Rule 14
# ==============================================================================

updateList <- function(path, set, old.list) {
    ## Purpose: update the list of all paths in the iterative functions
    ## ----------------------------------------------------------------------
    ## Arguments: - path: the path under investigation
    ##            - set: (integer) index set of variables to be added to path
    ##            - old.list: the list to update
    ## ----------------------------------------------------------------------
    c(old.list, lapply(set, function(s) c(path, s)))
}


find_possibleancestor <- function(amat, b) {
    ## Purpose: Find all possible ancestors of vertex b
    ## A possible ancestor of b is any vertex d such that there exists
    ## a path d o-* ... o-* b where each edge is not into d
    ## ----------------------------------------------------------------------
    ## Arguments: - amat: adjacency matrix (PAG)
    ##            - b: vertex index
    ## ----------------------------------------------------------------------
    new_D <- c()
    visited <- rep(FALSE, ncol(amat))
    visited[b] <- TRUE
    indD <- which(amat[, b] != 0 & amat[, b] != 3 & amat[b, ] != 2 & !visited)
    
    if (length(indD) > 0) {
        path.list <- updateList(b, indD, NULL)
        while (length(path.list) > 0) {
            mpath <- path.list[[1]]
            m <- length(mpath)
            d <- mpath[m]
            pred <- mpath[m - 1]
            path.list[[1]] <- NULL
            
            visited[d] <- TRUE
            indR <- which(amat[pred, ] == 0 & amat[d, ] != 0 & 
                         amat[d, ] != 2 & amat[, d] != 3 & !visited)
            if (length(indR) > 0) {
                path.list <- updateList(mpath[-1], indR, path.list)
            }
        }
    }
    return(which(visited))
}


detect_order_1 <- function(a, b, remain_set, amat_order) {
    ## Purpose: Detect which vertices are "prior to" b with order 1
    ## A vertex is prior to b if it satisfies one of three conditions
    ## ----------------------------------------------------------------------
    ## Arguments: - a: reference vertex
    ##            - b: target vertex
    ##            - remain_set: candidate vertices to check
    ##            - amat_order: adjacency matrix (PAG)
    ## ----------------------------------------------------------------------
    if (length(remain_set) == 0)
        return(c())
    
    # Condition 1: vertices with directed edge into b
    a1 <- remain_set[which(amat_order[remain_set, b] == 2 & 
                          amat_order[b, remain_set] == 3)]
    
    # Condition 2: vertices with uncovered possibly directed path to spouse of b
    a2 <- c()
    indC <- which((amat_order[b, ] == 3 & amat_order[, b] == 2))
    for (C in indC) {
        if (amat_order[a, C] == 0) {
            indb <- intersect(remain_set, which(amat_order[, a] == 1 & 
                                               amat_order[a, ] != 3))
            indb <- setdiff(indb, union(a1, a2))
            if (length(indb) > 0) {
                for (B in indb) {
                    upd <- minUncovPdPath(ncol(amat_order), amat_order, a, B, C,
                                        unfVect = NULL, verbose = FALSE)
                    if (length(upd) > 1)
                        a2 <- union(a2, B)
                }
            }
        }
    }
    
    # Condition 3: vertices with unbridged path structure
    a3 <- c()
    poss_a3 <- setdiff(which(amat_order[, a] == 1 & amat_order[a, ] != 3 & 
                            amat_order[, b] == 2 & amat_order[b, ] == 1), 
                      union(a1, a2))
    poss_S1 <- which(amat_order[, a] == 2 & amat_order[, b] == 2 & 
                    amat_order[b, ] == 2)
    S_A <- poss_S1
    p <- ncol(amat_order)
    
    if (length(poss_a3) != 0 & length(poss_S1) != 0) {
        for (D in poss_a3) {
            visited <- rep(FALSE, ncol(amat_order))
            path.list <- updateList(a, D, NULL)
            while (length(path.list) > 0) {
                mpath <- path.list[[1]]
                m <- length(mpath)
                d <- mpath[m]
                pred <- mpath[m - 1]
                path.list[[1]] <- NULL
                
                visited[c(a, d)] <- TRUE
                indR <- which(amat_order[pred, ] == 0 & amat_order[d, ] != 0 & 
                             amat_order[d, ] != 3 & amat_order[, d] != 2 & !visited)
                if (length(indR) > 0) {
                    path.list <- updateList(mpath[-1], indR, path.list)
                }
            }
            
            PossDe_a <- setdiff(which(visited == TRUE), union(a, S_A))
            new_S_A <- intersect(S_A, which(amat_order[, D] == 1 & 
                                           amat_order[D, ] == 2))
            if (bridged_new(PossDe_a, new_S_A, amat_order) == 0) {
                a3 <- union(a3, D)
            }
        }
    }
    return(union(union(a1, a2), a3))
}


bridged_new <- function(PossDe_X_new, local_parent, adj_mat, verbose = FALSE) {
    ## Purpose: Check if a path structure is "bridged"
    ## Returns 1 if bridged, 0 if unbridged
    ## ----------------------------------------------------------------------
    ## Arguments: - PossDe_X_new: set of vertices in possible descendant set
    ##            - local_parent: set of parent vertices
    ##            - adj_mat: adjacency matrix (PAG)
    ##            - verbose: print debug information
    ## ----------------------------------------------------------------------
    
    if (verbose) {
        cat("        bridged_new: Checking", length(PossDe_X_new), "vertices:", 
            paste(PossDe_X_new, collapse=", "), "\n")
        if(!is.null(rownames(adj_mat))) {
            cat("        Vertex names:", paste(rownames(adj_mat)[PossDe_X_new], collapse=", "), "\n")
        }
        cat("        local_parent:", paste(local_parent, collapse=", "), "\n")
    }
    
    amat <- adj_mat[PossDe_X_new, PossDe_X_new]
    
    ind1 <- which((amat != t(amat) | amat != 1), arr.ind = TRUE)
    amat[ind1] <- 0
    if (length(amat) == 1) {
        if (verbose) cat("        Single vertex → unbridged (return 0)\n")
        return(0)  # Single vertex is unbridged
    }
    
    conn.comp <- RBGL::connectedComp(methods::as(amat, "graphNEL"))
    indicator <- TRUE
    
    if (verbose) {
        cat("        Connected components:", length(conn.comp), "\n")
    }
    
    for (v_set in conn.comp) {
        if (length(v_set) == 1) {
            if (verbose) cat("        Component size 1 - skipping\n")
            next
        } else {
            tem_adj <- adj_mat[v_set, v_set]
            edge_set <- which(tem_adj == 1 & t(tem_adj) == 1, arr.ind = TRUE)
            if (verbose) {
                cat("        Component size", length(v_set), "- edge_set length:", length(edge_set), "\n")
            }
            if (length(edge_set) < 3) {
                if (verbose) cat("        Too few edges (< 3) → bridged (return 1)\n")
                return(1)  # Return bridged instead of error
            }
            
            D_set <- lapply(v_set, function(v) {
                local_parent[which(adj_mat[local_parent, v] == 1 | 
                                  adj_mat[local_parent, v] == 2)]
            })
            
            n_edge <- dim(edge_set)[1]
            for (i in c(1:n_edge)) {
                ii <- edge_set[i, ]
                if (ii[1] > ii[2])
                    next
                
                index1 <- ii[1]
                index2 <- ii[2]
                
                D1 <- D_set[[index1]]
                D2 <- D_set[[index2]]
                
                if (length(setdiff(D1, D2)) > 0 && length(setdiff(D2, D1)) > 0) {
                    indicator <- FALSE
                    break
                } else {
                    if (length(setdiff(D1, D2)) > 0) {
                        tem_adj[index1, index2] <- 2
                        tem_adj[index2, index1] <- 3
                    } else if (length(setdiff(D2, D1)) > 0) {
                        tem_adj[index1, index2] <- 3
                        tem_adj[index2, index1] <- 2
                    }
                }
            }
            if (indicator == FALSE)
                return(0)
            
            # Check for unshielded colliders
            n_v_set <- length(v_set)
            if (n_v_set > 2) {
                for (i in c(1:(n_v_set - 2))) {
                    for (j in c((i + 1):(n_v_set - 1))) {
                        for (k in c((j + 1):n_v_set)) {
                            if (tem_adj[i, j] == 2 & tem_adj[k, j] == 2 & 
                                tem_adj[i, k] == 0)
                                return(0)
                            if (tem_adj[i, k] == 2 & tem_adj[j, k] == 2 & 
                                tem_adj[i, j] == 0)
                                return(0)
                            if (tem_adj[j, i] == 2 & tem_adj[k, i] == 2 & 
                                tem_adj[j, k] == 0)
                                return(0)
                        }
                    }
                }
            }
            if (!indicator) {
                return(0)
            }
        }
    }
    if (!indicator) {
        return(0)
    }
    return(1)
}


minUncovPdPath <- function(p, pag, a, b, c, unfVect, verbose = FALSE) {
    ## Purpose: find a minimal uncovered pd path for a,b,c saved in path.
    ## If a path exists this is the output, otherwise NA
    ## ----------------------------------------------------------------------
    ## Arguments: - p: number of nodes in the graph
    ##            - pag: adjacency matrix
    ##            - a,b,c : nodes under interest
    ##            - unfVect: vector containing the ambiguous triples
    ## ----------------------------------------------------------------------
    
    stopifnot((pag[a, b] == 1 | pag[a, b] == 2) &
                 (pag[b, a] == 1 | pag[b, a] == 3))
    min.upd.path <- NA
    done <- FALSE
    if ((pag[b, c] == 1 | pag[b, c] == 2) &
        (pag[c, b] == 1 | pag[c, b] == 3) &
        (pag[c, a] == 0)) {
        mpath <- c(a, b, c)
        if (length(unfVect) == 0 || 
            faith.check(mpath, unfVect, p)) {
            min.upd.path <- mpath
            if (verbose)
                cat('    minUncovPdPath: path found: ', mpath, 
                    ', uncovered: ', TRUE, '\n')
            done <- TRUE
        }
    }
    
    if (!done) {
        visited <- rep(FALSE, p)
        visited[c(a, b, c)] <- TRUE
        min.upd.path <- NA
        indD <- which((pag[b, ] == 1 | pag[b, ] == 2) &
                      (pag[, b] == 1 | pag[, b] == 3) &
                      (pag[, a] == 0) & !visited)
        if (length(indD) > 0) {
            path.list <- updateList(b, indD, NULL)
            done <- FALSE
            while ((length(path.list) > 0) && (!done)) {
                mpath <- path.list[[1]]
                m <- length(mpath)
                d <- mpath[m]
                path.list[[1]] <- NULL
                visited[d] <- TRUE
                if (any(pag[d, c] == 1:2) && any(pag[c, d] == c(1, 3))) {
                    mpath <- c(a, mpath, c)
                    n <- length(mpath)
                    uncov <- TRUE
                    for (l in seq_len(n - 2)) {
                        if (!(pag[mpath[l], mpath[l + 2]] == 0 &&
                              pag[mpath[l + 2], mpath[l]] == 0)) {
                            uncov <- FALSE
                            break
                        }
                    }
                    if (verbose)
                        cat('    minUncovPdPath: path found: ', mpath,
                            ', uncovered: ', uncov, '\n')
                    if (uncov)
                        if (length(unfVect) == 0 ||
                            faith.check(mpath, unfVect, p)) {
                            min.upd.path <- mpath
                            done <- TRUE
                        }
                } else {
                    indR <- which((pag[d, ] == 1 | pag[d, ] == 2) &
                                  (pag[, d] == 1 | pag[, d] == 3) & !visited)
                    if (length(indR) > 0) {
                        path.list <- updateList(mpath, indR, path.list)
                    }
                }
            }
        }
    }
    min.upd.path
}


faith.check <- function(path, unfVect, p) {
    ## Helper function for minUncovPdPath  
    ## Check if path is faithful (not in unfaithful triple list)
    if (length(unfVect) == 0) 
        return(TRUE)
    n <- length(path)
    for (i in seq_len(n - 2)) {
        triple <- c(path[i], path[i + 1], path[i + 2])
        if (any(unfVect == triple2numb(p, triple[1], triple[2], triple[3])) ||
            any(unfVect == triple2numb(p, triple[3], triple[2], triple[1]))) {
            return(FALSE)
        }
    }
    return(TRUE)
}


triple2numb <- function(p, i, j, k) {
    ## Convert triple (i,j,k) to unique number for hashing
    (i - 1) * p^2 + (j - 1) * p + k
}
