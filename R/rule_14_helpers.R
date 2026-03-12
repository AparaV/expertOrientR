#
# Helper functions for Rule 14
#
# This file contains modular helper functions for implementing Rule 14
# according to the formal definitions.
#

# ==============================================================================
# MODULE 1: Find Unbridged Paths
# ==============================================================================

#' Find unbridged paths relative to {T1, T2}
#'
#' An unbridged path F1 o-o ... o-o Fk relative to {T1, T2} satisfies:
#' - There exist C1, C2 in {T1, T2}
#' - Structure: C1 <-o F1 o-o F2 o-o ... o-o Fk o-> C2
#'   (i.e., F1 o-> C1 and Fk o-> C2)
#' - NO edge C1 <-> C2 exists
#' - X o-* Fi for all Fi in the path
#'
#' @param pag Adjacency matrix (PAG)
#' @param T1 First vertex in reference set
#' @param T2 Second vertex in reference set
#' @param X Reference vertex (must have X o-* Fi for all Fi)
#' @param verbose Print debug information
#' @return List of unbridged paths (each path is a vector of vertex indices)
find_unbridged_paths <- function(pag, T1, T2, X, verbose = FALSE) {
    
    if (verbose) {
        cat("Finding unbridged paths relative to {", T1, ",", T2, "} for X =", X, "\n")
    }
    
    p <- nrow(pag)
    unbridged_paths <- list()
    
    # Try both orderings: C1=T1, C2=T2 and C1=T2, C2=T1
    for (config in 1:2) {
        if (config == 1) {
            C1 <- T1
            C2 <- T2
        } else {
            C1 <- T2
            C2 <- T1
        }
        
        # Check that C1 <-> C2 does NOT exist
        if (pag[C1, C2] == 2 && pag[C2, C1] == 2) {
            if (verbose) {
                cat("  Skipping config (", C1, ",", C2, "): C1 <-> C2 exists\n")
            }
            next
        }
        
        # Find all vertices F1 such that F1 o-> C1
        # This means: pag[C1, F1] = 2 (arrowhead at C1) and pag[F1, C1] = 1 (circle at F1)
        indF1 <- which(pag[, C1] == 2 & pag[C1, ] == 1)
        
        if (length(indF1) == 0) {
            if (verbose) {
                cat("  No F1 vertices with F1 o-> C1 for C1 =", C1, "\n")
            }
            next
        }
        
        # For each F1, find paths F1 o-o ... o-o Fk where Fk o-> C2
        for (F1 in indF1) {
            # Use BFS to find o-o paths from F1 to vertices that have o-> C2
            paths_from_F1 <- find_circle_paths_to_targets(pag, F1, C2, verbose = verbose)
            
            # Filter paths: verify all Fi satisfy X o-* Fi
            for (path in paths_from_F1) {
                if (length(path) < 2) next  # Need at least F1 and Fk
                
                # Check X o-* Fi for all Fi in path
                all_connected_to_X <- TRUE
                for (Fi in path) {
                    # X o-* Fi means: pag[Fi, X] = 1 (circle at X) and pag[X, Fi] ∈ {1,2,3} (any mark at Fi)
                    if (!(pag[Fi, X] == 1 && pag[X, Fi] != 0)) {
                        all_connected_to_X <- FALSE
                        break
                    }
                }
                
                if (all_connected_to_X) {
                    unbridged_paths[[length(unbridged_paths) + 1]] <- list(
                        path = path,
                        C1 = C1,
                        C2 = C2
                    )
                    if (verbose) {
                        cat("  Found unbridged path:", paste(path, collapse = " o-o "), 
                            "with C1 =", C1, ", C2 =", C2, "\n")
                    }
                }
            }
        }
    }
    
    return(unbridged_paths)
}


#' Find circle-circle paths from F1 to vertices that can reach C2
#'
#' Finds paths where all edges are o-o and the last vertex has o-> C2
#'
#' @param pag Adjacency matrix
#' @param F1 Starting vertex
#' @param C2 Target vertex (looking for Fk where Fk o-> C2)
#' @param verbose Print debug information
#' @return List of paths (each path is a vector of vertices)
find_circle_paths_to_targets <- function(pag, F1, C2, verbose = FALSE) {
    
    p <- nrow(pag)
    valid_paths <- list()
    visited <- rep(FALSE, p)
    visited[F1] <- TRUE
    
    # BFS with paths
    queue <- list(list(path = c(F1)))
    
    while (length(queue) > 0) {
        current <- queue[[1]]
        queue[[1]] <- NULL
        
        current_path <- current$path
        current_vertex <- current_path[length(current_path)]
        
        # Check if current vertex has o-> C2
        # This means: pag[current_vertex, C2] = 2 and pag[C2, current_vertex] = 1
        if (pag[current_vertex, C2] == 2 && pag[C2, current_vertex] == 1) {
            valid_paths[[length(valid_paths) + 1]] <- current_path
            if (verbose) {
                cat("    Found path to C2:", paste(current_path, collapse = " -> "), "\n")
            }
        }
        
        # Find neighbors with o-o edges
        # o-o edge: pag[i,j] = 1 and pag[j,i] = 1
        neighbors <- which(pag[current_vertex, ] == 1 & pag[, current_vertex] == 1 & !visited)
        
        for (neighbor in neighbors) {
            visited[neighbor] <- TRUE
            new_path <- c(current_path, neighbor)
            queue[[length(queue) + 1]] <- list(path = new_path)
        }
    }
    
    return(valid_paths)
}


# ==============================================================================
# MODULE 2: Prior-To Relationship
# ==============================================================================

#' Check if vertex A is prior to B relative to X
#'
#' Implements Definition 3: A is prior to B relative to X if there exists
#' a sequence F0 = A, F1, ..., Ft = B where Fi *-o X and one of three
#' conditions holds for each step.
#'
#' @param pag Adjacency matrix
#' @param A Starting vertex
#' @param B Target vertex
#' @param X Reference vertex
#' @param verbose Print debug information
#' @return TRUE if A is prior to B, FALSE otherwise
is_prior_to <- function(pag, A, B, X, verbose = FALSE) {
    
    # Build the transitive closure of the prior-to relation
    prior_to_B <- find_all_prior_to(pag, B, X, verbose = verbose)
    
    return(A %in% prior_to_B)
}


#' Find all vertices that are prior to B relative to X
#'
#' Uses BFS to build the transitive closure of the prior-to relationship
#'
#' @param pag Adjacency matrix
#' @param B Target vertex
#' @param X Reference vertex
#' @param verbose Print debug information
#' @return Vector of vertex indices that are prior to B
find_all_prior_to <- function(pag, B, X, verbose = FALSE) {
    
    p <- nrow(pag)
    prior_set <- c()
    remaining <- c(B)
    visited <- rep(FALSE, p)
    visited[B] <- TRUE
    
    # Find vertices in circle component around X (optimization)
    X_circle <- which(pag[, X] == 1 & pag[X, ] != 0)
    
    # Also include possible ancestors of B
    possAn_B <- expertOrientR:::find_possibleancestor(pag, B)
    candidates <- intersect(X_circle, possAn_B)
    candidates <- setdiff(candidates, X)  # Exclude X itself
    
    if (verbose) {
        cat("Finding vertices prior to", B, "relative to X =", X, "\n")
        cat("  Candidate set:", paste(candidates, collapse = ", "), "\n")
    }
    
    while (length(remaining) > 0) {
        current <- remaining[1]
        remaining <- remaining[-1]
        
        # Find vertices that are prior to current with order 1
        remain_candidates <- intersect(which(!visited), candidates)
        new_vertices <- is_prior_to_order_1(pag, current, X, remain_candidates, verbose = verbose)
        
        if (length(new_vertices) > 0) {
            prior_set <- c(prior_set, new_vertices)
            remaining <- c(remaining, new_vertices)
            visited[new_vertices] <- TRUE
            
            if (verbose) {
                cat("  Found", length(new_vertices), "vertices prior to", current, ":", 
                    paste(new_vertices, collapse = ", "), "\n")
            }
        }
    }
    
    return(unique(prior_set))
}


#' Check if any vertex in candidate_set is prior to target with order 1
#'
#' Implements the three conditions from Definition 3
#'
#' @param pag Adjacency matrix
#' @param target Target vertex (the Fi+1 in the definition)
#' @param X Reference vertex
#' @param candidate_set Set of candidate vertices to check
#' @param verbose Print debug information
#' @return Vector of vertices that are prior to target
is_prior_to_order_1 <- function(pag, target, X, candidate_set, verbose = FALSE) {
    
    if (length(candidate_set) == 0) {
        return(c())
    }
    
    prior_vertices <- c()
    
    # Condition 1: Direct edge Fi --> target
    cond1_vertices <- find_prior_condition_1(pag, target, candidate_set)
    if (verbose && length(cond1_vertices) > 0) {
        cat("    Condition 1: Found", length(cond1_vertices), "vertices\n")
    }
    prior_vertices <- union(prior_vertices, cond1_vertices)
    
    # Condition 2: Uncovered PD path <X, Fi, M, ...> where M --> target
    cond2_vertices <- find_prior_condition_2(pag, target, X, candidate_set)
    if (verbose && length(cond2_vertices) > 0) {
        cat("    Condition 2: Found", length(cond2_vertices), "vertices\n")
    }
    prior_vertices <- union(prior_vertices, cond2_vertices)
    
    # Condition 3: Unbridged path structure in induced subgraph
    cond3_vertices <- find_prior_condition_3(pag, target, X, candidate_set, verbose = verbose)
    prior_vertices <- union(prior_vertices, cond3_vertices)
    
    return(prior_vertices)
}


#' Find vertices satisfying Condition 1: Direct edge
#'
#' Find vertices Fi in candidate_set where Fi --> target
#'
#' @param pag Adjacency matrix
#' @param target Target vertex (Fi+1)
#' @param candidate_set Candidates for Fi
#' @return Vector of vertices with direct edges to target
find_prior_condition_1 <- function(pag, target, candidate_set) {
    # Fi --> target means: pag[Fi, target] = 2 (arrowhead at target) and pag[target, Fi] = 3 (tail at Fi)
    vertices <- candidate_set[which(pag[candidate_set, target] == 2 & 
                                    pag[target, candidate_set] == 3)]
    return(vertices)
}


#' Find vertices satisfying Condition 2: Uncovered PD path to parent
#'
#' Find vertices Fi where there exists:
#' - M --> target (M is a parent of target)
#' - Uncovered PD path <X, Fi, M, ...> where Fi is immediately after X
#'
#' @param pag Adjacency matrix
#' @param target Target vertex (Fi+1)
#' @param X Reference vertex
#' @param candidate_set Candidates for Fi
#' @return Vector of vertices satisfying condition 2
find_prior_condition_2 <- function(pag, target, X, candidate_set) {
    
    # Find parents M of target (M --> target)
    parents_M <- which(pag[target, ] == 2 & pag[, target] == 3)
    
    if (length(parents_M) == 0) {
        return(c())
    }
    
    vertices <- c()
    
    for (M in parents_M) {
        # Check if X and M are not adjacent
        if (pag[X, M] != 0 || pag[M, X] != 0) {
            next
        }
        
        # Find candidates Fi where Fi *-o X (circle at X, some mark at Fi)
        candidates_with_circle <- intersect(candidate_set, 
                                           which(pag[X, ] == 1 & pag[, X] != 0))
        
        # For each candidate, check if there's an uncovered PD path <X, Fi, M, ...>
        for (Fi in candidates_with_circle) {
            # Check for uncovered PD path from X to M through Fi
            # Fi must be immediately after X in the path
            path <- expertOrientR:::minUncovPdPath(nrow(pag), pag, X, Fi, M, unfVect = NULL, verbose = FALSE)
            
            if (!is.na(path[1]) && length(path) >= 3) {
                # Verify Fi is the second vertex (immediately after X)
                if (path[2] == Fi) {
                    vertices <- union(vertices, Fi)
                }
            }
        }
    }
    
    return(vertices)
}


#' Find vertices satisfying Condition 3: Unbridged path in induced subgraph
#'
#' This is the most complex condition - for now, return empty set
#' (Can be implemented if needed, but may not be critical for many cases)
#'
#' @param pag Adjacency matrix
#' @param target Target vertex (Fi+1)
#' @param X Reference vertex
#' @param candidate_set Candidates for Fi
#' @param verbose Print debug information
#' @return Vector of vertices satisfying condition 3
find_prior_condition_3 <- function(pag, target, X, candidate_set, verbose = FALSE) {
    # TODO: Implement if needed
    # This requires:
    # 1. Construct S_{Fi+1} = {V | V *-> X} ∪ {Fi+1}
    # 2. Create induced subgraph H[-S_{Fi+1}]
    # 3. Find unbridged paths in that subgraph
    # 4. Check uncovered PD paths from X to each Kj
    
    # For now, delegate to existing implementation
    vertices <- c()
    
    # Use the existing detect_order_1 logic for condition 3
    # This is a simplified approach
    poss_a3 <- which(pag[, X] == 1 & pag[X, ] != 3 & 
                     pag[, target] == 2 & pag[target, ] == 1)
    
    # Only check candidates in poss_a3
    poss_a3 <- intersect(poss_a3, candidate_set)
    
    poss_S1 <- which(pag[, X] == 2 & pag[X, ] == 2 & 
                    pag[, target] == 2 & pag[target, ] == 2)
    S_A <- poss_S1
    p <- ncol(pag)
    
    if (verbose && length(poss_a3) > 0) {
        cat("    Condition 3: Checking", length(poss_a3), "poss_a3 vertices\n")
        cat("    Condition 3: Found", length(poss_S1), "poss_S1 (S_A) vertices\n")
    }
    
    if (length(poss_a3) != 0 & length(poss_S1) != 0) {
        for (D in poss_a3) {
            if (verbose) {
                cat("    Condition 3: Testing vertex", D, "\n")
            }
            visited <- rep(FALSE, ncol(pag))
            path.list <- list(c(X, D))
            
            while (length(path.list) > 0) {
                mpath <- path.list[[1]]
                m <- length(mpath)
                d <- mpath[m]
                pred <- mpath[m - 1]
                path.list[[1]] <- NULL
                
                visited[c(X, d)] <- TRUE
                # Only traverse along edges that are not directed away from current vertex
                # i.e., d o-o r or d o-* r where * is not an arrowhead
                # Exclude d o-> r (where pag[d, r] = 2)
                indR <- which(pag[pred, ] == 0 & pag[d, ] == 1 & pag[, d] != 2 & !visited)
                if (length(indR) > 0) {
                    for (r in indR) {
                        path.list[[length(path.list) + 1]] <- c(mpath, r)
                    }
                }
            }
            
            PossDe_a <- setdiff(which(visited == TRUE), union(X, S_A))
            new_S_A <- intersect(S_A, which(pag[, D] == 1 & pag[D, ] == 2))
            
            if (verbose) {
                cat("      PossDe_a size:", length(PossDe_a), "\n")
                if (length(PossDe_a) > 0 && length(PossDe_a) <= 10 && !is.null(colnames(pag))) {
                    cat("      PossDe_a vertices:", paste(colnames(pag)[PossDe_a], collapse=", "), "\n")
                }
                cat("      new_S_A size:", length(new_S_A), "\n")
            }
            
            if (length(PossDe_a) > 0) {
                bridged_result <- expertOrientR:::bridged_new(PossDe_a, new_S_A, pag, verbose = verbose)
                if (verbose) {
                    cat("      bridged_new result:", bridged_result, "(0=unbridged, 1=bridged)\n")
                }
                if (bridged_result == 0) {
                    vertices <- union(vertices, D)
                    if (verbose) {
                        cat("      ✓ Vertex", D, "satisfies Condition 3\n")
                    }
                }
            }
        }
    }
    
    return(vertices)
}
