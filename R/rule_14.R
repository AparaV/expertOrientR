#
# Rule 14 - Main Implementation
#
# Implements Rule 14 for PAG orientation using modular helper functions
# Based on formal definitions from the paper
#

#' Apply Rule 14 orientation rule
#'
#' Rule 14: Orient X o-> B as X --> B when there exist T1, T2 such that:
#' - Both T1 *-o X o-* T2 (circle at X)
#' - Both T1 and T2 are prior to B relative to X
#' - EITHER:
#'   (a) There exists an unbridged path F1 o-o ... o-o Fk relative to {T1, T2}
#'       where X o-* Fi for all Fi, OR
#'   (b) T1 and T2 are NOT adjacent
#'
#' @param pag Adjacency matrix (PAG)
#' @param unfVect Vector of ambiguous triples (optional, not used)
#' @param verbose Print debug information
#' @return Updated PAG with Rule 14 orientations applied
rule.14 <- function(pag, unfVect = NULL, verbose = FALSE) {
    
    if (verbose) {
        cat("\n==============================================\n")
        cat("Applying Rule 14\n")
        cat("==============================================\n\n")
    }
    
    p <- nrow(pag)
    old_pag <- pag
    
    # Find all X o-> B edges
    # X o-> B means: pag[B, X] = 2 (arrowhead at X->B direction) 
    #                and pag[X, B] = 1 (circle at B->X direction)
    ind <- which((pag == 2 & t(pag) == 1), arr.ind = TRUE)
    
    if (nrow(ind) == 0) {
        if (verbose) {
            cat("No X o-> B edges found\n")
        }
        return(pag)
    }
    
    if (verbose) {
        cat("Found", nrow(ind), "X o-> B edge(s) to check\n\n")
    }
    
    # Process each X o-> B edge
    edge_count <- 1
    while (nrow(ind) > 0) {
        # Get first edge
        X <- ind[1, 1]
        B <- ind[1, 2]
        ind <- ind[-1, , drop = FALSE]
        
        if (verbose) {
            cat("----------------------------------------------\n")
            cat("Edge", edge_count, ": Checking X =", X, ", B =", B, "\n")
            edge_count <- edge_count + 1
        }
        
        # Step 1: Extract circle component around X
        # Find all vertices with *-o X (some mark at vertex, circle at X)
        X_circle <- which(pag[, X] == 1 & pag[X, ] != 0)
        
        if (length(X_circle) < 2) {
            if (verbose) {
                cat("  Insufficient circle component (need ≥2 vertices, found", 
                    length(X_circle), ")\n")
            }
            next
        }
        
        if (verbose) {
            cat("  Circle component around X:", paste(X_circle, collapse = ", "), "\n")
        }
        
        # Step 2: Check for o-o edges in circle component (for Condition a)
        X_circle_amat <- pag[X_circle, X_circle]
        ind1 <- which((X_circle_amat != t(X_circle_amat) | X_circle_amat != 1), 
                     arr.ind = TRUE)
        X_circle_amat[ind1] <- 0
        
        has_oo_edges <- any(X_circle_amat == 1)
        
        if (verbose) {
            if (has_oo_edges) {
                cat("  Found o-o edges in circle component\n")
            } else {
                cat("  No o-o edges in circle component (will check Condition b only)\n")
            }
        }
        
        # Step 3: Find connected components in circle subgraph (if needed for Condition a)
        conn.comp <- NULL
        if (has_oo_edges) {
            conn.comp <- RBGL::connectedComp(methods::as(X_circle_amat, "graphNEL"))
            
            if (verbose) {
                cat("  Found", length(conn.comp), "connected component(s) in circle subgraph\n")
            }
        }
        
        if (verbose) {
            cat("  Found", length(conn.comp), "connected component(s) in circle subgraph\n")
        }
        
        # Step 4: Identify candidate vertices for T1, T2
        # Candidates must be in circle component but not B itself
        C_A <- setdiff(X_circle, B)
        
        if (length(C_A) < 2) {
            if (verbose) {
                cat("  Insufficient candidates for T1, T2 (need ≥2, found", 
                    length(C_A), ")\n")
            }
            next
        }
        
        # Optimization: Only consider vertices that could be prior to B
        possAn_B <- expertOrientR:::find_possibleancestor(pag, B)
        Z <- setdiff(intersect(X_circle, possAn_B), X)
        
        if (verbose) {
            cat("  Candidate vertices for T1, T2:", paste(C_A, collapse = ", "), "\n")
            cat("  Possible ancestors of B in candidates:", paste(Z, collapse = ", "), "\n")
        }
        
        # Step 5: Check all pairs (T1, T2)
        R14_triggered <- FALSE
        
        for (i in seq_along(C_A[-1])) {
            if (R14_triggered) break
            
            for (j in (i + 1):length(C_A)) {
                T1 <- C_A[i]
                T2 <- C_A[j]
                
                if (verbose) {
                    cat("\n  Checking pair: T1 =", T1, ", T2 =", T2, "\n")
                }
                
                # Step 5a: Check if both T1 and T2 are prior to B
                T1_prior <- expertOrientR:::is_prior_to(pag, T1, B, X, verbose = verbose)
                T2_prior <- expertOrientR:::is_prior_to(pag, T2, B, X, verbose = verbose)
                
                if (!T1_prior || !T2_prior) {
                    if (verbose) {
                        cat("    Prior-to check: T1 =", T1_prior, ", T2 =", T2_prior, 
                            "- FAILED\n")
                    }
                    next
                }
                
                if (verbose) {
                    cat("    ✓ Both T1 and T2 are prior to B\n")
                }
                
                # Step 5b: Check Condition (b) - Are T1 and T2 non-adjacent?
                T1_T2_adjacent <- (pag[T1, T2] != 0 || pag[T2, T1] != 0)
                
                if (!T1_T2_adjacent) {
                    # Condition (b) satisfied: T1 and T2 are not adjacent
                    if (verbose) {
                        cat("    ✓ Condition (b) satisfied: T1 and T2 are NOT adjacent\n")
                        cat("    → Orienting X o-> B as X --> B\n")
                    }
                    
                    pag[B, X] <- 3  # Add tail at X
                    R14_triggered <- TRUE
                    break
                }
                
                # Step 5c: Check Condition (a) - Unbridged path exists?
                if (verbose) {
                    cat("    T1 and T2 are adjacent, checking Condition (a)...\n")
                }
                
                # Only check if T1 <-> T2 (bi-directed)
                if (!(pag[T1, T2] == 2 && pag[T2, T1] == 2)) {
                    if (verbose) {
                        cat("    Adjacent but not bi-directed, skipping\n")
                    }
                    next
                }
                
                # Look for unbridged paths
                unbridged_paths <- expertOrientR:::find_unbridged_paths(pag, T1, T2, X, verbose = verbose)
                
                if (length(unbridged_paths) > 0) {
                    if (verbose) {
                        cat("    ✓ Condition (a) satisfied: Found", length(unbridged_paths), 
                            "unbridged path(s)\n")
                        cat("    → Orienting X o-> B as X --> B\n")
                    }
                    
                    pag[B, X] <- 3  # Add tail at X
                    R14_triggered <- TRUE
                    break
                } else {
                    if (verbose) {
                        cat("    Condition (a) NOT satisfied: No unbridged paths found\n")
                    }
                }
            }
        }
        
        if (!R14_triggered && verbose) {
            cat("\n  No valid (T1, T2) pair found - edge remains X o-> B\n")
        }
    }
    
    # # Count how many orientations were made
    # if (verbose) {
    #     changes <- sum(pag != old_pag)
    #     if (changes > 0) {
    #         cat("\n==============================================\n")
    #         cat("Rule 14 applied", changes / 2, "orientation(s)\n")
    #         cat("==============================================\n\n")
    #     } else {
    #         cat("\n==============================================\n")
    #         cat("Rule 14 made no changes\n")
    #         cat("==============================================\n\n")
    #     }
    # }
    
    return(pag)
}
