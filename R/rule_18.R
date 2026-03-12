#
# Rule 18 - Main Implementation
#
# Implements Rule 18 for PAG orientation
# Based on formal definition from the paper
#


#' Apply Rule 18 orientation rule
#'
#' Rule 18: Orient A o-> B as A <-> B when:
#' - A o-> B <-> D (A has circle-arrow to B, B has bidirected to D)
#' - There exist two minimal possible directed paths <D, T1, ..., A> and <D, T2, ..., A>
#' - Either:
#'   (a) There is an unbridged path F1 o-o ... o-o Fk relative to {T1, T2}
#'       such that D o-* Fi for all Fi, OR
#'   (b) T1 and T2 are not adjacent
#'
#' @param pag Adjacency matrix (will be modified in place)
#' @param verbose Print debug information
#' @return Updated PAG with Rule 18 orientations applied
rule.18 <- function(pag, unfVect=NULL, verbose = FALSE) {
    
    if (verbose) {
        cat("\n==============================================\n")
        cat("Applying Rule 18\n")
        cat("==============================================\n\n")
    }
    
    n <- ncol(pag)
    changes_made <- FALSE
    
    # Step 1: Find all A o-> B <-> D patterns
    candidates <- list()
    
    for (A in 1:n) {
        for (B in 1:n) {
            if (A == B) next
            
            # Check if A o-> B (circle at A, arrowhead at B)
            if (!(pag[B, A] == 1 && pag[A, B] == 2)) next
            
            # Find all D where B <-> D
            for (D in 1:n) {
                if (D == A || D == B) next
                
                # Check if B <-> D (bidirected)
                if (pag[B, D] == 2 && pag[D, B] == 2) {
                    candidates[[length(candidates) + 1]] <- list(A = A, B = B, D = D)
                }
            }
        }
    }
    
    if (verbose) {
        cat("Found", length(candidates), "A o-> B <-> D pattern(s) to check\n\n")
    }
    
    # Step 2: For each candidate, check conditions
    for (i in seq_along(candidates)) {
        cand <- candidates[[i]]
        A <- cand$A
        B <- cand$B
        D <- cand$D
        
        if (verbose) {
            cat("----------------------------------------------\n")
            cat("Pattern", i, ": A =", A, ", B =", B, ", D =", D, "\n")
            if (!is.null(colnames(pag))) {
                cat("  (", colnames(pag)[A], "o->", colnames(pag)[B], "<->", colnames(pag)[D], ")\n")
            }
        }
        
        # Find two minimal possible directed paths from D to A
        paths <- expertOrientR:::find_possible_directed_paths(pag, D, A, verbose = verbose)
        
        if (length(paths) < 2) {
            if (verbose) {
                cat("  Need at least 2 paths, found", length(paths), "- skipping\n")
            }
            next
        }
        
        # Get T1 and T2 (second vertices in first two paths)
        T1 <- expertOrientR:::get_path_second_vertex(paths[[1]])
        T2 <- expertOrientR:::get_path_second_vertex(paths[[2]])
        
        if (is.null(T1) || is.null(T2)) {
            if (verbose) {
                cat("  Cannot extract T1/T2 from paths - skipping\n")
            }
            next
        }
        
        if (verbose) {
            cat("  T1 =", T1, ", T2 =", T2)
            if (!is.null(colnames(pag))) {
                cat(" (", colnames(pag)[T1], ",", colnames(pag)[T2], ")")
            }
            cat("\n")
        }
        
        # Check Condition (b): Are T1 and T2 non-adjacent?
        if (!expertOrientR:::are_adjacent(pag, T1, T2)) {
            if (verbose) {
                cat("  ✓ Condition (b) satisfied: T1 and T2 are NOT adjacent\n")
                cat("  → Orienting A o-> B as A <-> B\n")
            }
            
            pag[B, A] <- 2  # Change circle at A to arrowhead
            changes_made <- TRUE
            next
        }
        
        # Check Condition (a): Unbridged path relative to {T1, T2}
        # where D o-* all vertices on the path
        if (verbose) {
            cat("  T1 and T2 are adjacent, checking Condition (a)...\n")
        }
        
        # Only check if T1 <-> T2 (bi-directed)
        if (!(pag[T1, T2] == 2 && pag[T2, T1] == 2)) {
            if (verbose) {
                cat("  Adjacent but not bi-directed, skipping\n")
            }
            next
        }
        
        # Find unbridged paths between T1 and T2
        # Note: find_unbridged_paths needs a "parent" vertex; we use D here
        unbridged_paths <- expertOrientR:::find_unbridged_paths(pag, T1, T2, D, verbose = verbose)
        
        if (length(unbridged_paths) == 0) {
            if (verbose) {
                cat("  No unbridged paths found\n")
            }
            next
        }
        
        # Check if D o-* all vertices on at least one unbridged path
        path_satisfies_condition <- FALSE
        
        for (path in unbridged_paths) {
            all_connected <- TRUE
            
            for (Fi in path) {
                # Check if D o-* Fi (circle at Fi, anything at D except no edge)
                if (!expertOrientR:::check_edge_pattern(pag, Fi, D, "o-*")) {
                    all_connected <- FALSE
                    break
                }
            }
            
            if (all_connected) {
                path_satisfies_condition <- TRUE
                break
            }
        }
        
        if (path_satisfies_condition) {
            if (verbose) {
                cat("  ✓ Condition (a) satisfied: Found unbridged path with D o-* all vertices\n")
                cat("  → Orienting A o-> B as A <-> B\n")
            }
            
            pag[B, A] <- 2  # Change circle at A to arrowhead
            changes_made <- TRUE
        } else {
            if (verbose) {
                cat("  Condition (a) NOT satisfied\n")
            }
        }
    }
    
    # if (verbose) {
    #     cat("\n==============================================\n")
    #     if (changes_made) {
    #         cat("Rule 18 applied successfully\n")
    #     } else {
    #         cat("Rule 18 did not make any changes\n")
    #     }
    #     cat("==============================================\n\n")
    # }
    
    return(pag)
}
