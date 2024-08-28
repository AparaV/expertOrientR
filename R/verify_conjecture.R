
#' Verify conjecture
#' 
#' Verify whether the restricted equivalence class, represented by an adjacency matrix,
#' is a proper restriction with respect to some admissible and consistent expert knowledge.
#' 
#' @param pag Adjacency matrix of ancestral partial mixed graph. Typically, the output of `expertOrientR::complete_pag`
#' @param G Essential ancestral graph
#' @param stop_first Whether we should stop our search if we find some MAG that shares the same invariant edges as `pag`.
#' User should always set this to `FALSE` for verifying conjecture. It is set to `TRUE` by internal functions.
#' @param verbose Boolean describing verboseness level
#' @returns Boolean. `TRUE` if `pag` is a proper restriction. `FALSE` if `pag` is not a proper restriction.
#' @seealso [expertOrientR::generate_mag_pag()] for format of adjacency matrices
#' @export
verify_conjecture <- function(pag, G, stop_first=FALSE, verbose=FALSE) {
    
    # pag is the original essential ancestral graph, or PAG
    # G is the PAG + background knowledge + completed orientation rules
    # stop_first is a logical indicating whether we should search exhaustively by
    # turning every circle (in the non-circle component) into an arrow and tail,
    # or whether we just need to find one MAG.
    # if stop_first = TRUE, then we search only for one MAG
    
    # Turn off R3, R5-R7, R9, R11
    rules_to_use <- c(TRUE, TRUE, FALSE, TRUE, FALSE,
                      FALSE, FALSE, TRUE, FALSE, TRUE,
                      FALSE, TRUE, TRUE)
    rules_verbose <- FALSE
    inferred_edges_verbose <- TRUE
    
    
    ne_pag <- expertOrientR:::find_neighbors(pag)
    pa_pag <- expertOrientR:::find_parents(pag)
    si_pag <- expertOrientR:::find_siblings(pag)
    si_pairs_pag <- expertOrientR:::get_sibling_pairs(si_pag)
    dist_pag <- expertOrientR:::get_districts(si_pairs_pag)
    
    # Find all partially directed edges in PAG
    nonidentifed_edges <- which(pag == 1, arr.ind=T)
    num_nonidentifed_edges <- nrow(nonidentifed_edges)
    partial_edges_0 <- expertOrientR:::get_partial_edges(pag)
    pd_size_0 <- nrow(partial_edges_0)
    ccomp_0 <- expertOrientR:::get_circle_component(pag)
    num_edges <- nrow(which(pag != 0, arr.ind=T)) %/% 2
    
    
    # Identify all partially directed edges in G
    partial_edges <- expertOrientR:::filter_partial_edges(G, partial_edges_0)
    ccomp_g <- expertOrientR:::filter_circle_component(G, ccomp_0) # A *-o B
    ccomp_g_list <- lapply(seq_len(nrow(ccomp_g)), function(i) ccomp_g[i, ])
    ccomp_g_list_sym <- lapply(seq_len(nrow(ccomp_g)), function(i) c(ccomp_g[i, 2], ccomp_g[i, 1]))
    ccomp_g_list <- c(ccomp_g_list, ccomp_g_list_sym)
    pd_size <- nrow(partial_edges)
    ccomp_size_g <- num_edges - pd_size 
    
    nonidentifed_edges_after <- which(G == 1, arr.ind=T)
    num_nonidentifed_edges_after <- nrow(nonidentifed_edges_after)
    
    if (verbose) {
        cat("\nInitially", num_nonidentifed_edges, "non-identified edgemarks. Now", num_nonidentifed_edges_after, "edgemarks remain.\n")
        cat("Initially", pd_size_0, "partially-identified edges. Now", pd_size, "edges remain.\n")
    }
    
    # Get different orientations of the partially directed edges in G
    orientations <- expertOrientR:::enumerate_possibilities(partial_edges)
    orientations_l <- lapply(seq_len(nrow(orientations)), function(i) orientations[i, ])
    
    # All originally present unshielded colliders
    uc_0 <- expertOrientR:::find_unshielded_colliders(pag, list=TRUE)
    
    ccomp_arrows <- list()
    ccomp_tails <- list()
    ccomp_circs <- list()
    
    # Loop through the orientations
    while (length(orientations_l) > 0) {
        
        # Orient edge <A, B>
        a <- orientations_l[[1]][1]
        b <- orientations_l[[1]][2]
        a_edge_mark <- orientations_l[[1]][3]
        G_1 <- G
        G_1[b, a] <- a_edge_mark
        if (verbose) {
            cat("\nAdding edge,", a, b, a_edge_mark, "\n")
        }
        
        # Delete this edge orientation from orientations
        orientations_l <- orientations_l[-1]
        
        # Complete orientations
        result <- expertOrientR:::complete_pag(G_1, rules=rules_to_use, verbose=rules_verbose, debug=F)
        G_1 <- result$pag
        
        # Remove any edges in orientations_l that were inferred by the rules
        inferred_orientations_indices <- expertOrientR:::find_inferred_orientations(G_1, orientations_l, verbose=inferred_edges_verbose)
        if (length(inferred_orientations_indices) > 0) {
            orientations_l <- orientations_l[-inferred_orientations_indices]
        }
        
        # While there are unidentified edges in the non-circle component
        # Keep removing an orientation for it from orientations_l
        # If no leftover orientations in the queue, arbitrarily make it an arrowhead
        leftover_partial_edges <- expertOrientR:::filter_partial_edges(G_1, partial_edges)
        while (nrow(leftover_partial_edges) > 0) {
            
            # Pick next unoriented edge from orientations_l and remove it
            a1 <- leftover_partial_edges[1, 1]
            b1 <- leftover_partial_edges[1, 2]
            
            initial_num_orientations_l <- length(orientations_l)
            for (i in seq_len(initial_num_orientations_l)) {
                if (orientations_l[[i]][1] == a1 && orientations_l[[i]][2] == b1) {
                    a1_edge_mark <- orientations_l[[i]][3]
                    G_1[b1, a1] <- a1_edge_mark
                    orientations_l <- orientations_l[-i]
                    if (verbose) {
                        cat("\tAdding edge,", a1, b1, a1_edge_mark, "\n")
                    }
                    if (a1_edge_mark == 3) {
                        if (G_1[a1, b1] == 1) {
                            if (verbose) {
                                cat("\tEdge <", a1, b1, "> does not have an arrowhead at", b1, ".")
                                cat("Therefore, adding edge,", b1, a1, "2\n")
                            }
                            G_1[a1, b1] <-  2
                        }
                    }
                    break
                }
            }
            final_num_orientations_l <- length(orientations_l)
            if (initial_num_orientations_l == final_num_orientations_l) {
                G_1[b1, a1] <- 2
                if (verbose) {
                    cat("\tBrute force adding arrowhead,", a1, b1, 2, "\n")
                }
            }
            
            # Complete orientations
            result <- expertOrientR:::complete_pag(G_1, rules=rules_to_use, verbose=rules_verbose, debug=F)
            G_1 <- result$pag
            
            # Remove any edges in orientations_l that were inferred by the rules
            inferred_orientations_indices <- expertOrientR:::find_inferred_orientations(G_1, orientations_l, verbose=inferred_edges_verbose)
            if (length(inferred_orientations_indices) > 0) {
                orientations_l <- orientations_l[-inferred_orientations_indices]
            }
            
            leftover_partial_edges <- expertOrientR:::filter_partial_edges(G_1, partial_edges)
        }
        
        # Check for Markov equivalence
        
        # Check 1: Are unshielded colliders preserved?
        uc_g1 <- expertOrientR:::find_unshielded_colliders(G_1, list=TRUE)
        uc_g1_minus_uc_0 <- setdiff(uc_g1, uc_0)
        uc_0_minus_uc_g1 <- setdiff(uc_0, uc_g1)
        if (length(uc_g1_minus_uc_0) != 0 || length(uc_0_minus_uc_g1) != 0) {
            if (verbose) {
                print("Unshielded colliders are not preserved!")
                if (length(uc_g1_minus_uc_0) != 0) {
                    print("New unshielded colliders:")
                    print(uc_g1_minus_uc_0)
                }
                if (length(uc_0_minus_uc_g1) != 0) {
                    print("Lost unshielded colliders:")
                    print(uc_0_minus_uc_g1)
                }
            }
            return(FALSE)
        }
        
        # Check 2: Is G_1 ancestral?
        if (!expertOrientR:::is_ancestral(G_1)) {
            if (verbose) {
                print("Resulting graph is not ancestral!")
            }
            return(FALSE)
        }
        
        # Check 3: Discriminating collider equivalence
        if (!expertOrientR:::is_discr_path_equivalent(G_1, pag, ne_pag, pa_pag, si_pag, dist_pag)) {
            if (verbose) {
                print("Resulting graph is not markov equivalent to PAG")
            }
            return(FALSE)
        }
        
        # Keep track of any invariant edges in the circle component
        # A *-o B are the circle edges
        for (i in seq_len(nrow(ccomp_g))) {
            a <- ccomp_g[i, 1]
            b <- ccomp_g[i, 2]
            if (G_1[b, a] == 2) {
                ccomp_arrows <- c(ccomp_arrows, list(c(a, b)))
            }
            else if (G_1[b, a] == 3) {
                ccomp_tails <- c(ccomp_tails, list(c(a, b)))
            }
            else if (G_1[b, a] == 1) {
                ccomp_circs <- c(ccomp_circs, list(c(a, b)))
            }
        }
        
        if (verbose) {
            cat("Done with one graph\n")
        }
        if (stop_first) {
            if (verbose) {
                cat("Found one graph. Stopping search.\n")
            }
            return(TRUE)
        }
    }
    
    # Check for invariant edges in the circle component
    # If this edge is invariant, the final graph will not be Markov equivalent
    ccomp_tails_union <- union(ccomp_tails, ccomp_circs)
    ccomp_arrow_union <- union(ccomp_arrows, ccomp_circs)
    ccomp_arrow_invariant <- setdiff(ccomp_arrows, ccomp_tails_union)
    ccomp_tails_invariant <- setdiff(ccomp_tails, ccomp_arrow_union)
    
    arrow_variant <- TRUE
    tails_variant <- TRUE
    
    if (length(ccomp_arrow_invariant) != 0) {
        if (verbose) {
            cat("\nSome circle component edge marks are arrow head invariant!\n")
        }
        
        for (k in seq_len(length(ccomp_arrow_invariant))) {
            a <- ccomp_arrow_invariant[[k]][1]
            b <- ccomp_arrow_invariant[[k]][2]
            
            # Verify if this is okay
            G1 <- G
            G1[b, a] <- 3
            G1[a, b] <- 2
            mag_exists <- circle_component_verifier(pag, G1, verbose=verbose)
            if (!mag_exists) {
                arrow_variant <- FALSE
            }
            
            
            if (verbose) {
                if (mag_exists) {
                    cat("\tEdge: <", a, b, "> is okay\n")
                }
                else {
                    cat("\tEdge: <", a, b, "> is not okay\n")
                }
            }
        }
    } 
    if (length(ccomp_tails_invariant) != 0) {
        if (verbose) {
            cat("\nSome circle component edge marks are tail invariant!\n")
        }
        
        for (k in seq_len(length(ccomp_tails_invariant))) {
            a <- ccomp_tails_invariant[[k]][1]
            b <- ccomp_tails_invariant[[k]][2]
            
            # Verify if this is okay
            G1 <- G
            G1[b, a] <- 2
            mag_exists <- circle_component_verifier(pag, G1, verbose=verbose)
            if (!mag_exists) {
                tails_variant <- FALSE
            }
            
            if (verbose) {
                if (mag_exists) {
                    cat("\tEdge: <", a, b, "> is okay\n")
                }
                else {
                    cat("\tEdge: <", a, b, "> is not okay\n")
                }
            }
        }
    }
    
    if (arrow_variant && tails_variant) {
        if (verbose) {
            cat("\nSuccessfully enumerated all possibilities\n")
        }
        return(TRUE)
    }
    
    return(FALSE)
}



circle_component_verifier <- function(pag, G1, verbose=FALSE) {
    
    rules_to_use <- c(TRUE, TRUE, FALSE, TRUE, FALSE,
                      FALSE, FALSE, TRUE, FALSE, TRUE,
                      TRUE, TRUE, TRUE)
    
    if (verbose) {
        cat("\tCompleting background knowledge for this unverified edge\n")
    }
    G1_res <- expertOrientR:::complete_pag(G1, rules=rules_to_use, verbose=FALSE)
    G1 <- G1_res$pag
    
    mag_exists <- verify_conjecture(pag, G1, stop_first=TRUE, verbose=FALSE)
    return(mag_exists)
}
