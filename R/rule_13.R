#
# Rule 13
#

rule.13 <- function(pag, verbose=FALSE) {
    
    V <- seq_len(nrow(pag))
    colnames(pag) <- rownames(pag) <- V
    
    ccomp_pag <- expertOrientR:::get_circle_component(pag)
    ccomp_nodes <- unique(ccomp_pag[, 1])
    
    # Find all A o-* B edges
    circle_edges <- which(pag == 1, arr.ind=T)
    num_circle_edges <- nrow(circle_edges)
    
    for (i in seq_len(num_circle_edges)) {
        A <- circle_edges[i, 2]
        B <- circle_edges[i, 1]
        
        # Find spouses of A
        spouses_A <- which(pag[, A] == 2 & pag[A, ] == 2, arr.ind=T)
        if (length(spouses_A) < 2) {
            next
        }
        
        # Find candidates for the circle subpath
        leftover_nodes <- intersect(ccomp_nodes, setdiff(V, spouses_A))
        circle_candidates <- c()
        possible_descendants_paths <- expertOrientR:::uncovered.possible.descendants(pag, A)
        num_paths <- length(possible_descendants_paths)
        for (i in seq_len(num_paths)) {
            path_i <- possible_descendants_paths[[i]]
            if (length(path_i) > 2) {
                if (path_i[2] == B) {
                    K <- path_i[length(path_i)]
                    if (K %in% leftover_nodes) {
                        circle_candidates <- c(circle_candidates, K)
                    }
                }
            }
        }
        
        found <- FALSE
        for (i in seq_len(length(spouses_A) - 1)) {
            C <- spouses_A[i]
            for (j in seq(i+1, length(spouses_A))) {
                D <- spouses_A[j]
                
                subgraph_nodes <- c(C, D, circle_candidates)
                sub_pag <- pag[subgraph_nodes, subgraph_nodes]
                
                if (expertOrientR:::rule_13_paths(sub_pag, C, D)) {
                    if (verbose) {
                        cat("\nRule 13: Found an edge", A, "o-*", B, ", collider", C, "<->", A, "<->", D, ", ")
                        cat("and path", C, "<-o ... o-o ... o->", D, "\n")
                        cat("Turning", A, "o-*", B, "into", A, "<-*", B, "\n")
                    }
                    pag[B, A] <- 2
                    found <- TRUE
                    break
                }
                
            }
            if (found) {
                break
            }
        }
        
    }
    
    return(pag)
    
}