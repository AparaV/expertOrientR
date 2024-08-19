# source("graph_utils.R")
# source("orientation_rules/helpers.R")



rule_13 <- function(pag, unfVect=NULL, verbose=FALSE) {
    
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






# rule_13_tz <- function(pag, unfVect=NULL, verbose=FALSE) {
#     
#     V <- seq_len(nrow(pag))
#     colnames(pag) <- rownames(pag) <- V
#     
#     # Find all A o-* B edges
#     circle_edges <- which(pag == 1, arr.ind=T)
#     num_circle_edges <- nrow(circle_edges)
#     
#     uc_pag <- find_unshielded_colliders(pag)
#     
#     # For each A o-* B edge
#     for (i in seq_len(num_circle_edges)) {
#         A <- circle_edges[i, 2]
#         B <- circle_edges[i, 1]
#         
#         # Find S_A
#         S_A <- which(pag[, A] == 2, arr.ind=T)
#         S_A <- c(S_A, A)
#         
#         # Find all nodes in K \in V \ S_A such that there is an uncovered pd <A, B, ..., K>
#         leftover_nodes <- setdiff(V, S_A)
#         D <- c()
#         possible_descendants_paths <- uncovered.possible.descendants(pag, A)
#         num_paths <- length(possible_descendants_paths)
#         for (i in seq_len(num_paths)) {
#             path_i <- possible_descendants_paths[[i]]
#             if (length(path_i) > 2) {
#                 if (path_i[2] == B) {
#                     K <- path_i[length(path_i)]
#                     if (K %in% leftover_nodes) {
#                         D <- c(D, K)
#                     }
#                 }
#             }
#         }
#         
#         if (length(D) <= 1) {
#             next
#         }
#         
#         # Transformation 1
#         pag2 <- pag
#         for (i in seq_len(length(S_A))) {
#             V1 <- S_A[i]
#             for (j in seq_len(length(D))) {
#                 Vj <- D[j]
#                 if (pag2[V1, Vj] == 1) {
#                     pag2[V1, Vj] <- 2
#                 }
#             }
#         }
#         
#         # Find F_V in original pag with respect to S_A
#         F_V <- list()
#         for (i in seq_len(length(V))) {
#             Vi <- V[i]
#             F_Vi <- which(pag[, Vi] == 1 | pag[, Vi] == 2, arr.ind=T)
#             F_Vi <- intersect(F_Vi, S_A)
#             F_V <- c(F_V, list(F_Vi))
#         }
#         
#         # Transformation 2
#         for (i in seq_len(length(D)-1)) {
#             Vi <- D[i]
#             for (j in seq(i+1, length(D))) {
#                 Vj <- D[j]
#                 
#                 if (pag2[Vj, Vi] != 1 || pag2[Vi, Vj] != 1) {
#                     next
#                 }
#                 
#                 F_Vi_minus_F_Vj <- setdiff(F_V[[Vi]], F_V[[Vj]])
#                 
#                 Vi_pa <- which(pag2[, Vi] == 2 & pag2[Vi, ] == 3, arr.ind=T)
#                 Vj_non_neighbors <- which(pag2[, Vj] == 0, arr.ind=T)
#                 Vk_wrt_Vi <- intersect(Vi_pa, D)
#                 Vk_wrt_Vi <- intersect(Vk_wrt_Vi, Vj_non_neighbors)
#                 
#                 F_Vj_minus_F_Vi <- setdiff(F_V[[Vj]], F_V[[Vi]])
#                 
#                 Vj_pa <- which(pag2[, Vj] == 2 & pag2[Vj, ] == 3, arr.ind=T)
#                 Vi_non_neighbors <- which(pag2[, Vi] == 0, arr.ind=T)
#                 Vk_wrt_Vj <- intersect(Vj_pa, D)
#                 Vk_wrt_Vj <- intersect(Vk_wrt_Vj, Vi_non_neighbors)
#                 
#                 if (length(F_Vi_minus_F_Vj) > 0 || length(Vk_wrt_Vi) > 0) {
#                     pag2[Vi, Vj] <- 2
#                     pag2[Vj, Vi] <- 3
#                 }
#                 else if (length(F_Vj_minus_F_Vi) > 0 || length(Vk_wrt_Vj) > 0) {
#                     pag2[Vi, Vj] <- 3
#                     pag2[Vj, Vi] <- 2
#                 }
#                 
#                 
#             }
#         }
#         
#         uc_pag2 <- find_unshielded_colliders(pag2)
#         
#         if (nrow(uc_pag) != nrow(uc_pag2) || !all(uc_pag == uc_pag2)) {
#             if (verbose) {
#                 cat("\nRule 13: Transformation of mixed graph with respect to edge ")
#                 cat(A, "o-*", B, "created new unshielded colliders.\n")
#                 cat("Turning", A, "o-*", B, "into", A, "<-*", B, "\n")
#             }
#             pag[B, A] <- 2
#         }
#         
#     }
#     
#     return(pag)
# }

