#
# Rule 4
#

# library(pcalg)
# library(graph)

# source("graph_utils.R")



discriminating_paths_helper <- function(pag, path, y, all_paths=list()) {
    p <- nrow(pag)
    n <- length(path)
    q_i <- path[n]
    q_i_1 <- path[n-1]
    
    # if Q_{i-1} *-o Q_i, next edge mark should be Q_i <-* Q_{i+1}
    if (pag[q_i_1, q_i] == 1) {
        
        nbrs_q_i <- which(pag[q_i, ] == 2)
        for (j in seq_along(nbrs_q_i)) {
            q_j <- nbrs_q_i[j]
            if (pag[q_j, q_i] != 2 || q_j %in% path) {
                next
            }
            
            # TODO: Check this edge case
            if (n == 2) {
                if (pag[q_i_1, q_j] != 2) {
                    next
                }
            }
            else if (pag[q_i_1, q_j] != 2 || pag[q_j, q_i_1] != 1) {
                next
            }
            
            # check for end condition
            if (pag[q_j, q_i] == 2 && pag[y, q_j] == 1) {
                path_j <- c(path, q_j, y)
                all_paths <- c(all_paths, list(path_j))
            }
            else if (pag[q_j, y] == 2 && pag[y, q_j] == 3) {
                all_paths <- discriminating_paths_helper(pag, c(path, q_j), y, all_paths)
            }
        }
    }
    # otherwise, next edge mark should just not be a tail
    else {
        nbrs_q_i <- which(pag[, q_i] != 3 & pag[, q_i] != 0)
        for (j in seq_along(nbrs_q_i)) {
            q_j <- nbrs_q_i[j]
            if (q_j %in% path) {
                next
            }
            if (pag[q_j, q_i] == 1) {
                
                # TODO: Check this edge case
                if (n == 2) {
                    if (pag[q_i_1, q_j] != 1) {
                        next
                    }
                    if (pag[y, q_j] == 1) {
                        path_j <- c(path, q_j, y)
                        all_paths <- c(all_paths, list(path_j))
                    }
                }
                else if (pag[q_i_1, q_j] != 1 || pag[q_j, q_i_1] != 2) {
                    next
                }
            }
            
            # check for end condition
            if (pag[q_j, q_i] == 2 && pag[y, q_j] == 1) {
                path_j <- c(path, q_j, y)
                all_paths <- c(all_paths, list(path_j))
            }
            else if (pag[q_j, y] == 2 && pag[y, q_j] == 3) {
                all_paths <- discriminating_paths_helper(pag, c(path, q_j), y, all_paths)
            }
        }
    }
    
    return(all_paths)
}


find_almost_discriminating_paths <- function(pag, x, y) {
    paths <- list()
    num_paths <- 0
    
    # If X and Y are adjacent, there can be no discriminating paths
    # cat(x, y, "\n")
    if (pag[x, y] != 0) {
        return(paths)
    }
    
    possible_path <- c(x)
    q1_nodes <- which(pag[x, ] == 1 | pag[x, ] == 2)
    for (i in seq_along(q1_nodes)) {
        q1 <- q1_nodes[i]
        current_path <- c(x, q1)
        if (pag[q1, y] == 2 && pag[y, q1] == 3) {
            paths_q1 <- discriminating_paths_helper(pag, current_path, y)
            paths <- c(paths, paths_q1)
        }
    }
    
    
    return(paths)
}


# rule.4 <- function(pag, neighbors=NULL, unfVect=NULL, verbose=FALSE) {
#     
#     p <- nrow(pag)
#     
#     for (x in seq(p)) {
#         for (y in seq(p)) {
#             if (x == y) {
#                 next
#             }
#             
#             # find all almost discriminating paths
#             paths_x_y <- find_almost_discriminating_paths(pag, x, y)
#             
#             # for every almost discriminating path perform orientation
#             for (idx in seq_along(paths_x_y)) {
#                 path_i <- paths_x_y[[idx]]
#                 n <- length(path_i)
#                 q_k <- path_i[n-1]
#                 if (pag[y, q_k] == 1) {
#                     
#                     if (verbose) {
#                         cat("Found almost discriminating path: ", x, " ... ")
#                         cat(q_k, " o-* ", y, "\n")
#                         cat("Using Rule 4 to orient ", q_k, " -> ", y, "\n\n")
#                     }
#                     
#                     pag[y, q_k] <- 3
#                     pag[q_k, y] <- 2
#                 }
#             }
#         }
#     }
#     
#     return(pag)
# }

rule.4.fast <- function(pag, neighbors=NULL, unfVect=NULL, verbose=FALSE) {
    
    
    p <- nrow(pag)
    
    # This gives us a list of candidate end points
    partial_edges <- expertOrientR:::get_partial_edges(pag)
    
    for (i in seq_len(nrow(partial_edges))) {
        q_k <- partial_edges[i, 1]
        y <- partial_edges[i, 2]
        
        for (x in seq(p)) {
            if (x == y) {
                next
            }
            
            # find all almost discriminating paths
            paths_x_y <- find_almost_discriminating_paths(pag, x, y)
            
            # for every almost discriminating path perform orientation
            for (idx in seq_along(paths_x_y)) {
                path_i <- paths_x_y[[idx]]
                n <- length(path_i)
                q_k <- path_i[n-1]
                if (pag[y, q_k] == 1) {
                    
                    if (verbose) {
                        cat("Path:", path_i, "\n")
                        cat("Found almost discriminating path: ", x, " ... ")
                        cat(q_k, " o-* ", y, "\n")
                        cat("Using Rule 4 to orient ", q_k, " -> ", y, "\n\n")
                    }
                    
                    pag[y, q_k] <- 3
                    pag[q_k, y] <- 2
                }
            }
        }
    }
    
    
    return(pag)
}

# rule.4.fast.v2 <- function(pag, neighbors=NULL, unfVect=NULL, verbose=FALSE) {
#     
#     
#     p <- nrow(pag)
#     
#     if (is.null(neighbors)) {
#         neighbors <- find_neighbors(pag)
#     }
#     
#     # This gives us a list of candidate end points
#     partial_edges <- get_partial_edges(pag)
#     
#     for (i in seq_len(nrow(partial_edges))) {
#         q_k <- partial_edges[i, 1]
#         y <- partial_edges[i, 2]
#         
#         # Find neighbors of y, Ne(y)
#         ne_y <- neighbors[[y]]
#         ne_y <- c(ne_y, y)
#         
#         # Find parents of y, Pa(y)
#         pa_y <- which(pag[, y] == 2 & pag[y, ] == 3, arr.ind=T)
#         # print(y)
#         # print(pag)
#         # print(pa_y)
#         # print("---")
#         
#         # Find neighbors of Pa(y), NePa(y) = U_{q_k \in Pa(y)} Ne(q_k)
#         nepa_y <- c()
#         # print(pag)
#         for (q_ki in seq_len(length(pa_y))) {
#             q_k <- pa_y[q_ki]
#             # cat(q_k, ",")
#             nepa_y <- c(nepa_y, neighbors[[q_k]])
#         }
#         nepa_y <- unique(nepa_y)
#         # cat("\n")
#         # print(nepa_y)
#         # print("---")
#         
#         # Find candidates of x, X = NePa(y) \ Ne(y)
#         X_candidates <- setdiff(nepa_y, ne_y)
#         
#         
#         # for (x in seq(p)) {
#         for (xi in seq_len(length(X_candidates))) {
#             x <- X_candidates[xi]
#             if (x == y) {
#                 next
#             }
#             
#             # find all almost discriminating paths
#             paths_x_y <- find_almost_discriminating_paths(pag, x, y)
#             
#             # for every almost discriminating path perform orientation
#             for (idx in seq_along(paths_x_y)) {
#                 path_i <- paths_x_y[[idx]]
#                 n <- length(path_i)
#                 q_k <- path_i[n-1]
#                 if (pag[y, q_k] == 1) {
#                     
#                     if (verbose) {
#                         cat("Found almost discriminating path: ", x, " ... ")
#                         cat(q_k, " o-* ", y, "\n")
#                         cat("Using Rule 4 to orient ", q_k, " -> ", y, "\n\n")
#                     }
#                     
#                     pag[y, q_k] <- 3
#                     pag[q_k, y] <- 2
#                 }
#             }
#         }
#     }
#     
#     
#     return(pag)
# }



# pag <- matrix(c(c(0, 2, 0, 2, 2),
#                 c(3, 0, 1, 0, 3),
#                 c(0, 2, 0, 2, 2),
#                 c(2, 0, 1, 0, 1),
#                 c(1, 2, 1, 2, 0)), nrow=5)
# pag <- t(pag)
# 
# rule.4.fast(pag, T)

