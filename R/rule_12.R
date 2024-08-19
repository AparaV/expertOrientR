#
# Rule 12
#

# library(pcalg)
# library(graph)
# 
# source("orientation_rules/helpers.R")


directed.path.length.3 <- function(pag, source, destination, avoid) {
    
    # Find all nodes d such that source --> d or source <-> d
    indD <- which(pag[source, ] == 2 & pag[, source] == 3)
    
    while(length(indD) > 0) {
        d <- indD[1]
        d.idx <- indD[[1]]
        
        if (d.idx == destination || d.idx %in% avoid) {
            next
        }
        
        if (pag[d.idx, destination] == 2 && pag[destination, d.idx] == 2) {
            # cat(source, "->", d.idx, "<->", destination, "\n")
            return(TRUE)
        }
        
    }
    
    # We did not find such a path
    return(FALSE)
}


rule.12 <- function(pag, unfVect=NULL, verbose=FALSE) {
    p <- nrow(pag)
    node_names <- names(pag[1,])
    for (x in seq_len(p)) {
        # Find all possible uncovered descendants of type x o-* ... o-* d
        descendant.paths <- expertOrientR:::uncovered.possible.descendants(pag, x)
        for (i in seq_len(length(descendant.paths))) {
            path.i <- descendant.paths[[i]]
            if (length(path.i) <= 2) {
                next
            }
            # print("Here")
            d <- tail(path.i, 1)
            avoid <- path.i[2:length(path.i)]
            # Is there a directed/almost directed path from d to x
            almost.cycle <- expertOrientR:::directed.path.exists(pag, d, x, avoid=avoid)
            # almost.cycle <- directed.path.length.3(pag, d, x, avoid=avoid)
            if (almost.cycle) {
                if (verbose) {
                    # cat("\nRule 12\nFound ", node_names[x], "o-*", node_names[path.i[2]])
                    # cat(" o-* ... o-*", node_names[d], "and (almost) directed path from ")
                    # cat(node_names[d], "to", node_names[x], "\n")
                    # cat("Marking", node_names[x], "<-*", node_names[path.i[2]], "\n")
                    cat("Path:", path.i, "\n")
                    cat("\nRule 12\nFound ", x, "o-*", path.i[2])
                    cat(" o-* ... o-*", d, "and (almost) directed path from ")
                    cat(d, "to", x, "\n")
                    cat("Marking", x, "<-*", path.i[2], "\n")
                }
                pag[path.i[2], x] <- 2
            }
        }
    }
    return(pag)
}



# rule.12.old <- function(pag, unfVect=NULL, verbose=FALSE) {
#     p <- nrow(pag)
#     node_names <- names(pag[1,])
#     for (x in seq_len(p)) {
#         # Find all possible uncovered descendants of type x o-* ... o-* d
#         descendant.paths <- uncovered.possible.descendants(pag, x)
#         for (i in seq_len(length(descendant.paths))) {
#             path.i <- descendant.paths[[i]]
#             d <- tail(path.i, 1)
#             avoid <- path.i[2:length(path.i)]
#             # Is there a directed/almost directed path from d to x
#             almost.cycle <- directed.path.exists(pag, d, x, avoid=avoid)
#             if (almost.cycle) {
#                 if (verbose) {
#                     # cat("\nRule 12\nFound ", node_names[x], "o-*", node_names[path.i[2]])
#                     # cat(" o-* ... o-*", node_names[d], "and (almost) directed path from ")
#                     # cat(node_names[d], "to", node_names[x], "\n")
#                     # cat("Marking", node_names[x], "<-*", node_names[path.i[2]], "\n")
#                     print(path.i)
#                     cat("\nRule 12\nFound ", x, "o-*", path.i[2])
#                     cat(" o-* ... o-*", d, "and (almost) directed path from ")
#                     cat(d, "to", x, "\n")
#                     cat("Marking", x, "<-*", path.i[2], "\n")
#                 }
#                 pag[path.i[2], x] <- 2
#             }
#         }
#     }
#     return(pag)
# }
