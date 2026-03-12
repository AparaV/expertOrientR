
#' Complete expert knowledge
#' 
#' Perform orientation on an ancestral partial mixed graph to restrict equivalence class to expert knowledge.
#'
#' @param pag Adjacency matrix of ancestral partial mixed graph.
#'  This is the essential graph with some circle edges oriented as per expert knowledge.
#'  The matrix follows the convention in pcalg: 0 refers to no edge. 1 refers to circle edge mark.
#'  2 refers to an arrowhead and 3 refers to a tail. For example, `pag[a, b] = 2` and `pag[b, a] = 1`
#'  says that the edge is `a o-> b`.
#' @param rules A boolean vector of length 18 indicating which rules are to be invoked
#' @param verbose Boolean indicating verboseness
#' @param debug Boolean indicating verboseness level higher than verbose
#' @returns A list with two named elements, `pag` and `counter`.
#'  `pag` is the adjacency matrix obtained after completing all the rules turned on in the `rules` parameter.
#'  `counter` is a vector of length 18 indicating how many times each rule was invoked.
#' @seealso [expertOrientR::generate_mag_pag()] for format of adjacency matrices
#' @export
complete_pag <- function(pag, rules=NULL, verbose=FALSE, debug=FALSE) {
    if (!is.numeric(pag)) {
        storage.mode(pag) <- "numeric"
    }
    p <- as.numeric(dim(pag)[1])
    num.rules <- 18
    counter <- rep.int(0, num.rules)
    old_pag <- matrix(0, p, p)
    
    if (is.null(rules)) {
        rules <- c(TRUE, TRUE, FALSE, TRUE, FALSE,
                   FALSE, FALSE, TRUE, FALSE, TRUE,
                   FALSE, TRUE, TRUE, TRUE, FALSE,
                   FALSE, FALSE, TRUE)
    }
    
    
    rules_func <- list(
        c("Rule 1", expertOrientR:::rule.1),
        c("Rule 2", expertOrientR:::rule.2),
        c("Rule 3", expertOrientR:::rule.3),
        c("Rule 4", expertOrientR:::rule.4),
        c("Rule 5", expertOrientR:::rule.5),
        c("Rule 6", expertOrientR:::rule.6),
        c("Rule 7", expertOrientR:::rule.7),
        c("Rule 8", expertOrientR:::rule.8),
        c("Rule 9", expertOrientR:::rule.9),
        c("Rule 10", expertOrientR:::rule.10),
        c("Rule 11", expertOrientR:::rule.11),
        c("Rule 12", expertOrientR:::rule.12),
        c("Rule 13", expertOrientR:::rule.13),
        c("Rule 14", expertOrientR:::rule.14),
        c("Rule 15", rule.null),      # Not implemented
        c("Rule 16", rule.null),      # Not implemented
        c("Rule 17", rule.null),      # Not implemented
        c("Rule 18", expertOrientR:::rule.18)
    )
    
    # Rules not implemented: 15, 16, 17
    unimplemented_rules <- c(15, 16, 17)
    for (r in unimplemented_rules) {
        if (rules[r]) {
            cat("Warning: Rule", r, "is not implemented and will be skipped.\n")
            rules[r] <- FALSE
        }
    }
    
    neighbors <- expertOrientR:::find_neighbors(pag)
    
    while (any(old_pag != pag)) {
        
        old_pag <- pag
        
        for (i in seq_len(length(rules))) {
            if (rules[i]) {
                if (debug) {
                    cat("[Debug] Invoking", rules_func[[i]][[1]], "...\n")
                }
                temp <- pag
                
                if (rules_func[[i]][[1]] == "Rule 4") {
                    pag <- rules_func[[i]][2][[1]](pag, neighbors, verbose)
                }
                else {
                    pag <- rules_func[[i]][2][[1]](pag, verbose)
                }
                
                if (any(temp != pag))
                    counter[i] <- counter[i] + 1
            }
        }
        
        
    }
    
    
    output <- list(pag=pag, counter=counter)
    return(output)
}


rule.null <- function(pag, unfVect=NULL, verbose=FALSE) {
    return(pag)
}
