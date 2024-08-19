# library(graph)

# source("orientation_rules/rule_01.R")
# source("orientation_rules/rule_02.R")
# source("orientation_rules/rule_03.R")
# source("orientation_rules/rule_04.R")
# source("orientation_rules/rule_05.R")
# source("orientation_rules/rule_06.R")
# source("orientation_rules/rule_07.R")
# source("orientation_rules/rule_08.R")
# source("orientation_rules/rule_09.R")
# source("orientation_rules/rule_10.R")
# source("orientation_rules/rule_11.R")
# source("orientation_rules/rule_12.R")
# source("orientation_rules/rule_13.R")


# 0: No edge
# 1: Circle
# 2: Arrowhead
# 3: Tail
# amat[a,b] = 2  and  amat[b,a] = 3   implies   a --> b.
# The above notation is different from cpdags...


#' @export
complete_pag <- function(pag, sepset=NULL, unfVect=NULL, rules=rep(TRUE, 13),
                          verbose=FALSE, debug=FALSE) {
    if (!is.numeric(pag))
        storage.mode(pag) <- "numeric"
    p <- as.numeric(dim(pag)[1])
    num.rules <- 13
    counter <- rep.int(0, 13)
    old_pag <- matrix(0, p, p)
    
    
    rules_func <- list(
        c("Rule 1", expertOrientR:::rule.1),
        c("Rule 2", expertOrientR:::rule.2),
        c("Rule 3", expertOrientR:::rule.3),
        c("Rule 4", expertOrientR:::rule.4.fast),
        c("Rule 5", expertOrientR:::rule.5),
        c("Rule 6", expertOrientR:::rule.6),
        c("Rule 7", expertOrientR:::rule.7),
        c("Rule 8", expertOrientR:::rule.8),
        c("Rule 9", expertOrientR:::rule.9),
        c("Rule 10", expertOrientR:::rule.10),
        c("Rule 11", expertOrientR:::rule.11),
        c("Rule 12", expertOrientR:::rule.12),
        c("Rule 13", expertOrientR:::rule_13)
    )
    
    
    rule_times <- rep_len(0, 13)
    
    neighbors <- expertOrientR:::find_neighbors(pag)
    
    while (any(old_pag != pag)) {
        
        old_pag <- pag
        
        for (i in seq_len(length(rules))) {
            if (rules[i]) {
                if (debug)
                    cat("[Debug] Invoking", rules_func[[i]][[1]], "...\n")
                temp <- pag
                
                if (rules_func[[i]][[1]] == "Rule 4") {
                    pag <- rules_func[[i]][2][[1]](pag, neighbors, unfVect, verbose)
                }
                else {
                    pag <- rules_func[[i]][2][[1]](pag, unfVect, verbose)
                }
                
                if (any(temp != pag))
                    counter[i] <- counter[i] + 1
            }
        }
        
        
    }
    
    
    output <- list(pag=pag, counter=counter)
    return(output)
}
