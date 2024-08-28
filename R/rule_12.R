#
# Rule 12
#

rule.12 <- function(pag, verbose=FALSE) {
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
            if (almost.cycle) {
                if (verbose) {
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
