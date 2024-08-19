# library(pooh)

# source("orientation_rules/helpers.R")


# 0: No edge
# 1: Circle
# 2: Arrowhead
# 3: Tail
# amat[a,b] = 2  and  amat[b,a] = 3   implies   a --> b.
# The above notation is different from cpdags...

#' @export
get_partial_edges <- function(pag) {
    # Returns n x 2 matrix of edges (A, B) such that A o-> B
    indices <- which(pag == 1, arr.ind=T)
    edges_list <- c()
    for (i in seq_len(nrow(indices))) {
        b <- indices[i, 1]
        a <- indices[i, 2]
        if (pag[a, b] == 2) {
            edges_list <- c(edges_list, a, b)
        }
    }
    if (length(edges_list) > 0) {
        edges <- t(matrix(edges_list, nrow=2))
    }
    else {
        edges <- matrix(nrow=0, ncol=2)
    }
    colnames(edges) <- c("A", "B")
    return(edges)
}


get_circle_component <- function(g) {
    ccomp <- which(g == 1 & t(g) == 1, arr.ind=T)
    colnames(ccomp) <- c("A", "B")
    return(ccomp)
}


filter_partial_edges <- function(g, partial_edges) {
    edges_list <- c()
    for (i in seq_len(nrow(partial_edges))) {
        a <- partial_edges[i, 1]
        b <- partial_edges[i, 2]
        if (g[b, a] == 1 && g[a, b] == 2) {
            edges_list <- c(edges_list, a, b)
        }
    }
    if (length(edges_list) > 0) {
        edges <- t(matrix(edges_list, nrow=2))
    }
    else {
        edges <- matrix(nrow=0, ncol=2)
    }
    colnames(edges) <- c("A", "B")
    return(edges)
}


filter_circle_component <- function(g, ccomp_0) {
    edges_list <- c()
    for (i in seq_len(nrow(ccomp_0))) {
        a <- ccomp_0[i, 1]
        b <- ccomp_0[i, 2]
        if (g[b, a] == 1) {
            edges_list <- c(edges_list, a, b)
        }
    }
    if (length(edges_list) > 0) {
        edges <- t(matrix(edges_list, nrow=2))
    }
    else {
        edges <- matrix(nrow=0, ncol=2)
    }
    colnames(edges) <- c("A", "B")
    return(edges)
}


enumerate_possibilities <- function(partial_edges) {
    possibilities <- matrix(nrow=nrow(partial_edges)*2, ncol=3)
    colnames(possibilities) <- c("A", "B", "A_mark")
    for (i in seq_len(nrow(partial_edges))) {
        a <- partial_edges[i, 1]
        b <- partial_edges[i, 2]
        j <- 2*i - 1
        possibilities[j,] <- c(a, b, 3) # A --> B
        possibilities[j+1,] <- c(a, b, 2) # A <-> B
    }
    return(possibilities)
}

find_unshielded_colliders <- function(g, list=FALSE) {
    n <- nrow(g)
    num_triples <- n*(n-1)*(n-2)
    uc <- matrix(nrow=num_triples, ncol=3)
    counter <- 0
    ind <- which((g == 2 & t(g) != 0), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        a <- ind[i, 1]
        b <- ind[i, 2]
        indC <- which((g[b, ] != 0 & g[, b] == 2) & (g[a, ] == 0 & g[, a] == 0))
        indC <- setdiff(indC, a)
        for (j in seq_len(length(indC))) {
            counter <- counter + 1
            c <- indC[j]
            if (a < c) {
                uc[counter, ] <- c(a, b, c)
            }
            else {
                uc[counter, ] <- c(c, b, a)
            }
        }
    }
    if (counter > 0) {
        uc <- uc[1:counter, ]
        uc <- unique(uc)
    }
    else {
        uc <- matrix(nrow=0, ncol=3)
    }
    
    if (nrow(uc) > 1) {
        uc <- uc[order(uc[,1], uc[,2], uc[,3]), ]
    }
    
    colnames(uc) <- c("A", "B", "C")
    
    if (list) {
        uc <- lapply(seq_len(nrow(uc)), function(i) uc[i,])
    }
    
    return(uc)
}


find_inferred_orientations <- function(g, orientations_l, verbose=FALSE) {
    inferred_orientations_indices <- c()
    for (i in seq_len(length(orientations_l))) {
        a <- orientations_l[[i]][1]
        b <- orientations_l[[i]][2]
        a_edge_mark <- orientations_l[[i]][3]
        if (g[b, a] == a_edge_mark) {
            inferred_orientations_indices <- c(inferred_orientations_indices, i)
            if (verbose) {
                cat("\n\tInferred edge", a, b, a_edge_mark, "\n")
            }
        }
    }
    return(inferred_orientations_indices)
}


cycle_length_3 <- function(g) {
    ind <- which((g == 2 & t(g) == 3), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        a <- ind[i, 1]
        b <- ind[i, 2]
        indC <- which((g[b, ] != 2 & g[, b] == 3) & (g[a, ] == 3 & g[, a] == 2))
        if (length(indC) != 0) {
            return(TRUE)
        }
    }
    return(FALSE)
}


almost_cycle_length_3 <- function(g) {
    ind <- which((g == 2 & t(g) == 2), arr.ind = TRUE)
    for (i in seq_len(nrow(ind))) {
        a <- ind[i, 1]
        b <- ind[i, 2]
        indC <- which((g[b, ] != 2 & g[, b] == 3) & (g[a, ] == 3 & g[, a] == 2))
        if (length(indC) != 0) {
            return(TRUE)
        }
    }
    return(FALSE)
}


is_ancestral <- function(g) {
    return(!almost_cycle_length_3(g) && !cycle_length_3(g))
}


find_parents <- function(g) {
    pa <- list()
    n <- nrow(g)
    for (x in seq_len(n)) {
        pa_x <- which(g[, x] == 2 & g[x, ] == 3, arr.ind=T)
        pa <- c(pa, list(pa_x))
    }
    return(pa)
}


find_siblings <- function(g) {
    si <- list()
    n <- nrow(g)
    for (x in seq_len(n)) {
        si_x <- which(g[, x] == 2 & g[x, ] == 2, arr.ind=T)
        si <- c(si, list(si_x))
    }
    return(si)
}


get_sibling_pairs <- function(siblings) {
    pairs <- c()
    for (x in seq_len(length(siblings))) {
        for (y in seq_len(length(siblings[[x]]))) {
            pairs <- c(pairs, x, siblings[[x]][y])
        }
    }
    if (length(pairs) > 0) {
        pairs <- matrix(pairs, ncol=2, byrow=TRUE)
    }
    return(pairs)
}


get_districts <- function(si_pairs) {
    districts <- list()
    if (length(si_pairs) > 0) {
        districts <- pooh::weak(si_pairs[, 1], si_pairs[, 2])
    }
    return(districts)
}


subset_districts <- function(districts, pa_y) {
    districts_y <- list()
    for (i in seq_len(length(districts))) {
        d <- districts[[i]]
        if (length(intersect(pa_y, d)) > 0) {
            districts_y <- c(districts_y, list(d))
        }
    }
    return(districts_y)
}


find_neighbors <- function(g) {
    ne <- list()
    n <- nrow(g)
    for (x in seq_len(n)) {
        ne_x <- which(g[, x] != 0 & g[x, ] != 0, arr.ind=T)
        ne <- c(ne, list(ne_x))
    }
    return(ne)
}


discr_path_equiv_helper <- function(g1, g2, ne_g1, pa_g1, si_g1, dist_g1) {
    n <- nrow(g1)
    for (y in seq_len(n)) {
        ne_y <- ne_g1[[y]]
        pa_y <- pa_g1[[y]]
        si_y <- si_g1[[y]]
        districts_y <- subset_districts(dist_g1, pa_y)
        for (i in seq_len(length(districts_y))) {
            d <- districts_y[[i]]
            pa_d <- c()
            si_d <- c()
            for (x in seq_len(length(d))) {
                pa_d <- c(pa_d, pa_g1[[x]])
                si_d <- c(si_d, si_g1[[x]])
            }
            pa_si_d <- union(pa_d, si_d)
            if (length(setdiff(pa_si_d, pa_y)) == 0) {
                next
            }
            si_d_y <- intersect(si_d, si_y)
            for (j in seq_len(length(si_d_y))) {
                b <- si_d_y[j]
                if (g2[b, y] == 2 && g2[y, b] == 3) {
                    return(FALSE)
                }
            }
        }
    }
    return(TRUE)
}


is_discr_path_equivalent <- function(g, mag, ne_mag, pa_mag, si_mag, dist_mag) {
    ne_g <- find_neighbors(g)
    pa_g <- find_parents(g)
    si_g <- find_siblings(g)
    si_pairs_g <- get_sibling_pairs(si_g)
    dist_g <- get_districts(si_pairs_g)
    g_mag_equiv <- discr_path_equiv_helper(g, mag, ne_g, pa_g, si_g, dist_g)
    if (g_mag_equiv) {
        mag_g_equiv <- discr_path_equiv_helper(mag, g, ne_mag, pa_mag, si_mag, dist_mag)
        return(mag_g_equiv)
    }
    return(FALSE)
}
