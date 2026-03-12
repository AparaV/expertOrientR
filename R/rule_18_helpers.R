#
# Rule 18 Helper Functions
#
# Helper functions for implementing Rule 18 orientation rule for PAGs
#

library(graph)
library(RBGL)

# Source Rule 14 helpers to reuse find_unbridged_paths
if (!exists("find_unbridged_paths")) {
    if (file.exists("rule_14_helpers.R")) {
        source("rule_14_helpers.R")
    } else if (file.exists("orientation_rules/rule_14_helpers.R")) {
        source("orientation_rules/rule_14_helpers.R")
    } else {
        source("Code/orientation_rules/rule_14_helpers.R")
    }
}


#' Find minimal possible directed paths from source to target
#'
#' A "possible directed path" is a path where edges could be oriented
#' to form a directed path. We look for paths following edges that
#' are not blocked by arrowheads pointing away from the path direction.
#'
#' @param pag Adjacency matrix
#' @param source Source vertex (D)
#' @param target Target vertex (A)
#' @param verbose Print debug information
#' @return List of paths, each path is a vector of vertices
find_possible_directed_paths <- function(pag, source, target, verbose = FALSE) {
    # Use BFS to find all minimal paths from source to target
    # A path is "possible directed" if at each step we can follow an edge
    # that could be oriented toward the target
    
    n <- ncol(pag)
    paths <- list()
    
    # BFS with path tracking
    queue <- list(list(path = c(source), visited = rep(FALSE, n)))
    queue[[1]]$visited[source] <- TRUE
    
    while (length(queue) > 0) {
        current <- queue[[1]]
        queue[[1]] <- NULL
        
        current_path <- current$path
        current_vertex <- current_path[length(current_path)]
        
        # If we reached target, save this path
        if (current_vertex == target) {
            paths[[length(paths) + 1]] <- current_path
            next  # Don't extend beyond target (keep paths minimal)
        }
        
        # Find neighbors we can extend to
        # We want edges that could potentially be directed FROM current TOWARD target
        # i.e., avoid edges with arrowhead pointing FROM next TO current
        for (next_vertex in 1:n) {
            if (current$visited[next_vertex]) next
            if (pag[current_vertex, next_vertex] == 0) next  # No edge
            
            # Check if this edge could be part of a directed path toward target
            # Exclude edges where next_vertex --> current_vertex (wrong direction)
            # This means: pag[current_vertex, next_vertex] = 3 (tail at next in edge current-next)
            #         AND pag[next_vertex, current_vertex] = 2 (arrowhead at current in edge next-current)
            # Simplified: exclude if there's an arrowhead at current_vertex
            if (pag[next_vertex, current_vertex] == 2) next  # next --> current (wrong direction)
            
            # Add this path to queue
            new_visited <- current$visited
            new_visited[next_vertex] <- TRUE
            new_path <- c(current_path, next_vertex)
            queue[[length(queue) + 1]] <- list(path = new_path, visited = new_visited)
        }
    }
    
    if (verbose && length(paths) > 0) {
        cat("  Found", length(paths), "possible directed path(s) from", source, "to", target, "\n")
    }
    
    return(paths)
}


#' Find the first vertex after source in a path
#'
#' For path <D, T, ..., A>, extract T
#'
#' @param path Vector of vertices in path
#' @return Second vertex in path (or NULL if path too short)
get_path_second_vertex <- function(path) {
    if (length(path) < 2) return(NULL)
    return(path[2])
}


#' Check if two vertices are adjacent in PAG
#'
#' @param pag Adjacency matrix
#' @param v1 First vertex
#' @param v2 Second vertex
#' @return TRUE if v1 and v2 are adjacent, FALSE otherwise
are_adjacent <- function(pag, v1, v2) {
    return(pag[v1, v2] != 0 || pag[v2, v1] != 0)
}


#' Check if vertex has specific edge pattern to another vertex
#'
#' @param pag Adjacency matrix
#' @param from From vertex
#' @param to To vertex
#' @param pattern Pattern: "o-*" means circle at from, anything at to
#' @return TRUE if pattern matches
check_edge_pattern <- function(pag, from, to, pattern) {
    if (pattern == "o-*") {
        # Circle at from, anything at to (except no edge)
        return(pag[to, from] == 1 && pag[from, to] != 0)
    }
    return(FALSE)
}
