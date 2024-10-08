% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_generators.R
\name{generate_mag_pag}
\alias{generate_mag_pag}
\title{Generate MAG and PAG}
\usage{
generate_mag_pag(size, prob, L_fraction, seed = NA)
}
\arguments{
\item{size}{Number of nodes in the underlying causal DAG.
Note that this is not the number of nodes in the ancestral graphs.}

\item{prob}{Probability of an edge between two nodes in the causal DAG}

\item{L_fraction}{Fraction of source and confounding nodes in the underlying DAG
that should be chosen as latent nodes}

\item{seed}{Random seed. Defaults to \code{NA}}
}
\value{
Named list containing two elements, \code{mag} and \code{pag}.
\code{mag} is the adjacency matrix of the generated MAG.
\code{pag} is the corresponding essential ancestral graph (PAG) for \code{mag}.
}
\description{
Randomly generate a MAG (maximal ancestral graph) and PAG (partial ancestral graph i.e., the essential ancestral graph).
The ancestral graphs are generated by first drawing a topologically ordered random DAG using \code{pcalg::randomDAG}.
Then potential latent nodes are found by identifying all confounders and source nodes.
Some fraction of these potential latent nodes are chosen as latent nodes.
The PAG is generated using \code{pcalg::dag2pag} and the MAG is generated from the PAG by looking at descendants in the DAG.
}
\details{
The adjacency matrices follow the convention in pcalg: 0 refers to no edge. 1 refers to circle edge mark.
2 refers to an arrowhead and 3 refers to a tail. For example, \code{g[a, b] = 2} and \code{g[b, a] = 1}
says that the edge is \verb{a o-> b}.
}
