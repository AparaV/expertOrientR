# expertOrientR

This is an R package implementing orientation rules for adding expert knowledge to ancestral graphs. See our paper, [Towards Complete Causal Explanation with Expert Knowledge](https://arxiv.org/abs/2407.07338).

## Installation

Install using `devtools`

```{R}
# install.packages("devtools")
# install.packages("rmarkdown")
devtools::install_github("AparaV/expertOrientR", build_vignettes = TRUE)
```

If you run into issues, ensure that the dependency `pcalg` is properly installed. Several packages required by pcalg need to be installed from BioConductor as they are unavailable on CRAN:

```{R}
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("graph")
BiocManager::install("RBGL")
```

## Quick start

See `vignettes/expertOrientR-introduction.html` for usage.

```{R}
vignette("expertOrientR-introduction")
```