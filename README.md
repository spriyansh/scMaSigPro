<img src="man/figures/logo.png" alt="scMaSigPro Logo" width="17%"/>

### Implementation of MaSigPro for scRNA-Seq Data.

[![R-CMD-Check](https://github.com/BioBam/scMaSigPro/actions/workflows/r.yml/badge.svg?branch=main)](https://github.com/BioBam/scMaSigPro/actions/workflows/r.yml)
[![test-coverage](https://github.com/BioBam/scMaSigPro/actions/workflows/test-coverage.yaml/badge.svg?branch=main)](https://github.com/BioBam/scMaSigPro/actions/workflows/test-coverage.yaml)

---

## Introduction

`scMaSigPro` is an R package designed for analyzing single-cell RNA-seq data over pseudotime. Building on the [maSigPro](https://www.bioconductor.org/packages/release/bioc/html/maSigPro.html) package, it identifies genes with significant expression changes across branching paths in a pseudotime-ordered dataset. This guide provides a step-by-step workflow for ScMaSigPro, making it accessible for users.

## Installation

### Bioconductor and Dependencies
```
# Install Dependencies
if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")
BiocManager::install(version = "3.14")

BiocManager::install(c('SingleCellExperiment', 'maSigPro', 'MatrixGenerics', 'S4Vectors'))
```

### scMaSigPro latest version
To install `scMaSigPro` from GitHub, use the following R code:

```
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
}

# Install scMaSigPro
devtools::install_github("BioBam/scMaSigPro",
                         ref = "main",
                         build_vignettes = FALSE,
                         build_manual = TRUE,
                         upgrade = "never",
                         force = TRUE,
                         quiet = TRUE)
```

---

## Basic Usage
The basic workflow of `scMaSigPro` involves the following steps:

### 1. Load Package and Dataset
```
set.seed(123)
library(scMaSigPro)
data("splat.sim", package = "scMaSigPro")
```

### 2. Create `scMaSigPro` Object

```
# Helper Function to convert annotated SCE object to scmpObject
scmp.ob <- as_scmp(
  object = splat.sim, from = "sce",
  align_pseudotime = FALSE,
  verbose = TRUE,
  additional_params = list(
    labels_exist = TRUE,
    exist_ptime_col = "Step",
    exist_path_col = "Group"
  )
)
```

### 3. Pseudo-bulking with `sc.squeeze()`

This function discretizes a continuous pseudotime column into bins:

```
scmp.ob <- sc.squeeze(scmp.ob)
```

### 4. Setting up the Polynomial Model

```
scmp.ob <- sc.set.poly(scmp.ob)
```

### 5. Detecting Genes with Non-Flat Profiles

```
scmp.ob <- sc.p.vector(scmp.ob)
```

### 6. Model Refinement

```
scmp.ob <- sc.t.fit(scmp.ob)
```

### 7. Gene selection with $R^2$

```
scmp.ob <- sc.filter(scmp.ob, vars = "all")
```

### 8. Gene Trend Visualization

```
plotTrend(scmp.ob, "Gene10")
```

For detailed instructions and additional steps, please refer to the following vignettes.

- [Basic Workflow](https://www.metapriyansh.com/scMaSigPro/package_vignette/Basic-Workflow.html)
- [maSigPro Analysis](https://www.metapriyansh.com/scMaSigPro/package_vignette/scMaSigPro-maSigPro.html) 
- [scMaSigPro Class](https://www.metapriyansh.com/scMaSigPro/package_vignette/scMaSigPro-Class.html) 

## Contributing
Contributions, including bug reports, suggestions, and pull requests, are welcome.

## License
This project is licensed under GPL>=2 - see the LICENSE.md file for details.

## Funding Information 
This project has received funding from the European Union’s Framework Programme for Research and Innovation Horizon 2020 (2014-2020) under the Marie Skłodowska-Curie Grant Agreement No 953407.

## Citation
If you use `scMaSigPro` in your research, please cite:

Priyansh Srivastava, Marta Benegas Coll, Stefan Götz, María José Nueda, Ana Conesa, "scMaSigPro: differential expression analysis along single-cell trajectories"", Bioinformatics, Volume 40, Issue 7, July 2024, btae443, [https://doi.org/10.1093/bioinformatics/btae443](https://doi.org/10.1093/bioinformatics/btae443)

---
