# Package index

## Input helpers

Create a ScMaSigPro object from a SingleCellExperiment or from counts
and a cell table, and build the pseudotime and path labels of the cells
from the output of a trajectory tool.

- [`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md)
  : Create a ScMaSigPro object from a SingleCellExperiment
- [`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md)
  : Create a ScMaSigPro object from counts and cell metadata
- [`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md)
  : Assign cells to paths from lineage weights
- [`paths_from_graph()`](https://spriyansh.github.io/scMaSigPro/reference/paths_from_graph.md)
  : Lineage weights from a trajectory graph

## Workflow steps

Bin the cells along pseudotime, set up the polynomial model, test and
fit each gene, list the significant genes and cluster their trends.

- [`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)
  : Bin the cells of each path along pseudotime
- [`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md)
  : Restructure the bins of paths that share a root
- [`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md)
  : Set up the polynomial design
- [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
  : Test the full model of each gene
- [`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
  : Select the significant terms of each gene
- [`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md)
  : List the significant genes
- [`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md)
  : Cluster the significant genes

## Binning helpers

Compute the pseudo-bulk counts of the bins.

- [`pb_counts()`](https://spriyansh.github.io/scMaSigPro/reference/pb_counts.md)
  : Pseudo-bulk counts of the bins

## Plots

Plot the bins, the trends of genes and clusters, and the gene lists.

- [`plotBinTile()`](https://spriyansh.github.io/scMaSigPro/reference/plotBinTile.md)
  : Plot the sizes of the bins
- [`plotTrend()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrend.md)
  : Plot the trend of a gene
- [`plotTrendCluster()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrendCluster.md)
  : Plot the trends of the clusters
- [`plotIntersect()`](https://spriyansh.github.io/scMaSigPro/reference/plotIntersect.md)
  : Plot the overlaps of the gene lists
- [`plotDiagnostics()`](https://spriyansh.github.io/scMaSigPro/reference/plotDiagnostics.md)
  : Plot the diagnostics of the model of a gene

## Results

Read the results of the fits and the settings of an analysis.

- [`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md)
  [`scmpToRowData()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md)
  : Per-gene results of an analysis
- [`showCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)
  [`showInflu()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)
  [`showTS()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)
  [`showSol()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)
  [`showSigProf()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)
  [`showGroupCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)
  : Results of the term selection
- [`showPoly()`](https://spriyansh.github.io/scMaSigPro/reference/showParams.md)
  [`showParams()`](https://spriyansh.github.io/scMaSigPro/reference/showParams.md)
  : Parameters and model formula of an analysis
- [`queryCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/queryCoeff.md)
  : Query the coefficients of the selected models

## Accessors

Read and replace the data of the cells, the bins and the design.

- [`cSparse()`](https://spriyansh.github.io/scMaSigPro/reference/cSparse.md)
  [`` `cSparse<-`() ``](https://spriyansh.github.io/scMaSigPro/reference/cSparse.md)
  : Cell metadata of the Sparse slot
- [`eSparse()`](https://spriyansh.github.io/scMaSigPro/reference/eSparse.md)
  [`` `eSparse<-`() ``](https://spriyansh.github.io/scMaSigPro/reference/eSparse.md)
  : Cell counts of the Sparse slot
- [`cDense()`](https://spriyansh.github.io/scMaSigPro/reference/cDense.md)
  [`` `cDense<-`() ``](https://spriyansh.github.io/scMaSigPro/reference/cDense.md)
  : Bin metadata of the Dense slot
- [`eDense()`](https://spriyansh.github.io/scMaSigPro/reference/eDense.md)
  [`` `eDense<-`() ``](https://spriyansh.github.io/scMaSigPro/reference/eDense.md)
  : Pseudo-bulk counts of the Dense slot
- [`predictors()`](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)
  [`` `predictors<-`() ``](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)
  : Design matrix of the polynomial model
- [`pathAssign()`](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md)
  [`` `pathAssign<-`() ``](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md)
  : Path assignment matrix of the design

## Classes

The ScMaSigPro class, the classes of its slots and their update.

- [`ScMaSigPro`](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  [`ScMaSigPro-class`](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  : ScMaSigPro
- [`MatrixDesign`](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md)
  [`MatrixDesign-class`](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md)
  : MatrixDesign
- [`VariableProfiles`](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md)
  [`VariableProfiles-class`](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md)
  : VariableProfiles
- [`Estimates`](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md)
  [`Estimates-class`](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md)
  : Estimates
- [`Significant`](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md)
  [`Significant-class`](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md)
  : Significant
- [`ParameterConfig`](https://spriyansh.github.io/scMaSigPro/reference/ParameterConfig-class.md)
  [`ParameterConfig-class`](https://spriyansh.github.io/scMaSigPro/reference/ParameterConfig-class.md)
  : ParameterConfig
- [`updateObject(`*`<ScMaSigPro>`*`)`](https://spriyansh.github.io/scMaSigPro/reference/updateObject-ScMaSigPro-method.md)
  : Update an ScMaSigPro object to the current class definition

## Datasets

Data sets bundled with the package.

- [`splat.sim`](https://spriyansh.github.io/scMaSigPro/reference/splat.sim.md)
  : Simulated bifurcating trajectory
- [`scmp.ob`](https://spriyansh.github.io/scMaSigPro/reference/scmp.ob.md)
  : A complete analysis of splat.sim
- [`multi.lin.sce`](https://spriyansh.github.io/scMaSigPro/reference/multi.lin.sce.md)
  : Simulated multifurcating trajectory
