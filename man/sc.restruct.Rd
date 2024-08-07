% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sc.restruct.R
\name{sc.restruct}
\alias{sc.restruct}
\title{Restructure the binned data.}
\usage{
sc.restruct(
  scmpObj,
  end_node_list,
  root_node,
  link_node_list,
  assay_name = "counts",
  aggregate = "sum",
  link_sep = "_links_",
  verbose = TRUE
)
}
\arguments{
\item{scmpObj}{An object of class \code{\link{ScMaSigPro}}.
path assignment in 'Sparse' or 'Dense' data.}

\item{end_node_list}{A list of end nodes in of the branch.}

\item{root_node}{A character string specifying the root node.}

\item{link_node_list}{A list of links between two nodes.}

\item{assay_name}{Name of the Assay in sparse data from which the counts are
used. (Default = "counts").}

\item{aggregate}{A character string specifying the method to aggregate counts
within each cluster. Available options are 'mean' or 'sum'. (Default = "sum").}

\item{link_sep}{A character string to separate the link nodes. (Default = "_links_")}

\item{verbose}{Print detailed output in the console. (Default is TRUE)}
}
\value{
An object of class \code{\link{ScMaSigPro}}, with updated `Dense`
slot.
}
\description{
`sc.restruct()` Add Description
}
\seealso{
\code{\link{estBinSize}}, \code{\link{discretize}},
\code{\link{create_range}}
}
\author{
Priyansh Srivastava \email{spriyansh29@gmail.com}
}
