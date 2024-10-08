% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getNodeRanking.R
\name{getNodeRanking}
\alias{getNodeRanking}
\title{Sorting of the nodes from upstream to downstream for RunModel and Calibration}
\usage{
getNodeRanking(griwrm)
}
\arguments{
\item{griwrm}{[object of class \code{GRiwrm}] see \link{CreateGRiwrm} for details}
}
\value{
A \link{character} \link{vector} containing ordered node ids
}
\description{
Sorting of the nodes from upstream to downstream for RunModel and Calibration
}
\details{
The sort is done by searching upstream nodes in the networks recursively.
Ungauged node clusters are processed by cluster and the algorithm tries to
process ungauged nodes which receive their parameters from upstream or
sibling node after their donor node.
Use \code{options(debug = TRUE)} to get details on how the sort is performed.
}
\seealso{
\code{\link[=CreateGRiwrm]{CreateGRiwrm()}}
}
