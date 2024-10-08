% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RunModel.InputsModel.R
\name{RunModel.InputsModel}
\alias{RunModel.InputsModel}
\title{Wrapper for \link[airGR:RunModel]{airGR::RunModel} for one sub-basin}
\usage{
\method{RunModel}{InputsModel}(x = NULL, RunOptions, Param, FUN_MOD = NULL, InputsModel = NULL, ...)
}
\arguments{
\item{x}{[object of class \emph{InputsModel}] see \link[airGR:CreateInputsModel]{airGR::CreateInputsModel} for details}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link[airGR]{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of model parameters (See details for SD lag model)}

\item{FUN_MOD}{[function] hydrological model function (e.g. \code{\link[airGR]{RunModel_GR4J}}, \code{\link[airGR]{RunModel_CemaNeigeGR4J}})}

\item{InputsModel}{[object of class \emph{InputsModel}] see \code{\link[airGR]{CreateInputsModel}} for details}

\item{...}{Further arguments for compatibility with S3 methods}
}
\value{
[object of class \emph{OutputsModel}] returned by \link[airGR:RunModel]{airGR::RunModel}
(See Value section of \link[airGR:RunModel_GR4J]{airGR::RunModel_GR4J}) completed by new items:
\itemize{
\item \code{Qsim_m3}: simulated flow in cubic meters per time step
\item \code{Qover_m3} volumes of over abstractions which occurs when \code{RunModel_Lag} warns
for negative simulated flows
\item \code{Qnat}: only present in case of Diversion in the node, simulated flow in mm
per time step before application of the Diversion
\item \code{Qdiv_m3}: only present in case of Diversion in the node, simulated diverted flow in
cubic meters per time step. The latter differs from the flows time series provided
in argument \code{Qinf} of \link{CreateInputsModel.GRiwrm} by the limitation of diversion
applied by the minimum flow threshold \code{Qmin} to keep flowing in the river
}
}
\description{
Wrapper for \link[airGR:RunModel]{airGR::RunModel} for one sub-basin
}
