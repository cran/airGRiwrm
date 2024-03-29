% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RunModel.SD.R
\name{RunModel.SD}
\alias{RunModel.SD}
\title{Run a semi-distributed model from rainfall-runoff model outputs}
\usage{
\method{RunModel}{SD}(x, RunOptions, Param, QcontribDown, ...)
}
\arguments{
\item{x}{[object of class \code{InputsModel}] used as \code{InputsModel} parameter for \link[airGR:RunModel_Lag]{airGR::RunModel_Lag}}

\item{RunOptions}{[object of class \emph{RunOptions}] see \code{\link[airGR]{CreateRunOptions}} for details}

\item{Param}{[numeric] vector of 1 parameter
    \tabular{ll}{
      Velocity \tab mean flow velocity [m/s]
    }
  }

\item{QcontribDown}{[numeric] vector or [OutputsModel] containing the time series of the runoff contribution of the downstream sub-basin}

\item{...}{further arguments passed to or from other methods}
}
\value{
\code{OutputsModel} object. See \link[airGR:RunModel_Lag]{airGR::RunModel_Lag}
}
\description{
Run a semi-distributed model from rainfall-runoff model outputs
}
