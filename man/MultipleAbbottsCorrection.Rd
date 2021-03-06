% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MultipleAbbottsCorrection.R
\name{MultipleAbbottsCorrection}
\alias{MultipleAbbottsCorrection}
\title{MultipleAbbottsCorrection}
\usage{
MultipleAbbottsCorrection(mortality.data, ci.quantile = 0.95)
}
\arguments{
\item{mortality.data}{A \code{data.frame} or \code{data.table} with the
following columns: \code{sample}, \code{dose} and \code{p} (uncorrected
fractional mortality). Other columns will be ignored.}

\item{ci.quantile}{\code{numeric} specifying the quantile for the confidence
intervals (default 0.95)}
}
\value{
A \code{data.frame} or \code{data.table} (depending on input) with
  the columns \code{sample}, \code{dose}, \code{p.corr} (corrected mortality)
  and \code{ci} (confidence interval at specified \code{ci.quantile}).
}
\description{
Run \code{AbbottsCorrection} on multiple \code{sample}s
}

