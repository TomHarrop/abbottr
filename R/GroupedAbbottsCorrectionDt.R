#' GroupedAbbottsCorrectionDt
#'
#' \code{data.table} back-end to run groupued \code{AbbottsCorrection}s for
#' multiple samples. \strong{This corrects to \code{dose == 0}, should it be
#' possible to specify the base level? Add details}.
#'
#' @param mortality.data A \code{data.table} with colums \code{sample},
#'   \code{dose} and \code{p} (uncorrected fractional mortality).
#' @param sample.group The group for this correction
#' @param ci.quantile \code{numeric} specifying the quantile for the confidence
#'   intervals (default 0.95)
#'
#' @return \code{data.table} with colums \code{sample}, \code{dose},
#'   \code{p.corr} (corrected mortality) and \code{ci} (confidence interval at
#'   specified \code{ci.quantile})
#'
#' @import data.table
#'

GroupedAbbottsCorrectionDt <- function(mortality.data,
                                       sample.group,
                                       ci.quantile = 0.95){
  control.results <- mortality.data[sample == sample.group & dose == 0, p]
  mortality.data[sample == sample.group,
                 AbbottsCorrection(control.results, p, ci.quantile),
                 by = dose]
}
