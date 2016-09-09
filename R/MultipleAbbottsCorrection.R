#' MultipleAbbottsCorrection
#'
#' Run \code{AbbottsCorrection} on multiple \code{sample}s
#'
#' @param mortality.data A \code{data.frame} or \code{data.table} with the
#'   following columns: \code{sample}, \code{dose} and \code{p} (uncorrected
#'   fractional mortality). Other columns will be ignored.
#' @param ci.quantile \code{numeric} specifying the quantile for the confidence
#'   intervals (default 0.95)
#'
#' @return A \code{data.frame} or \code{data.table} (depending on input) with
#'   the columns \code{sample}, \code{dose}, \code{p.corr} (corrected mortality)
#'   and \code{ci} (confidence interval at specified \code{ci.quantile}).
#'
#' @import data.table
#' @export

MultipleAbbottsCorrection <- function(mortality.data, ci.quantile = 0.95) {
  if(class(mortality.data)[[1]] == "data.table") {
    return.dt <- TRUE
    my.mortality.data <- data.table::copy(mortality.data)
  } else {
    my.mortality.data <- data.table::data.table(mortality.data)
  }

  # call GroupedAbbottsCorrectionDt by sample
  corrected.dt <- my.mortality.data[, GroupedAbbottsCorrectionDt(
    mortality.data = my.mortality.data,
    sample.group = sample,
    ci.quantile = ci.quantile
  ), by = sample]

  if (!return.dt) {
    return(data.frame(corrected.dt))
  } else {
    return(corrected.dt)
  }

}
