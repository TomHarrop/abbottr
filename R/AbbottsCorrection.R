#' AbbottsCorrection
#'
#' Perform Abbott's correction \strong{ref???}. \strong{add details}.
#'
#' @param control.mortality Vector of fractional mortality in control group
#' @param experimental.mortality Vector of fractional mortality in treated group
#' @param ci.quantile \code{numeric} specifying the quantile for the confidence
#'   intervals (default 0.95)
#'
#' @return Returns a named list of corrected mortality (\code{p.corr}) and
#'   confidence interval (\code{ci}) at specified \code{ci.quantile}
#'
#' @export

AbbottsCorrection <- function(control.mortality,
                              experimental.mortality,
                              ci.quantile = 0.95){
  # calculate summary statistics
  n.cont <- sum(!is.na(control.mortality))
  p.cont <- mean(control.mortality, na.rm = TRUE)
  v.cont <- var(control.mortality, na.rm = TRUE)

  n.expt <- sum(!is.na(experimental.mortality))
  p.expt <- mean(experimental.mortality, na.rm = TRUE)
  v.expt <- var(experimental.mortality, na.rm = TRUE)

  # degrees of freedom and t-value
  df <- min(n.cont, n.expt) - 1
  t <- qt(ci.quantile, df)

  # g
  g <- (v.cont * (t^2)) / (((1 - p.cont)^2) * n.cont)

  # corrected mortality (p)
  p.corr <- 1 - ((1 - p.expt) / ((1 - p.cont) / (1 - g)))

  # confidence intervals (eqn 5)
  x1 <- t / ((1 - p.cont) / (1 - g))
  x2 <- (1 - g) * (v.expt / n.expt)
  x3 <- (((1 - p.expt)^2) * v.cont) / (((1 - p.cont)^2) * n.cont)

  ci <- x1 * ((x2 + x3)^0.5)

  # return
  list(p.corr = p.corr, ci = ci)
}
