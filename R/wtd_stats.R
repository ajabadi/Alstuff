#' Wrapper for Hmisc::wtd.stats
#'
#' @title Weighted stats
#' @author Frank Harrell , Benjamin Tyner
#' @param x numeric vector possibly with missing values
#' @param weights numeric vector pof weights possibly with missing values
#' @param normwt normalise weights?
#' @param na.rm na.rm?
#' @param method one of c("unbiased", "ML")
#' @import stats
#' @importFrom stats cov.wt
#' @references \url{https://github.com/harrelfe/Hmisc/blob/master/man/wtd.stats.Rd}
#' @family stats
#' @return a numeric scalar
#'
#'@rdname wtd_stats
#' @export
wtd_var <- function (x, weights = NULL, normwt = FALSE, na.rm = TRUE, method = c("unbiased",
                                                                                 "ML"))
{
  method <- match.arg(method)
  if (!length(weights)) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(var(x))
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  if (normwt)
    weights <- weights * length(x)/sum(weights)
  if (normwt || method == "ML")
    return(as.numeric(stats::cov.wt(cbind(x), weights, method = method)$cov))
  sw <- sum(weights)
  if (sw <= 1)
    warning("only one effective observation; variance estimate undefined")
  xbar <- sum(weights * x)/sw
  return(sum(weights * ((x - xbar)^2))/(sw - 1))
}

#'@rdname wtd_stats
#' @export
wtd_mean <- function (x, weights = NULL, normwt = "ignored", na.rm = TRUE)
{
  if (!length(weights))
    return(mean(x, na.rm = na.rm))
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  return(sum(weights * x)/sum(weights))
}

#'@rdname wtd_stats
#'@export
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
