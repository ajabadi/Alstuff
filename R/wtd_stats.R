#' Wrapper for Hmisc::wtd.stats
#'
#' A detailed description
#' @author Frank Harrell , Benjamin Tyner
#' @title weighted variance with NA's
#' @param x numeric vector possibly with missing values
#' @param weights numeric vector pof weights possibly with missing values
#' @import stats
#' @importFrom stats cov.wt
#' @export wtd_var
#' @export wtd_mean
#' @references \url{https://github.com/harrelfe/Hmisc/blob/master/man/wtd.stats.Rd}
#' @family stats
### Weighted Variance
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

### Weighted Mean
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
