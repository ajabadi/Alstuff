#' Simulate matrix of single-cell gene expression counts
#'
#' Samples from a negative binomial distribution where log mean expression of
#' genes are uniformly distributed. If \code{nhvgs} is an integer, the dispersion
#' for \code{nhvgs} genes is increased by dividing \code{nsuccess} by 5.
#' @param ncells NUmber of cells
#' @param ngenes Number of genes
#' @param nhvgs Number of highly variable genes
#' @param nsuccess Number of success in NB, proportional to the
#'  inverse of dispersion
#' @param log_mu_lim Log mean expression range
#'
#' @return A matrix of gene expression
#' @export
#' @examples
#' View(simulate_genexp())
#' matrixStats::rowVars(simulate_genexp()) %>% hist()
#' ## 100 HVGs
#' matrixStats::rowVars(simulate_genexp(nhvgs = 100)) %>% hist()
simulate_sc_genexp <-
  function(ncells = 50,
           ngenes = 500,
           nhvgs = NULL,
           log_mu_lim = c(-1, 5),
           nsuccess = 10,
           hvgs_nsuccess = NULL)
  {
    mu <- 2^runif(ngenes, log_mu_lim[1], log_mu_lim[2])

    y <- matrix(rnbinom(ncells*length(mu), mu=mu, size= nsuccess), nrow=length(mu))

    if (!is.null(nhvgs)) {
      hvgs_nsuccess <- ifelse(is.null(hvgs_nsuccess), ceiling(nsuccess/5), hvgs_nsuccess)
      y[seq_len(nhvgs),] <- matrix(rnbinom(ncells*nhvgs, mu=mu[seq_len(nhvgs)], size= hvgs_nsuccess), nrow=nhvgs)
    }
    rownames(y) <- sprintf("GENE_%i", seq_along(mu))
    colnames(y) <- sprintf("CELL_%i", seq_len(ncells))
    return(y)
  }
