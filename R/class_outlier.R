new_outlier_model <- function(A, sims, mu, sigma, cdf, cms, sms) {
  structure(
    list(
      A     = A,
      sims  = sims,
      mu    = mu,
      sigma = sigma,
      cdf   = cdf,
      cms   = cms,
      sms   = sms
    ),
    class = c("outlier_model", "list")
  )
}

new_novelty <- function(m, dev, pv, cv, a) {
  # m : outlier_model object
  m$dev    <- dev
  m$pval   <- pv
  m$cv     <- cv
  m$alpha  <- a
  class(m) <- c("novelty", class(m))
  return(m)
}

new_outlier <- function(m, cv, a) {
  # m : outlier_model object
  m$cv     <- cv
  m$alpha  <- a
  class(m) <- c("outlier", class(m))
  return(m)
}

new_mixed_outlier <- function(m, dev, pv, cv, a) {
  # m : outlier_model object
  m <- new_novelty(m, dev, pv, cv, a)
  class(m) <- c("mixed_outlier", class(m))
  return(m)
}

convolute <- function(m1, m2) {
  # m1 and m2 : outlier_model objects
  .sims <- m1$sims + m2$sims
  .cdf  <- stats::ecdf(.sims)
  .mu   <- m1$mu + m2$mu
  .sig  <- m1$sigma + m2$sigma
  m     <- new_outlier_model(rbind(m1$A, m2$A), .sims, .mu, .sig, .cdf, NULL, NULL)
  return(m)
}
