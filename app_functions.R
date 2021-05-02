rtnorm <- function(n, m, s, lo, hi) {
  u_lo <- pnorm(lo, m, s)
  u_hi <- pnorm(hi, m, s)
  
  u <- runif(n, u_lo, u_hi)
  tn <- qnorm(u, m, s)
  
  if (any(tn == Inf | tn == -Inf)) {
    tn <- ifelse(tn == Inf, 10, tn)
    tn <- ifelse(tn == -Inf, -10, tn)
  }
  
  return(tn)
}