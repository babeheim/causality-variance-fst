
covd <- function(x, y, q = NULL) {
  if (is.null(q)) q <- rep(1, length(x))/length(x)
  sum(q * (x - sum(q * x, na.rm = TRUE)) * (y - sum(q * y, na.rm = TRUE)), na.rm = TRUE)
}
vard <- function(x, q = NULL) {
  if (is.null(q)) q <- rep(1, length(x))/length(x)
  sum(q * (x - sum(q * x, na.rm = TRUE))^2, na.rm = TRUE)
}

simplex <- function(x) x/sum(x)
softmax <- function(x) simplex(exp(x))

pay <- function(x, p, m, n, k, wt) {
  wt + n * (p - k) * (1 - x) + m * (p - k) * x
}

sample_safe <- function(x, ...) {
  x[sample.int(length(x), ...)]
}

calc_cooperation_deltas <- function(b, c, px = 100) {
  Fsts <- seq(0, 1, length.out = px + 1)
  mus <- seq(0, 1, length.out = px + 1)
  pars <- expand.grid(Fst = Fsts, mu = mus)
  pars$btw <- pars$mu * (1 - pars$mu) * pars$Fst *  b
  pars$win <- -pars$mu * (1 - pars$mu) * c
  pars$total <- pars$btw + pars$win
  return(pars)
}

calc_coordination_deltas <- function(m, n, k = 0.5, px = 100) {
  Fsts <- seq(0, 1, length.out = px + 1)
  mus <- seq(0, 1, length.out = px + 1)
  wt <- 0
  phi <- n/(n-m)
  pars <- expand.grid(Fst = Fsts, mu = mus)
  pars$total <- -(n-m) * pars$mu * (1 - pars$mu) * ((pars$mu - k) + pars$Fst * (1 - (pars$mu + phi)))
  return(pars)
}
