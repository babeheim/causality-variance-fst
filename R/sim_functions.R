
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



sim_coordination_deltas <- function(n_groups = 5000, px = 100, game = "the void") {

  Fsts <- seq(0, 1, length.out = px + 1)
  mus <- seq(0, 1, length.out = px + 1)

  wt <- 0
  k <- 0.5
  m <- 5

  if (game == "the void") {
    m <- abs(m)
    n <- 0
  } else if (game == "stag hunt") {
    m <- abs(m)
    n <- 0
  } else if (game == "pure coordination") {
    m <- abs(m)
    n <- -m * (1 - k)/k
  } else if (game == "hawk dove") {
    m <- abs(m)
    n <- m * (2-k)/(1-k)
  } else if (game == "commensalism") {
    n <- abs(m)
    m <- 0
  } else if (game == "invisible hand") {
    m <- (-1) * abs(m)
    n <- -m * k/(1-k)
  } else {
    stop("invalid game")
  }

  phi <- n/(n-m)

  pars <- expand.grid(Fst = Fsts, mu = mus)
  pars$btw <- NA
  pars$win <- NA
  pars$total <- NA

  for (j in 1:nrow(pars)) {
    Fst <- pars$Fst[j]
    mu <- pars$mu[j]
    if (Fst != 0) {
      theta <- (1 - Fst)/Fst
    } else {
      theta <- 0
    }
    x_group <- rbeta(n_groups, mu * theta, (1-mu) * theta)
    w_group <- wt + n * (x_group - k) + x_group * (m-n) * (x_group - k)
    cov_w_x_group <- rep(NA, length(n_groups))
    for (i in 1:n_groups) cov_w_x_group[i] <- x_group[i] * (wt + m * (x_group[i] - k)) - x_group[i] * w_group[i]
    pars$btw[j] <- covd(x_group, w_group)
    pars$win[j] <- mean(cov_w_x_group)
    pars$total[j] <- pars$btw[j] + pars$win[j]

  }

  return(pars)

}

