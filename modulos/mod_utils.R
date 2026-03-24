# =========================================================
# Utilidades de cálculo de muestra
# =========================================================

ss4HHSm <- function (N, M, rho, mu, sigma, delta, conf, m, r = 1, b = 1)
{
  bar.n <- Deff <- n <- Mi <- M <- rep(NA, times = length(m))
  for (k in 1:length(m)) {
    bar.n[k] <- m[k] * r * b
    Deff[k] <- 1 + (bar.n[k] - 1) * rho
    n[k] <- ss4m(N, mu, sigma, DEFF = Deff[k], conf = 0.95,
                 error = "rme", delta = delta)
    Mi[k] <- n[k]/(r * b)
    M[k] <- n[k]/bar.n[k]
  }
  result <- data.frame(HouseholdsPerPSU = round(m), PersonsPerPSU = round(bar.n),
                       DEFF = round(Deff, 2), PSUinSample = round(M), HouseholdsInSample = round(Mi),
                       PersonsInSample = round(n))
  return(result)
}

ss4HHSp <- function (N, M, r, b, rho, P, delta, conf, m)
{
  bar.n <- Deff <- n <- Mi <- M <- rep(NA, times = length(m))
  for (k in 1:length(m)) {
    bar.n[k] <- m[k] * r * b
    Deff[k] <- 1 + (bar.n[k] - 1) * rho
    n[k] <- ss4p(N, P, DEFF = Deff[k], conf = conf, error = "rme",
                 delta = delta)
    Mi[k] <- n[k]/(r * b)
    M[k] <- n[k]/bar.n[k]
  }
  result <- data.frame(HouseholdsPerPSU = round(m), PersonsPerPSU = round(bar.n),
                       DEFF = round(Deff, 2), PSUinSample = round(M), HouseholdsInSample = round(Mi),
                       PersonsInSample = round(n))
  return(result)
}

ipfp_aproximada <- function(tabla_prop, n_total) {
  tabla_n <- round(tabla_prop * n_total)
  dif <- n_total - sum(tabla_n)
  if (dif != 0) {
    idx <- which.max(tabla_prop)
    tabla_n[idx] <- tabla_n[idx] + dif
  }
  tabla_n
}
