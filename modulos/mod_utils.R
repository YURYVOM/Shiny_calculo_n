# =========================================================
# Utilidades de cálculo de muestra
# =========================================================

ss4HHSm <- function(N, M, rho, mu, sigma, delta, conf, m) {
  z <- qnorm(1 - (1 - conf) / 2)
  n_srs <- (z * sigma / delta)^2
  deff <- 1 + (m - 1) * rho
  n_hh <- n_srs * deff
  n_hh_fpc <- n_hh / (1 + (n_hh - 1) / N)
  n_psu <- min(ceiling(n_hh_fpc / m), ceiling(M))
  ceiling(n_psu * m)
}

ss4HHSp <- function(N, M, rho, p, delta, conf, m) {
  z <- qnorm(1 - (1 - conf) / 2)
  n_srs <- (z^2 * p * (1 - p)) / (delta^2)
  deff <- 1 + (m - 1) * rho
  n_hh <- n_srs * deff
  n_hh_fpc <- n_hh / (1 + (n_hh - 1) / N)
  n_psu <- min(ceiling(n_hh_fpc / m), ceiling(M))
  ceiling(n_psu * m)
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
