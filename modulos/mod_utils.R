# =========================================================
# Utilidades de cálculo de muestra
# =========================================================

SS4HHSm <- function(s, amplitud, conf, N, deff = 1) {
  z <- qnorm(1 - (1 - conf) / 2)
  me <- amplitud / 2
  n0 <- (z * s / me)^2
  n_dis <- n0 * deff
  n_adj <- n_dis / (1 + (n_dis - 1) / N)
  ceiling(n_adj)
}

SS4HHSp <- function(p, amplitud, conf, N, deff = 1) {
  z <- qnorm(1 - (1 - conf) / 2)
  me <- amplitud / 2
  n0 <- (z^2 * p * (1 - p)) / (me^2)
  n_dis <- n0 * deff
  n_adj <- n_dis / (1 + (n_dis - 1) / N)
  ceiling(n_adj)
}

DGJunction <- function(tipo_param, s = NULL, p = NULL, amplitud, conf, N, m, rho, r, b, c_h, c_upm,
                       usa_dominios = FALSE, n_dominios = 1, sim = 1) {
  deff <- 1 + (m - 1) * rho

  n_encuestas <- if (tipo_param == "Media") {
    SS4HHSm(s = s, amplitud = amplitud, conf = conf, N = N, deff = deff)
  } else {
    SS4HHSp(p = p, amplitud = amplitud, conf = conf, N = N, deff = deff)
  }

  n_total <- if (isTRUE(usa_dominios)) ceiling(n_encuestas * n_dominios * sim) else n_encuestas
  n_hogares <- ceiling(n_total / (r * b))
  upm <- ceiling(n_hogares / m)
  costo_total <- n_hogares * c_h + upm * c_upm

  list(
    deff = deff,
    n_encuestas = n_encuestas,
    n_total = n_total,
    n_hogares = n_hogares,
    upm = upm,
    costo_total = costo_total
  )
}

ipfp_aproximada <- function(tabla_prop, n_total) {
  # Aproximación proporcional para asignar muestra total en tabla cruzada.
  tabla_n <- round(tabla_prop * n_total)
  dif <- n_total - sum(tabla_n)
  if (dif != 0) {
    idx <- which.max(tabla_prop)
    tabla_n[idx] <- tabla_n[idx] + dif
  }
  tabla_n
}
