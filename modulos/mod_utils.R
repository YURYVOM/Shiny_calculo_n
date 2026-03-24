# =========================================================
# Utilidades de cálculo de muestra
# =========================================================

.resolve_fun <- function(fun_name, package_candidates) {
  for (pkg in package_candidates) {
    if (requireNamespace(pkg, quietly = TRUE) && exists(fun_name, envir = asNamespace(pkg), mode = "function")) {
      return(getFromNamespace(fun_name, pkg))
    }
  }
  stop(
    sprintf(
      "No se encontró la función '%s' en los paquetes esperados: %s",
      fun_name,
      paste(package_candidates, collapse = ", ")
    ),
    call. = FALSE
  )
}

.call_with_variants <- function(fn, variants) {
  errores <- character(0)

  for (args in variants) {
    intento <- tryCatch(
      do.call(fn, args),
      error = function(e) e
    )

    if (!inherits(intento, "error")) {
      return(intento)
    }

    errores <- c(errores, conditionMessage(intento))
  }

  stop(
    paste0(
      "No se pudo ejecutar la función externa con las variantes esperadas. ",
      "Errores observados: ",
      paste(unique(errores), collapse = " | ")
    ),
    call. = FALSE
  )
}

ss4HHSm <- function(N, M, rho, mu, sigma, delta, conf, m) {
  fn <- .resolve_fun("ss4HHSm", c("samplesize4surveys", "SAMPLESIZE4SURVEYS"))

  .call_with_variants(
    fn,
    variants = list(
      list(N = N, M = M, rho = rho, mu = mu, sigma = sigma, delta = delta, conf = conf, m = m),
      list(N = N, M = M, rho = rho, mu = mu, sigma = sigma, delta = delta, m = m, conf = conf),
      list(N = N, M = M, rho = rho, xbar = mu, S = sigma, delta = delta, conf = conf, m = m),
      list(N = N, M = M, rho = rho, mean = mu, sd = sigma, delta = delta, conf = conf, m = m),
      list(N, M, rho, mu, sigma, delta, conf, m)
    )
  )
}

ss4HHSp <- function(N, M, rho, p, delta, conf, m) {
  fn <- .resolve_fun("ss4HHSp", c("samplesize4surveys", "SAMPLESIZE4SURVEYS"))

  .call_with_variants(
    fn,
    variants = list(
      list(N = N, M = M, rho = rho, p = p, delta = delta, conf = conf, m = m),
      list(N = N, M = M, rho = rho, P = p, delta = delta, conf = conf, m = m),
      list(N = N, M = M, rho = rho, p = p, delta = delta, m = m, conf = conf),
      list(N = N, M = M, rho = rho, P = p, delta = delta, m = m, conf = conf),
      list(N, M, rho, p, delta, conf, m)
    )
  )
}

ipfp_aproximada <- function(tabla_prop, n_total) {
  fn_ipfp <- .resolve_fun("IPFP", c("ipfp"))

  if (!is.matrix(tabla_prop)) {
    tabla_prop <- as.matrix(tabla_prop)
  }

  if (sum(tabla_prop) <= 0) {
    stop("La tabla base para IPFP debe tener suma positiva.", call. = FALSE)
  }

  tabla_prop <- tabla_prop / sum(tabla_prop)
  tot_fila <- rowSums(tabla_prop) * n_total
  tot_col <- colSums(tabla_prop) * n_total

  ajustada <- fn_ipfp(
    Row.knw = tot_fila,
    Col.knw = tot_col,
    Table = tabla_prop
  )

  tabla_n <- round(ajustada)
  dif <- as.numeric(n_total - sum(tabla_n))
  if (dif != 0) {
    idx <- which.max(ajustada)
    tabla_n[idx] <- tabla_n[idx] + dif
  }

  tabla_n
}
