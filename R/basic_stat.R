p_ci <- function(x, r = 2, p = TRUE) {
  out <- format(round(x, r), nsmall = r, trim = TRUE)

  if (p) {
    paste0(out["mean"], "%", " [", out["low"],"%,", out["high"],"%]")
  } else {
    paste0(out["mean"], " [", out["low"],",", out["high"],"]")
  }

}

get_effect_ci <- function(
  effect = tu,
  resp = NULL,
  term = "duration_m",
  ci_lvl = "0.95",
  r = 2,
  p = FALSE,
  add_CI = TRUE
  ) {
  term1 <- term

  out <- effect %>%
    filter(
      ci_level == paste0("level:",ci_lvl),
      term == term1
    )

  if (!is.null(resp)) {
    out %<>%
      filter(response == resp)
  }

  out <- out[, c("low", "high", "mean")] %>%
    pivot_longer(everything()) %>%
    deframe()

  ci <- p_ci(x = out, r = r, p = p)

  if (add_CI) {
    ci <- paste0("CI", as.numeric(ci_lvl)* 100, "%: ", ci)
  }

  ci
}
