get_re_prediction_inla <- function(
  inla_mod = NULL,
  effect = "siteid1",
  trend_class = TRUE,
  exponentiate = FALSE,
  modelling_data = NULL) {

  re <- inla_mod$summary.random[[effect]] %>%
    as_tibble() %>%
    rename(
      quant0.025 = `0.025quant`,
      quant0.975 = `0.975quant`,
      quant0.5 = `0.5quant`
    ) %>%
    select(-kld)

  re_name <- str_extract(effect, "station|basin")
  colnames(re)[colnames(re) == "ID"] <- re_name

  if(trend_class) {
    re <- re %>%
      mutate(
        trend_class = case_when(
          quant0.025 > 0 & quant0.975 > 0 ~ "increase",
          quant0.025 < 0 & quant0.975 < 0 ~ "decrease",
          sign(quant0.025) * sign(quant0.975) == -1 ~ "stable",
          TRUE ~ "NA"
        )
      )

  }


  if (exponentiate) {
    re <- re %>%
      mutate(across(where(is.double), ~exp(. - 1)))
  }

  return(re)
}

get_hpdmarginal_inla <- function(
  inla_mod = NULL,
  type = "fixed",
  p = c(.80, .90, 0.95)) {

  if (type == "fixed") {
    m <- inla_mod$marginals.fixed
    mi <- inla_mod$summary.fixed
  } else if (type == "rand") {
    m <- inla_mod$marginals.hyperpar
    mi <- inla_mod$summary.hyperpar
  }
  output <- map_dfr(m, function(x) {
    if (!any(x == "Inf")) {

     inla.hpdmarginal(marginal = x, p = c(.80, .90, 0.95)) %>%
    as.data.frame() %>%
    rownames_to_column("ci_level")
    } else {
      tibble(
        ci_level = c("level:0.80", "level:0.90", "level:0.95"),
        low = NA,
        high = NA)
    }

  }
,
  .id = "term"
  )

  ## Add mean
  output <- output %>%
    left_join(
      mi %>%
        rownames_to_column("term") %>%
        as_tibble %>%
        select(term, mean),
      by = "term"
    )


  if (type == "rand") {
    output[c("low", "mean", "high")] <- map(output[c("low", "mean", "high")], tau_to_sigma)
  }

  return(output)
}

plot_uniform_quantile_inla <- function(mod_inla = NULL) {
  pit <- sort(mod_inla$cpo$pit)

  tb <- tibble(
    pit = pit,
    uniquant = (seq_along(pit)) / (length(pit)+1)
  )

  tb %>%
  ggplot(aes(x = uniquant, y = pit)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = "uniform quantiles", y = "Sorted PIT values")
}

tau_to_sigma <- function(x) {
  1 / sqrt(x)
}

sigma_to_tau <- function(x) {
  1 / (x^2)
}

r2_mvp <- function(
  var_pred = NULL,
  epsilon = NULL,
  std_intercept = NULL,
  type = "all"
  ) {

  # from std to var
  var_interp <- sum(map_dbl(std_intercept, ~.x^2))

  out <- list(
    marginal = var_pred / (var_pred + var_interp + epsilon^2),
    conditional = (var_pred  + var_interp) / (var_pred + var_interp + epsilon^2)
    )

  if (type == "all") {
    return(out)
  } else if (type == "marginal") {
    return(out[["marginal"]])
  } else if (type == "conditional") {
    return(out[["conditional"]])
  }

}

get_std_inla_from_rand <- function (
  inla_rand_tab = NULL
  ) {
  x <- inla_rand_tab %>%
    filter(ci_level == "level:0.95") %>%
    mutate(
      term = str_remove(term, "Precision for "),
      term = str_replace(term, "the Gaussian observations", "epsilon")
      ) %>%
    distinct(response, term, mean)
  x %>%
    pivot_wider(names_from = "term", values_from = "mean")
}

r2_mvp <- function(
  var_pred = NULL,
  epsilon = NULL,
  std_intercept = NULL,
  type = "all"
  ) {

  # from std to var
  var_interp <- sum(map_dbl(std_intercept, ~.x^2))

  out <- list(
    marginal = var_pred / (var_pred + var_interp + epsilon^2),
    conditional = (var_pred  + var_interp) / (var_pred + var_interp + epsilon^2)
    )

  if (type == "all") {
    return(out)
  } else if (type == "marginal") {
    return(out[["marginal"]])
  } else if (type == "conditional") {
    return(out[["conditional"]])
  }

}

log_beta_to_perc_rate <- function (x) {
  (exp(x) - 1) * 100
}

plot_credible_interval <- function (dataset) {

  # Borrowed from
  # https://github.com/roelvanklink/Final-insect-abundance-changes/blob/master/Full%20INLA%20analysis%20FINAL%20cleaned%2020191230.R
  dataset %>%
  ggplot() +
    geom_errorbar(aes(x = Unit, ymin = X0.025quant, ymax = X0.975quant, color = Realm),
      alpha = 0.5,
      size = 1, width = 0, position = position_dodge(width = 0.7)
    ) +
    geom_errorbar(aes(x = Unit, ymin = X0.05quant, ymax = X0.95quant, color = Realm),
      alpha = 0.75,
      size = 2, width = 0, position = position_dodge(width = 0.7)
    ) +
    geom_errorbar(aes(x = Unit, ymin = X0.1quant, ymax = X0.9quant, color = Realm),
      alpha = 1,
      size = 3, width = 0, position = position_dodge(width = 0.7)
    ) +
    geom_point(aes(x = Unit, y = mean, shape = Realm),
      size = 2.5, position = position_dodge(width = 0.7),
      color = "black", fill = "black", alpha = 1
    ) +
    #scale_color_manual(values = col.scheme.realm) +
    #scale_fill_manual(values = col.scheme.realm) +
    #scale_shape_manual(values = shps) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip()

}

plot_posterior_gaussian_sd <- function(inla_mod = NULL) {

  hyper_tb <-
    inla_mod$marginals.hyperpar[["Precision for the Gaussian observations"]]
  sigma_dist <- inla.tmarginal(tau_to_sigma, hyper_tb) %>%
    as_tibble

  sigma_dist %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    labs(x = expression(sigma), y = expression(paste("P(", sigma, " | Data)")))
}

plot_posterior_random <- function(inla_mod = NULL, scales = "free", ncol = 4) {

  hyper_tb <- inla_mod$marginals.hyperpar

  dist_term <- purrr::map_dfr(hyper_tb,
    ~inla.tmarginal(tau_to_sigma, .x) %>%
    as_tibble(),
  .id = "term")

  dist_term %>%
    mutate(term = str_replace_all(term, get_model_term_replacement())) %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    facet_wrap(vars(term), ncol = ncol, scales = scales) +
    labs(x = expression(sigma), y = expression(paste("P(", sigma, " | Data)")))
}

plot_posterior_fixed <- function(inla_mod = NULL, scales = "free", ncol = 4) {

  sum_fix <- inla_mod$marginals.fixed

  dist_term <- purrr::map_dfr(sum_fix,
    ~inla.smarginal(.x) %>%
    as_tibble(),
  .id = "term")

  dist_term %>%
    mutate(term = str_replace_all(term, get_model_term_replacement())) %>%
    ggplot(aes(x = x, y = y)) +
    geom_line() +
    facet_wrap(vars(term), ncol = ncol, scales = scales) +
    labs(x = expression(beta), y = expression(paste("P(", beta, " | Data)")))
}

plot_inla_fixed_effect <- function(
  x,
  color_var = response,
  color_point = NULL,
  term_to_rm = c("(Intercept)", "ter2", "ter3", "ter4"),
  intercept = 0,
  point_size = 3
  ) {

  p <- x %>%
    filter(!term %in% term_to_rm) %>%
    mutate(
      term = str_replace_all(term, term_replacement()),
      width_bar = 0,
      width_bar = case_when(
        ci_level == "level:0.95" ~ .1,
        ci_level == "level:0.90" ~ 5,
        ci_level == "level:0.80" ~ 10,
        TRUE ~ width_bar
        ),
      term = factor(term, term_levels())
      ) %>%
    ggplot(aes(y = term, x = mean, xmin = low, xmax = high, color = {{color_var}})) +
    geom_vline(xintercept = intercept, linetype = "dashed") +
    scale_y_discrete(limits = rev) +
    geom_blank() +
    geom_errorbar(
      aes(width = 0, size = width_bar),
      alpha = 0.5,
      position = position_dodge(width = .9),
      show.legend = FALSE
      ) +
    theme_bw()

  if (is.null(color_point)) {
    p +
      geom_point(
        aes(x = mean, y = term, color = {{color_var}}),
        alpha = 1, size = point_size,
        position = position_dodge(width = .9)
      )
  } else {
    p +
      geom_point(
        aes(x = mean, y = term),
        color = color_point,
        alpha = 1, size = point_size,
        position = position_dodge(width = .9)
      )
  }

}

old_plot_inla_fixed_effect <- function(
  dataset = NULL,
  scale_color = NULL,
  term_level = NULL,
  xaxis_title = FALSE,
  yaxis_title = FALSE,
  legend_present = FALSE,
  my_position_dodge = .9,
  point_size = 3,
  scale_width_bar = 1
  ) {

  if (is.null(term_level)) {
    term_level <- c(
      "Log (Year nb + 1)",
      "PCA1\nstream gradient",
      "Human footprint\n(1993)",
      "Log2 Human footprint\nratio (2009/1993)",
      "Log (Year nb + 1):\nPCA1\nstream gradient",
      "Log (Year nb + 1):\nHuman footprint\n(1993)",
      "Log (Year nb + 1):\nLog2 Human footprint\nratio (2009/1993)",
      "Log (Year nb + 1):\nPCA1\nstream gradient:\nHuman footprint\n(1993)",
      "Log (Year nb + 1):\nPCA1\nstream gradient:\nLog2 Human footprint\nratio (2009/1993)",
      "Log (Year nb + 1):\nHuman footprint\n(1993):\nLog2 Human footprint\nratio (2009/1993)"
          )
  }

  p <- dataset %>%
    mutate(
      term = factor(term, levels = term_level),
      response = factor(response, levels = names(scale_color))
    ) %>%
    ggplot(aes(
        y = term, x = mean,
        xmin = low, xmax = high,
        color = response,
        size = width_bar * scale_width_bar, width = 0)
      ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_discrete(limits = rev) +
    geom_blank() +
    geom_errorbar(
      alpha = 0.5,
      position = position_dodge(width = my_position_dodge),
      show.legend = FALSE
      ) +
    geom_point(
      aes(x = mean, y = term, color = response),
      alpha = 1, size = point_size,
      position = position_dodge(width = my_position_dodge)
      )

    if (!xaxis_title) {
      p <- p +
        theme(axis.title.x = element_blank())
    }

    if (!is.null(scale_color)) {
      p <- p +
        scale_color_manual(values = scale_color,
          name = "Community metrics")
    } else {
      p <- p +
        scale_color_brewer(palette = "RdYlBu")
    }

    if (!yaxis_title) {
      p <- p +
        theme(axis.title.y = element_blank())
    }

    if (!legend_present) {
      p <- p +
        theme(legend.position = "none")
    }

    return(p)
}

format_inla_model_list <- function(
  x = gaussian_inla_prior_std,
  prob = c(.80, .90, .95)
  ) {

  x %>%
    mutate(
      hpd_fixed = map(
        mod,
        ~get_hpdmarginal_inla(
          inla_mod = .x,
          type = "fixed",
          p = prob
          )
        ),
      #hpd_random = map(
        #mod,
        #~get_hpdmarginal_inla(
          #inla_mod = .x,
          #type = "rand",
          #p = prob
        #)
      #)
      ) %>%
    select(-mod) %>%
    #select(-hpd_random) %>%
    unnest(hpd_fixed) %>%
    mutate(order_effect = str_count(term, ":"))
}

formula_inla <- function(
  response = "d",
  species = FALSE,
  time = TRUE,
  dataset_name = "growth_data",
  plot_effect = TRUE
  ) {

  core_pred <- "ter + com * ms * watering"

  if (time) {
    if (response %in% c("d", "hm", "h", "bm")) {
      core_pred <- paste0("duration_m + I(duration_m^2) + ", core_pred)
    } else {
      core_pred <- paste0("duration_m + ", core_pred)
    }
  }

  if (species) {
    core_pred <- paste0(core_pred, " * species")
  } else {
    core_pred <- paste0(core_pred, " + species")
  }

  if (plot_effect) {
    pl <- paste0('f(IDtp, model = "z",
    Z = as(model.matrix(~ 0 + ter:plot, data = ', dataset_name,'), "Matrix")) +')
  } else {
    pl <- ''
  }

  as.formula(paste0(response, ' ~ 1 + ', core_pred,' + ', pl,
      'f(IDtl, model = "z",
        Z = as(model.matrix(~ 0 + ter:label, data = ', dataset_name,'), "Matrix"))
    '))
#+
      #f(IDtli, model = "z",
        #Z = as(model.matrix(~ 0 + ter:label:ind, data = ', dataset_name,'), "Matrix"))
}

formula_inla_env <- function(
  response = "temperature",
  time = TRUE,
  dataset_name = "growth_data",
  plot_effect = TRUE
  ) {

  core_pred <- "ter + ms * watering"

  if (time) {
    if (response %in% c("d", "hm", "h", "bm")) {
      core_pred <- paste0("duration_m + I(duration_m^2) + ", core_pred)
    } else {
      core_pred <- paste0("duration_m + ", core_pred)
    }
  }

  if (plot_effect) {
    pl <- paste0('f(IDtp, model = "z",
      Z = as(model.matrix(~ 0 + ter:plot, data = ', dataset_name,'), "Matrix")) +')
  } else {
    pl <- ''
  }

  as.formula(paste0(response, ' ~ 1 + ', core_pred,' + ', pl,
      'f(IDtl, model = "z",
      Z = as(model.matrix(~ 0 + ter:label, data = ', dataset_name,'), "Matrix"))
    '))
}

get_pred_data <- function(x = surv_data_modelling,
  predictor = c("com", "ms", "watering", "species", "duration_m")){

  pred_surv_data <- map(setNames(predictor, predictor), ~unique(x[[.x]])) %>%
    expand.grid()
  pred_surv_data[colnames(x)[!colnames(x) %in% predictor]] <- NA
  pred_surv_data
}

plot_obs_fitted_inla <- function(
  mod_inla = NULL,
  dataset = NULL,
  resp = NULL,
  pred_nrows = NULL,
  return_df = FALSE
  ) {

  fitted_values <- mod_inla$summary.fitted.values[["mean"]]

  if (!is.null(pred_nrows)) {
    row_selection <- length(fitted_values) - pred_nrows
    fitted_values <-
      fitted_values[1:row_selection]
  }

  obs_fit <- tibble(
    response = resp,
    fit = fitted_values,
    obs = dataset[[resp]]
  )

  if (return_df) {
    return(obs_fit)
  }

  obs_fit %>%
    ggplot(aes(x = fit, y = obs)) +
    geom_point(alpha = .5, size = 2) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    labs(x = "Fitted values", y = "Observed values")
}

inla_sampling_from_pred <- function(
  model = NULL,
  pred_data = pred_surv_data,
  variable = c("duration_m", "ms", "com", "watering", "species"),
  nsamp = 1000
  ) {
  cbind(
    pred_data[, variable],
    tibble(fitted = model$marginals.fitted.values[(length(model$marginals.fitted.values) - nrow(pred_data) + 1):length(model$marginals.fitted.values)])
    ) %>%
  as_tibble() %>%
  mutate(
    sampling = map(fitted,
      ~tibble(
        rep = 1:nsamp,
        value = inla.rmarginal(nsamp, .x)
      )
    )
    ) %>%
  select(-fitted) %>%
  unnest(sampling)
}

get_pred_values <- function(
  x = env_inla_pred,
  resp = "soil.moisture",
  m = 3,
  site = "Open",
  w = "No Watered"
  ) {
  mask <- x$response == resp &
    x$duration_m == m &
    x$ms == site &
    x$watering == w

  out <- x[mask, ]
  output <- out[, c("mean", "0.025quant", "0.975quant")]
  output <- unlist(output)
  names(output) <- c("mean", "low", "high")
  output

}
