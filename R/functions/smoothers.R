#' Some smoothing functions





# smooth hazard ratios, both inputs are log-transformed
# log-transform both vectors and smooth with a GAM
smoothllgam <- function(.data, .x, .y, se.fac = 1) {
  quo_x <- enquo(.x)
  quo_y <- enquo(.y)
  # quo_k <- enquo(.k)

  name_y <- quo_name(quo_y)

  # unlog = function(x){exp(x)-0.000001}

  datNA <-
    .data %>%
    select(!!quo_x, !!quo_y) %>%
    mutate(
      .lx = log(!!quo_x),
      .ly = log(!!quo_y)
    )

  dat <-
    datNA %>%
    filter(!is.na(.ly))

  gamobj <- mgcv::gam(data = dat, formula = .ly ~ s(.lx))


  fit_name <- paste0(name_y, "_smth")
  llfit_name <- paste0(name_y, ".ll_smth")
  ulfit_name <- paste0(name_y, ".ul_smth")

  pred <-
    predict(gamobj, dat, type = "link", se.fit = TRUE) %>%
    as.tibble() %>%
    transmute(
      !!fit_name := fit,
      !!llfit_name := fit - se.fac * se.fit,
      !!ulfit_name := fit + se.fac * se.fit
    ) %>%
    mutate_all(funs(gamobj$family$linkinv(.))) %>%
    mutate_all(funs(exp(.))) %>%
    bind_cols(dat, .) %>%
    select(-.lx, -.ly)

  output <-
    datNA %>%
    select(!!quo_x, !!quo_y) %>%
    left_join(pred)

  return(output)
}


# smooth survival variable at d days over continuous variable
smoothbingam <- function(.data,
                         .x,
                         .surv,
                         .cens,
                         days,
                         se.fac = 1,
                         k = 7,
                         range = NULL) {
  quo_x <- enquo(.x)
  quo_surv <- enquo(.surv)
  quo_cens <- enquo(.cens)

  name_x <- quo_name(quo_x)
  name_mean <- paste0("meanmort", days)
  name_smth <- paste0("smthmort", days)
  name_smthll <- paste0("smthmort.ll", days)
  name_smthul <- paste0("smthmort.ul", days)

  dat <-
    .data %>%
    select(!!quo_x, !!quo_surv, !!quo_cens) %>%
    mutate(
      days = days,
      mort = replace(!!quo_cens, (!!quo_surv) > days, 0)
    )

  meandat <-
    dat %>%
    group_by(!!quo_x) %>%
    summarise(
      n = n(),
      mortmean = mean(mort)
    ) %>%
    transmute(
      !!quo_x,
      n,
      !!name_mean := mortmean
    )



  gamobj <- mgcv::gam(
    data = dat,
    formula = as.formula(paste0("mort ~ s(", name_x, ", k=",k,")")),
    family = "binomial"
  )

  if (is.null(range)) {
    preddat <-
      tibble(!!name_x := full_seq(dat[[name_x]], 1))
  }

  if (!is.null(range)) {
    preddat <-
      tibble(!!name_x := range)
  }

  pred <-
    predict(gamobj, preddat, type = "link", se.fit = TRUE) %>%
    as.tibble() %>%
    transmute(
      !!name_smth := fit,
      !!name_smthll := fit - se.fac * se.fit,
      !!name_smthul := fit + se.fac * se.fit
    ) %>%
    mutate_all(funs(gamobj$family$linkinv(.))) %>%
    bind_cols(preddat, .) %>%
    left_join(meandat) %>%
    replace_na(list(n = 0))

  return(pred)
}





ggmm <- function(df,
                 x,
                 y,
                 alpha_condition = 1 == 1,
                 add_text        = c(NA, "n", "prop", "perc"),
                 round_text      = 2) {
  stopifnot(is.data.frame(df))
  add_text <- match.arg(add_text)

  x_q <- enquo(x)
  y_q <- enquo(y)
  a_q <- enquo(alpha_condition)

  plot_set <- df %>%
    add_alpha_ind(a_q) %>%
    x_cat_y_cat(x_q, y_q) %>%
    add_freqs_col()

  plot_return <- mm_plot(plot_set, x_q, y_q)

  plot_return <- set_alpha(df, plot_return, a_q)

  if (!is.na(add_text)) {
    plot_set$text <- make_text_vec(plot_set, add_text, round_text)
    plot_set$freq <- calculate_coordinates(plot_return)
    text_part <- geom_text(data = plot_set, aes(label = text))
  } else {
    text_part <- NULL
  }

  plot_return + text_part
}
