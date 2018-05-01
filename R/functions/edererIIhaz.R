#' Ederer II hazard estimator
#'
#' Calculate mean expected risk/hazard/mortality using the Ederer II method (i.e., only for those who remain at risk)
#'
#' @param .data data.frame
#' @param .time Integer. Survival time
#' @param .event 0/1 or 'TRUE'/'FALSE'. Censor / Survival status indicator
#' @param estimate "surv" (point estimate), "lower" (lower conf limit), "upper" (upper conf limit)
#' @return A tibble
#' @keywords survival
#' @export


edererIIhaz <-
  function(
    .data,
    .time,
    .event,
    .matchedhaz # expected hazard from matched population
  ) {

    if (is_grouped_df(.data)) {
      stop(".data must be ungrouped (not a grouped_df)")
    }

    quo_time <- enquo(.time)
    quo_event <- enquo(.event)
    quo_matchedhaz <- enquo(.matchedhaz)

    dat <-
      .data %>%
      mutate(
        time = !!quo_time,
        event = !!quo_event,
        match.haz = !!quo_matchedhaz
      )

    n <- nrow(dat)

    ndat <- dat %>%
      group_by(time) %>%
      summarise(event_or_censor = n()) %>%
      ungroup() %>%
      mutate(n.risk = n - lag(cumsum(event_or_censor), n = 1, default = 0))

    output <-
      dat %>%
      group_by(time) %>%
      summarise(sumhaz = sum(match.haz)) %>%
      right_join(ndat, ., by = c("time")) %>%
      ungroup() %>%
      arrange(-time) %>%
      mutate(
        n.risk2 = cumsum(event_or_censor),
        exp.hazII = cumsum(sumhaz) / cumsum(event_or_censor)
      ) %>%
      arrange(time) %>%
      complete(time = full_seq(time, 1)) %>% # this expands dataset to include all possible time points, even where no events occurred
      select(-sumhaz) %>%
      replace_na(list(event_or_censor = 0)) %>%
      fill(everything(), .direction = "up") %>%
      mutate(
        cml.exp.hazII = cumsum(exp.hazII), # matches very closely to that produced by rs.surv
        # cml.exp.hazIIb=(exp.hazII)*time,
        exp.survII = exp(-cml.exp.hazII)
      )

    return(output)
  }
