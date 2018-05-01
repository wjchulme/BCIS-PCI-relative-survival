#' To tidy up survfit object
#'
#' Add various other survival-based point estimates to a tidied survfit object. Like [broom::tidy()] but adds more estimates. Includes ability to expland o include all possible time points, even where no events occurred
#'
#' @param survfit Object of class "survfit" from [survival::Surv()]
#' @param expand Logical. if true then expand survival estimates to include rows for days when no events (or censoring) occurred.
#' @return A tibble.
#' @keywords survival
#' @export

tidy_surv <-
  function(
    survfit,
    expand = FALSE
  ) {

  if (!expand) {
    output <-
      survfit %>%
      tidy() %>%
      transmute(
        time,
        leadtime = lead(time),
        int = leadtime - time,
        n.risk,
        n.event,
        n.censor,

        # actuarial estimates
        surv = cumprod(1 - n.event / n.risk), # =1-(cml.event/max(n.risk))
        cml.event = cumsum(n.event),
        cml.censor = cumsum(n.censor),
        cml.haz = -log(surv), # =cumsum(haz)
        haz = n.event / ((n.risk - (n.censor / 2) - (n.event / 2)) * int), # =(cml.haz-lag(cml.haz))/int
        se.haz = (haz * sqrt(1 - (haz * int / 2)^2)) / sqrt(n.event),
        sumerand = n.event / ((n.risk - n.event) * n.risk),
        se.surv = surv * sqrt(cumsum(sumerand)),

        # log(-log()) scale
        llsurv = log(-log(surv)),
        se.llsurv = sqrt((1 / log(surv)^2) * cumsum(sumerand)),

        # kaplan-meier / nelson-aalen estimators
        haz_km = n.event / (n.risk * int), # =-(surv-lag(surv))/lag(surv)
        cml.haz_km = cumsum(haz_km), # =cumsum(haz_km)
        se.haz_km = haz_km * sqrt((n.risk - n.event) / (n.risk * n.event))#,
      )
  }

  if (expand) {
    output <-
      survfit %>%
      tidy() %>%
      complete(
        time = full_seq(time, 1),
        fill = list(n.event = 0, n.censor = 0)
      ) %>%
      fill(n.risk, .direction = c("up")) %>%
      transmute(
        time,
        n.risk,
        n.event,
        n.censor,
        surv = cumprod(1 - n.event / n.risk), # =1-(cml.event/max(n.risk))
        cml.event = cumsum(n.event),
        cml.censor = cumsum(n.censor),
        cml.haz = -log(surv), # =cumsum(haz)
        haz = n.event / ((n.risk - (n.censor / 2) - (n.event / 2))), # =cml.haz-lag(cml.haz)
        se.haz = (haz * sqrt(1 - (haz / 2)^2)) / sqrt(n.event),
        sumerand = n.event / ((n.risk - n.event) * n.risk),
        se.surv = surv * sqrt(cumsum(sumerand)),
        llsurv = log(-log(surv)),
        se.llsurv = sqrt((1 / log(surv)^2) * cumsum(sumerand)),
        # LL.surv=surv^(exp(1.96*se.llsurv)),
        # UL.surv=surv^(exp(-1.96*se.llsurv)),

        # kaplan-meier / nelson-aalen estimators
        haz_km = n.event / (n.risk), # =-(surv-lag(surv))/lag(surv)
        cml.haz_km = cumsum(n.event / n.risk), # =cumsum(haz_km)
        se.haz_km = haz_km * sqrt((n.risk - n.event) / (n.risk * n.event))#,
      )
  }

  return(output)
}
