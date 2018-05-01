#' To tidy survfit object from [relsurv::rs.surv()]
#'
#' Adds various useful survival-based point estimates for survfit object
#'
#' @param rsurvfit Object of class "survfit" from [relsurv::rs.surv()]
#' @return A tibble.
#' @keywords survival, relative survival
#' @export

# clean-up and add variables to output from rs.surv model object
# returns tibble
tidy_rsurv <-
  function(rsurvfit)
  {

  se.fac <- sqrt(qchisq(0.95, 1))

  tidied <-
    rsurvfit %>%
    tidy()

  intforevents <-
    tidied %>%
    filter(n.event != 0) %>%
    add_row(time = 0, .before = 1) %>%
    transmute(
      time,
      leadtime = lead(time),
      int = leadtime - time,
      lagint = lag(int, n = 1, default = NA)
    )

  output <-
    tidied %>%
    add_row(
      time = 0,
      n.risk = rsurvfit$n,
      n.event = 0,
      n.censor = 0,
      estimate = 1,
      std.error = 0,
      .before = 1
    ) %>%
    left_join(intforevents, by = "time") %>%
    transmute(
      time,
      leadtime,
      int,
      lagint,
      leadtime_ = lead(time),
      int_ = leadtime_ - time,
      n.risk,
      n.event,
      n.censor,
      sumerand = n.event / ((n.risk - n.event) * n.risk),
      cml.event = cumsum(n.event),
      cml.censor = cumsum(n.censor),
      haz = ifelse(n.event == 0,
                   NA,
                   n.event / ((n.risk - (n.censor / 2) - (n.event / 2)) * int)
                   ), # =(cml.haz-lag(cml.haz))/int
      se.haz = (haz * sqrt(1 - (haz * int / 2)^2)) / sqrt(n.event),
      surv = cumprod(1 - n.event / n.risk),
      se.surv = surv * sqrt(cumsum(sumerand)), # Greenwood's formula

      # log(-log) scale to get standard errors for CIs
      llsurv = log(-log(surv)),
      se.llsurv = sqrt((1 / log(surv)^2) * cumsum(sumerand)),
      surv.ll = surv^(exp(se.fac * se.llsurv)),
      surv.ul = surv^(exp(-se.fac * se.llsurv)),
      cml.haz = -log(surv), # =cumsum(haz)
      cml.haz.ll = -log(surv.ul),
      cml.haz.ul = -log(surv.ll),

      # kaplan-meier / nelson-aalen estimators
      haz_km = n.event / (n.risk * int), # =-(surv-lag(surv))/lag(surv)
      cml.haz_km = cumsum(ifelse(is.na(haz_km), 0, haz_km)),
      se.haz_km = haz_km * sqrt((n.risk - n.event) / (n.risk * n.event)),

      # relative survival
      rel.surv = estimate,
      se.rel.surv = std.error,

      # expected survival
      exp.surv = surv / rel.surv, # retrieve expected survival by inverting relative survival estimate
      exp.cml.haz = -log(exp.surv), # =(cml.haz-cml.exs.haz)
      exp.haz = (exp.cml.haz - lag(exp.cml.haz, n = 1, default = 0)) / lag(int, n = 1, default = NA),

      # conf intervals all based on on standard error of log(-log) of surv - ie, assume exp.haz is known without error
      rel.surv.ll = surv.ll / exp.surv,
      rel.surv.ul = surv.ul / exp.surv,

      # cumulative excess hazard
      cml.exs.haz = -log(rel.surv), # cumulative excess hazard
      cml.exs.haz.ll = -log(rel.surv.ul),
      cml.exs.haz.ul = -log(rel.surv.ll),

      # hazard ratio
      rat.haz = haz / exp.haz#,

      # transformed survival for testing relative survival differences using log-rank test:
      # these produce very similar CIs so may as well ignore
      # rel.llsurv = log(-log(rel.surv)),
      # se.rel.llsurv = sqrt( (1/log(rel.surv)^2)* cumsum(sumerand) ),
      # rel.surv.ll = rel.surv^(exp( se.fac*se.rel.llsurv)),
      # rel.surv.ul = rel.surv^(exp(-se.fac*se.rel.llsurv)),
      )
  return(output)
}

