#' KM estimator
#'
#' Wrapper for [stepfun()] to creates a function to output vector of KM estimates for a given survival-censor pair
#'
#' @param day Integer. Number of days after time zero
#' @param time Integer. Survival time
#' @param event 0/1 or 'TRUE'/'FALSE'. Censor / Survival status indicator
#' @param estimate "surv" (point estimate), "lower" (lower conf limit), "upper" (upper conf limit)
#' @return A function of class "stepfun"
#' @keywords survival, kaplan-meier
#' @export


kmstepfun <-
  function(
    day,
    time,
    event,
    estimate = c("surv", "lower", "upper")
  ) {
    
    df <- tibble(time = time, event = event) %>% arrange(time)
    km <- survfit(Surv(time, event) ~ 1, data = df)
    survest <- stepfun(km$time, c(1, km[["surv"]]))(day)
    survestll <- stepfun(km$time, c(1, km[["lower"]]))(day)
    survestul <- stepfun(km$time, c(1, km[["upper"]]))(day)
    return(c(survest,survestll, survestul))
  }


#' @examples
#' kmstepfun()
