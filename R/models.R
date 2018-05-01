
######################################################################################################################################
# RUN MODELS, STRATIFIED BY SEX, YEAR, INDICATION
######################################################################################################################################



##use this if using synthetic data - everything else will follow
survdata <- survdata_syn




### stratifying by one-year bands

survmodels <-
  survdata %>%
  nest(-year, -sex, -indicationGrp) %>%
  mutate(
    Nprocedures = map_int(data, ~ nrow(.)),
    RSurvmodel = map(data, ~ rs.surv(Surv(psurv365, pcens365) ~ ratetable(age = age * 365.241, year = yearRT1, sex = sexRT),
                                     data = .x, ratetable = rate_tables, method = "ederer2")
                     ),
    RSurvests = map(RSurvmodel, ~ tidy_rsurv(.)),

    smoothhaz = map(RSurvests, ~ smoothllgam(., .x = time, .y = haz, sqrt(qchisq(0.95, 1)))),
    smoothrathaz = map(RSurvests, ~ smoothllgam(., .x = time, .y = rat.haz, sqrt(qchisq(0.95, 1)))),

    smoothmortage30 = map(data, ~ smoothbingam(., .x = age, .surv = psurv, .cens = pcens, days = 30, sqrt(qchisq(0.95, 1)), k=7)),
    smoothmortage365 = map(data, ~ smoothbingam(., .x = age, .surv = psurv, .cens = pcens, days = 365, sqrt(qchisq(0.95, 1)), k=7)),
    smoothmortage = map2(smoothmortage30,smoothmortage365, ~left_join(.x, .y, by=c('age','n')))
  ) %>% select(-smoothmortage30, -smoothmortage365)




### stratifying by two-year bands

survmodels2 <-
  survdata %>%
  nest(-year2, -sex, -indicationGrp) %>%
  mutate(
    Nprocedures = map_int(data, ~ nrow(.)),
    #Survmodel = map(data, ~ survfit(Surv(psurv365, pcens365) ~ 1, data=.x)),
    RSurvmodel = map(data, ~ rs.surv(Surv(psurv365, pcens365) ~ ratetable(age = age * 365.241, year = yearRT1, sex = sexRT),
                                     data = .x, ratetable = rate_tables, method = "ederer2")
                     ),
    RSurvests = map(RSurvmodel, ~ tidy_rsurv(.)),
    
    smoothhaz = map(RSurvests, ~ smoothllgam(., .x = time, .y = haz, sqrt(qchisq(0.95, 1)))),
    smoothrathaz = map(RSurvests, ~ smoothllgam(., .x = time, .y = rat.haz, sqrt(qchisq(0.95, 1)))),

    smoothmortage30 = map(data, ~ smoothbingam(., .x = age, .surv = psurv, .cens = pcens, days = 30, sqrt(qchisq(0.95, 1)), k=7)),
    smoothmortage365 = map(data, ~ smoothbingam(., .x = age, .surv = psurv, .cens = pcens, days = 365, sqrt(qchisq(0.95, 1)), k=7)),
    smoothmortage = map2(smoothmortage30,smoothmortage365, ~left_join(.x, .y, by=c('age','n')))
  ) %>% select(-smoothmortage30,-smoothmortage365)


save(survmodels, file = file.path("data","survmodels.RData"))
save(survmodels2, file = file.path("data","survmodels2.RData"))


######################################################################################################################################
# useful, unnested format
######################################################################################################################################


survests_age <-
  survmodels %>%
  unnest(smoothmortage) %>%
  left_join(life_tables) %>%
  arrange(year, sex, indicationGrp)

survests_time <-
  survmodels %>%
  unnest(RSurvests, smoothhaz, smoothrathaz) %>%
  arrange(year, sex, indicationGrp) 


survests_age2 <-
  survmodels2 %>%
  unnest(smoothmortage) %>%
  left_join(life_tables2) %>%
  arrange(year2, sex, indicationGrp) 


survests_time2 <-
  survmodels2 %>%
  unnest(RSurvests, smoothhaz, smoothrathaz) %>%
  arrange(year2, sex, indicationGrp) 


######################################################################################################################################
#log rank tests, STRATIFIED BY SEX, INDICATION
######################################################################################################################################

#### gets p-value from log-rank test
logrankp <- function(...) {
  sdo <- survdiff(...)
  1 - pchisq(sdo$chisq, length(sdo$n) - 1)
}

#### gets p-value from long-rank test in relative survival setting (see Stare J, Henderson R, Pohar M.; 2005; An individual measure of relative survival. J R Stat Soc Ser C )
logrankrel <- function(...) {
  phcoxo <- rstrans(...)
  1 - pchisq(phcoxo$score, nrow(phcoxo$var))
}


logrank2 <-
  survdata %>%
  arrange(year2, sex, indicationGrp) %>%
  nest(-sex, -indicationGrp) %>%
  mutate(
    LRt = map(data, ~ logrankp(Surv(psurv365, pcens365) ~ year2, data = .)),
    LRtrel = map(data, ~ logrankrel(Surv(psurv365, pcens365) ~ year2 + ratetable(age = age * 365.241, sex = sexRT, year = yearRT1), data = ., ratetable = rate_tables))
  ) %>%
  unnest(LRt, LRtrel) %>%
  mutate(
    p = print_pval(LRt, 3),
    prel = print_pval(LRtrel, 3)
  )


survtable2 <-
  survests_time2 %>%
  arrange(sex, indicationGrp, year2, time) %>%
  group_by(sex, indicationGrp, year2) %>%
  mutate(
    time30 = ((time <= 30)*time) == max((time <= 30)*time),
    time90 = ((time <= 90)*time) == max((time <= 90)*time),
    time365 = ((time <= 365)*time) == max((time <= 365)*time),
    days = fct_inorder(as.character(30*time30 + 90*time90 + 365*time365))
  ) %>%
  filter(time30 | time90 | time365) %>%
  group_by(days, add = TRUE) %>%
  select(
    year2, sex, indicationGrp, days, time,
    surv,
    surv.ll,
    surv.ul,
    rel.surv,
    rel.surv.ll,
    rel.surv.ul,
    cml.exs.haz,
    cml.exs.haz.ul,
    cml.exs.haz.ll
  ) %>% 
  arrange(days, sex, indicationGrp, year2) %>%
  ungroup()

