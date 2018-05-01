#######################################
# create mock data
#######################################

library('mice')




BCISsample <- PCIdata %>%
  select(-hospID) %>%
  sample_frac(0.1)

BCISsample_mis <- BCISsample %>%
  mutate_at(
    .vars = vars(
      age,
      sex,
      indicationGrp,
      ethnicityWBAO,
      CS,circsupportIABP,circsupportCPS,circsupportInotropes,ventilated,
      LVEFcat,
      histPCI,histCABG,
      histMI,
      histmedicalCVA,
      histmedicalVHD,histmedicalPVD,histmedicalHC,histmedicalHTN,
      histDiabetes_YN,
      histrenalGrp,
      smoking,
      accessGrp,
      terMulti,terLMS,occlusions_YN
    ), 
    .funs = funs(ifelse(runif(length(.)) < 0.2, NA, .))
  ) 


class(BCISsample_mis$sex)
table(BCISsample_mis$sex, useNA='ifany')


class(BCISsample$sex)
table(BCISsample$sex, useNA='ifany')

BCISsample_ini <- mice(BCISsample_mis, m=1, maxit=0, pred=quickpred(BCISsample_mis, exclude=c("procID","patID","year","year2","month","psurv","pcens")))


BCISsample_mids <- mice(BCISsample_ini, m=1, maxit=5, pred=BCISsample_ini$predictorMatrix)


BCISsample_imputed <- mice::complete(BCISsample_mids, action=1)


table(BCISsample_imputed$sex, useNA = 'ifany')

plot(BCISsample$age, BCISsample_imputed$age+runif(length(BCISsample_imputed$age)))


table(BCISsample$sex, paste(BCISsample_imputed$sex, "imp"), useNA='ifany')



test <- PCIdata %>%
  select(year,year2,month,sex,indicationGrp,age,psurv,pcens) %>%
  simulate_dataset(
    digits=2, n=NA,
    use.levels=TRUE, use.miss=TRUE,
    stealth.level=2, level3.noise=FALSE, 
    ignore=c("year2", "year", "month", "sex", "indicationGrp")
  ) %>%
  mutate(
    pcens30 = replace(pcens, psurv > 30, 0),
    psurv30 = replace(psurv, psurv > 30, 30),
    pcens365 = replace(pcens, psurv > 365, 0),
    psurv365 = replace(psurv, psurv > 365, 365),
    yearRT = mdy.date(month, 1, year),
    yearRT1 = mdy.date(1, 1, year),
    sexRT = tolower(sex)
  )


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



survests_age2 <-
  survmodels2 %>%
  unnest(smoothmortage) %>%
  left_join(life_tables2) %>%
  arrange(year2, sex, indicationGrp) 


survests_time2 <-
  survmodels2 %>%
  unnest(RSurvests, smoothhaz, smoothrathaz) %>%
  arrange(year2, sex, indicationGrp) 
