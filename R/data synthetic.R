#######################################
# create mock data
#######################################

library('synthpop')



PCIdata_forsynthing <- PCIdata %>%
  select(-hospID, -patID, -procID) 

choose.seed=2007
rules.conditions <- list(CS = "indicationGrp == 'Elective'")
rules.values <- list(CS = "96. Coded not applicable")

predmatrix <- mice::quickpred(PCIdata_forsynthing, mincor=0, exclude=c("year","year2","month"))

PCIdata_sds <- syn(PCIdata_forsynthing, seed=choose.seed, 
                rules=rules.conditions, rvalues=rules.values,
                predictor.matrix=predmatrix,
                #method="parametric",
                default.method=c('pmm','logreg','polyreg','poly'),
                proper=TRUE,
                drop.not.used=FALSE,
                drop.pred.only=FALSE)


survdata_syn <- PCIdata_sds$syn %>%
  select(monthly, sex, age, indicationGrp, psurv, pcens) %>%
  mutate(year = floor(monthly),
         year2 = paste0(plyr::round_any(year, 2, ceiling) - 1,"-",plyr::round_any(year, 2, ceiling)),
         month = round((monthly-year)*12+0.5),
         age=round(age),
         psurv = psurv + 1, # this is because most survival methods assume event-time>=1 whereas BCIS registry has time>=0
         pcens30 = replace(pcens, psurv > 30, 0),
         psurv30 = replace(psurv, psurv > 30, 30),
         pcens365 = replace(pcens, psurv > 365, 0),
         psurv365 = replace(psurv, psurv > 365, 365),
         yearRT = mdy.date(month, 1, year),
         yearRT1 = mdy.date(1, 1, year),
         sexRT = tolower(sex)
         ) %>%
  arrange(year,sex,age,indicationGrp)


save(survdata_syn, file = file.path("data","survdata_syn.RData"))

