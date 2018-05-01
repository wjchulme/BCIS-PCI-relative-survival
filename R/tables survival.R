
######################################################################################################################################
#print summary of model outputs
######################################################################################################################################


survtable_print <-
  survtable2 %>%
  transmute(
    year2, sex, indicationGrp, days,
    mort = format((1-surv)*100, digits = 1),
    mortCI = print_2bracket((1-surv.ll)*100, (1-surv.ul)*100, 1),
    surv = format(surv, digits = 3),
    survCI = print_2bracket(surv.ll, surv.ul, 3),
    rel.surv = format(rel.surv, digits = 3),
    rel.survCI = print_2bracket(rel.surv.ll, rel.surv.ul, 3),
    NULL
  )

write_csv(survtable_print, "./output/tables/relsurv-estimates.csv")



######################################################################################################################################
#print some statistics in format for baseline tables
######################################################################################################################################



#km-estimates
kmestimates_print <-
  survtable2 %>%
  mutate(
    sex_year2=paste0(sex, year2),
    KM_CIs = print_est2bracket((1-surv)*100, (1-surv.ul)*100, (1-surv.ll)*100, 1)
  ) %>%
  select(sex_year2,indicationGrp,days, KM_CIs) %>%
  spread(sex_year2, KM_CIs) %>%
  filter(days!=90)

write_csv(kmestimates_print, "./output/tables/KM-estimates.csv")



#### mean ages
meanages_print <-
  survdata %>%
  arrange(year, sex, indicationGrp) %>%
  group_by(year2, sex, indicationGrp) %>%
  summarise(
    n = n(),
    meanage = mean(age),
    sdage = sd(age),
    age_m_sd = paste0(specify_decimal(meanage, 1), " (", specify_decimal(sdage, 1), ")"),
    TRUE
  ) %>%
  mutate(sexyear2 = paste0(sex, year2)) %>%
  ungroup() %>%
  select(sexyear2, age_m_sd, indicationGrp) %>%
  spread(sexyear2, age_m_sd)

write_csv(meanages_print, "./output/tables/mean-ages.csv")


######################################################################################################################################
#print summary of model outputs
######################################################################################################################################



#####################################################


# when does censoring begin?
survdata %>% 
  filter(year == 2014) %>% 
  group_by(year, month) %>% 
  summarise(n = n(),
            censored = sum(psurv < 365 & pcens == 0)/n
  )



#how many patients appear twice or more?
repproc=
  PCIdata %>%
  group_by(indicationGrp, patID) %>%
  arrange(year,month) %>%
  mutate(
    ones=1,
    procnumber=cumsum(ones)
  ) %>%
  group_by(procnumber) %>%
  summarise(
    n=n()
  )

#### distribution of procedures in each year
survdata %>%
  arrange(year, sex, indicationGrp) %>%
  group_by( year2) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    N = sum(n),
    pct = specify_decimal(100 * n / N, 1)
  ) %>%
  ungroup() %>% 
  utils::View()






 


#numbers in each group
with(survdata, table(year2, indicationGrp)) %>% prop.table(1)
with(survdata, table(indicationGrp)) %>% prop.table()

with(survdata, table(year, sex)) %>% prop.table(1)
with(survdata, table(indicationGrp, sex)) %>% prop.table(1)
with(survdata, table(sex)) %>% prop.table()



