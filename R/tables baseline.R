


#categorise patient and procedural characteristics
covargroups<-rbind(    c('group'='patients','variable'='patients')

                       ,cbind('Demography',       c('age','ageGrp','agecat','sex','ethnicity','ethnicityWBAO','ethnicityWAO','HDscore'))
                       ,cbind('Medical history',  c('smoking','BMI','histrenalGrp','histDiabetes_YN',
                                                    'histPCI','histCABG','histMI','histmedicalCVA','histmedicalHTx',
                                                    'histmedicalNCCS','histmedicalHC','histmedicalHTN','histmedicalPVD',
                                                    'creatinine','cholesterol'))
                       ,cbind('Structural HD',    c('histmedicalVHD','LVEFcat','occlusions_YN'))
                       ,cbind('Presentation',     c('urgency','indicationGrp','CS1','CS','GCSgrp','ventilated','circsupportIABP','circsupportCPS','circsupportInotropes','circsupport_YN'))
                       ,cbind('Procedure',        c('accessGrp','terLMS','terMulti','stentGrp','devicesATrmvl_YN','imaging','pressurewire'))
                       ,cbind('Pharmacology',     c('anticoagHepBiv','antiplat','anticoagHepBiv2','antiplat2',
                                                    'drugTh5','drugTh7','procGP2b3adrugs_YN','procGP2b3adrugs_YN2'))
                       ,cbind('Date',             c('year',"hospvolGrp"))
                       ,cbind('Outcomes',           c('pcens30','pcens365','inhosp','MACE'))
) %>% as_tibble() %>% mutate(order=row_number())


baselinetable0 <-
PCIdata %>%
  mutate(sex_year2 = paste0(sex,year2),
         procedures = 1,
         agecat = cut(age, breaks = c(0,50,60,70,80,120), labels = c("<50","50s","60s","70s","80+"), include.lowest = TRUE, right = FALSE)
         ) %>%
  group_by(indicationGrp, sex_year2) %>%
  summarise_tags(
    .vars = vars(
      procedures,
      agecat,
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
    n = length(procedures)
  ) %>% group_by(variable, add = TRUE) %>%
  mutate(
    N=sum(n),
    Nnonmissing = sum(n*(value %ni% NA)),
    pctnonmissing = Nnonmissing*100/N,
    pct = n*100/Nnonmissing,
    print_stat = paste0(specify_decimal(n,0), " (", specify_decimal(pct, 1), ")")
  ) %>%
  filter(
    value %ni% c(NA,"FALSE","0. No","0", "0. No renal disease"),
    !(indicationGrp == "Elective" & variable %in% c("CS","ventilated", "circsupportIABP", "circsupportCPS", "circsupportInotropes"))
 )


baseline_print <- baselinetable0 %>%
  select(indicationGrp, sex_year2, variable, value, print_stat) %>%
  group_by(indicationGrp, variable, value) %>%
  spread(sex_year2, print_stat) %>%
  left_join(covargroups) %>%
  group_by(variable) %>%
  arrange(indicationGrp, order)
  

missing_print <- baselinetable0 %>%
  summarise(print_nonmissing=paste0(specify_decimal(first(Nnonmissing),0), " (", specify_decimal(first(pctnonmissing), 1), ")")) %>%
  select(indicationGrp, sex_year2, variable, print_nonmissing) %>%
  group_by(indicationGrp, variable) %>%
  spread(sex_year2, print_nonmissing) %>%
  left_join(covargroups) %>%
  group_by(variable) %>%
  arrange(indicationGrp, order)

write.csv(baseline_print, file = "./output/tables/baseline-data.csv")

write.csv(missing_print, file = "./output/tables/missing-data.csv")








