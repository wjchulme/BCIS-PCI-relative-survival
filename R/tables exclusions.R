
######################################################################################################################################
#####flowchart describing BCIS registry data exclusions
######################################################################################################################################



flowchart0 =
  readRDS(file = BCISdatapath) %>%
  filter(
    geoCountry %in% c("England NHS", "Wales NHS"),
    year %in% 2007:2014,
    !is.na(terGrp)
  )



#better way to do this??
flowchart0 %>%
  mutate(
    step0=TRUE,
    step1=!is.na(age) & between(age,18,100),
    step2=step1 & !is.na(sex),
    step3=step2 & lvl(indicationGrp) %in% 1:3,
    step4=step3 & !is.na(psurv) & !is.na(pcens)
  ) %>%
  summarise_at(vars(starts_with("step")), sum) %>%
  gather(value='n') %>%
  mutate(excl=lag(n,1,first(n))-n,
         cumexcl=cumsum(excl),
         pct=100*n/lag(n),
         pcttot=100*n/first(n)
         )




