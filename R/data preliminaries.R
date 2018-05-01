
#devtools::install_github("wjchulme/willsutils")
library("tidyverse")
library("httr")
library("readxl")
library("broom")

library("survival")
library("relsurv")

library("scales")
library('ggmosaic')
library("willsutils")


# choose BCIS registry data from local file
BCISdatapath=file.choose()

# this loads useful functions useful throughout the project
list.files("./R/functions", pattern = "*.R", full.names = TRUE) %>%
  walk(source)



# this re-loads the big data frames
list.files("./data", pattern = "*.RData", full.names = TRUE) %>%
  walk(load, envir=.GlobalEnv)


################################################################################################
## extract ONS life-table data
################################################################################################

# currently no ONS API support for life-table data, so extracting direct from ONS URL:

lt_URL <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandandwalesreferencetables/current/nltew1416reg.xls"
GET(lt_URL, write_disk(lt_filepath <- file.path("./data-raw", basename(lt_URL)), overwrite = FALSE))


lt_M <-
  lt_filepath %>%
  excel_sheets() %>%
  set_names() %>%
  grep("\\d{4}[-]\\d{4}", x = ., value = TRUE) %>% # takes only sheets with names of the form eg "2006-2008"
  map_dfr(read_excel, path = lt_filepath, range = "A7:F108", .id = "band") %>%
  mutate(sex = "Male") # matches values in BCIS


lt_F <-
  lt_filepath %>%
  excel_sheets() %>%
  set_names() %>%
  grep("\\d{4}[-]\\d{4}", x = ., value = TRUE) %>%
  map_dfr(read_excel, path = lt_filepath, range = "H7:L108", .id = "band") %>%
  mutate(
    sex = "Female", # matches values in BCIS
    x = rep(0:100, n() / 101) # because age column is separated from data for women in the spreadsheet
  )


# check ages for men and women match element-wise
sum(lt_M$x != lt_F$x) 

life_tables <-
  full_join(lt_M, lt_F) %>%
  transmute(
    band,
    year = as.integer(stringr::str_sub(band, 1, 4)) + 1, # extract middle year in each band
    
    sex,
    age = x,
    # agecat=cut(age,c(0,50,60,70,80,Inf), labels=c("<50","50s","60s","70s","80+"),right=FALSE),
    
    # LTmort=mx, #m=central mortality rate ~= average force of mortality between x and x+1 - if using this, best to ?? floor age ( floor(age) )
    LTmort = qx, # q=proportion of people aged exactly x who die before reaching x+1 - if using this, best to round age to nearest birthday ( round(age,1) )
    
    # LTmortMth=1-((1-LTmort)^(30/365.241)), #mortality at 30 days (derived from 1 year estimate)
    # LTmortMthc=1-((1-LTmort)/(1-LTmortMth)), #mortality at 365 given survival to 30
    
    hq = qx / 365.241, # daily death rate
    hm = mx / 365.241, #  actuarial estimate / daily central death rate
    hq_const = -log(1 - qx) / 365.241, # assumes hazard constant within interval
    
    LThaz = hq_const,
    # q30=1-((1-q)^(30/365))
    LTsurv = 1 - LTmort
    
  ) %>%
  arrange(year, sex, age) %>%
  select(band, year, sex, age, LTmort, LThaz, LTsurv) %>%
  filter(year %in% 2007:2014)


life_tables2 <-
  life_tables %>%
  mutate(year2 = paste0(plyr::round_any(year, 2, ceiling) - 1, "-", plyr::round_any(year, 2, ceiling))) %>%
  group_by(year2, sex, age) %>%
  summarise(LThaz = mean(LThaz),
            LTsurv = mean(LTsurv)
            )

########################################################################################
# convert life_tables data to ratetable format to work with relsurv functions
########################################################################################

rt_M <- life_tables %>%
  filter(sex == "Male") %>%
  select(year, age, LTsurv) %>%
  group_by(age) %>%
  spread(year, LTsurv) %>%
  remove_rownames() %>%
  column_to_rownames(var = "age") %>%
  as.matrix()

rt_F <- life_tables %>%
  filter(sex == "Female") %>%
  select(year, age, LTsurv) %>%
  group_by(age) %>%
  spread(year, LTsurv) %>%
  remove_rownames() %>%
  column_to_rownames(var = "age") %>%
  as.matrix()

rate_tables <- relsurv::transrate(
  rt_M, rt_F,
  yearlim = c(2007, 2014),
  int.length = 1
)


################################################################################################
# BCIS-NICOR data
################################################################################################


PCIdata <-
  readRDS(file = BCISdatapath) %>%
    filter(
      geoCountry %in% c("England NHS", "Wales NHS"),
      year %in% 2007:2014,
      !is.na(terGrp),
      lvl(indicationGrp) %in% 1:3,
      !is.na(sex),
      !is.na(age),
      between(age, 18, 100),
      !is.na(psurv), !is.na(pcens),
      TRUE
    ) %>%
    arrange(year,sex,indicationGrp) %>%
    mutate(
      year2 = paste0(plyr::round_any(year, 2, ceiling) - 1,"-",plyr::round_any(year, 2, ceiling)),
      sex = lvlval(sex),
      indicationGrp = fct_inorder(lvlval(indicationGrp)),
      NULL
    ) %>%
    select(
      procID, patID, hospID, year, year2, month, monthly, sex, age, indicationGrp, psurv, pcens,
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
      #procGP2b3adrugs_YN2,antiplat2,drugTh5,drugTh7,
      #stentGrp,
      terMulti,terLMS,occlusions_YN
    ) %>%
    droplevels()



survdata <-
  PCIdata %>%
  select(procID, patID, hospID, year, year2, month, monthly, sex, age, indicationGrp, psurv, pcens) %>%
    mutate(
      age0 = age,
      agefloor = floor(age0), # use if using m as the mortality rate estimate (see life tables)
      ageround = round(age0), # use if using q as the mortality rate estimate (see life tables)
      age = ageround,
      psurv = psurv + 1, # this is because most survival methods assume event-time>=1 whereas BCIS registry has time>=0
      pcens30 = replace(pcens, psurv > 30, 0),
      psurv30 = replace(psurv, psurv > 30, 30),
      pcens365 = replace(pcens, psurv > 365, 0),
      psurv365 = replace(psurv, psurv > 365, 365),
      yearRT = mdy.date(month, 1, year),
      yearRT1 = mdy.date(1, 1, year),
      sexRT = tolower(sex),
      NULL
    ) %>%
  left_join(life_tables, by = c("sex", "age", "year")) 




#save(PCIdata, file = file.path("data","PCIdata.RData"))
#save(survdata, file = file.path("data","survdata.RData"))

save(rate_tables, file = file.path("data","rate_tables.RData"))
save(life_tables, file = file.path("data","life_tables.RData"))
save(life_tables2, file = file.path("data","life_tables2.RData"))










