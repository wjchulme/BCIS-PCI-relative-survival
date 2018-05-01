

#### distribution of procedures by sex, year, indication
ggplot(data = survdata %>% mutate(sex_year=paste0(sex, ", ", year))) +
  geom_mosaic(aes(weight = 1, x = product(indicationGrp, year, sex), fill = factor(indicationGrp)), colour = "black", offset = 0, divider = mosaic("v")) +
  labs(x = "Year", y = "Proportion", colour="Indication") +
  #facet_grid(sex ~ .) +
  guides(fill=guide_legend(title = "", reverse = TRUE))+
  #scale_colour_manual(values = colourgrad) +
  scale_fill_manual(values = c("#E69F00", "#0072B2", "#CC79A7")) +
  theme_bw()


ggplot(data = survdata %>% mutate(sex_ind=paste0(sex, ", ", indicationGrp))) +
  geom_mosaic(aes(weight = 1, x = product(year), fill = factor(sex_ind)), colour = "black", offset = 0, divider = mosaic("h")) +
  labs(x = "Year", y = "Proportion", colour="Indication") +
  #facet_grid(sex ~ .) +
  guides(fill=guide_legend(title = "", reverse = TRUE))+
  #scale_colour_manual(values = colourgrad) +
  scale_fill_manual(values = c("#E69F00", "#0072B2", "#CC79A7", "#E69F00", "#0072B2", "#CC79A7")) +
  theme_bw()







##################################################################################################
#### make some reusable plotting things
##################################################################################################

exp_trans <- function() trans_new("exp", exp, exp)
colourpal <- c("#E69F00", "#0072B2", "#CC79A7", "#56B4E9", "#666666")
colourgrad <- c("#0072B2", "#56B4E9", "#E69F00", "#D55E00")
colourgrad <- c("#0072B2", "#90AFC1", "#D8A349", "#D55E00")
#colourgrad <- colorRampPalette(c("#56B4E9", "#E69F00"), space='Lab')(6)
#barplot(rep(1, length(colourgrad)), names.arg = colourgrad, col = colourgrad)
#colourgrad <- c("#fdcc8a", "#fc8d59", "#e34a33", "#b30000")
surv_theme <- theme_bw(base_size = 15) + theme(axis.ticks = element_line(size = 0.1), panel.grid.minor = element_blank(), strip.background = element_blank(), legend.position = "none")



################################################################################
# PLOTS
# sex * indication * year
################################################################################


# annotation template
ann_text2 <-
  tibble(
    # time=365,
    # y=c(0.1,0.01,0.001),
    year = unique(survests_time2$year2),
    label = (as.character(year)),
    colour = colourgrad[1:4],
    sex = "Male",
    indicationGrp = "Elective",
    vjust = 1, hjust = 1
  )

annopdata<-
survests_time2 %>%
  group_by(sex, indicationGrp) %>%
  summarise_at(
    .vars=vars(surv, rel.surv, exp.haz, cml.haz, rat.haz, rat.haz_smth, exp.cml.haz, exp.haz),
    .funs=funs(min, max), na.rm=TRUE
  ) %>%
  left_join(logrank2, .) %>%
  ungroup()


legendanno2 <- function(xpos, ypos) {
  geom_text(data = ann_text2 %>% mutate(x = xpos), aes(x = x, label = label, vjust = vjust, hjust = hjust, colour = year), y = ypos, fontface = "bold", inherit.aes = FALSE)
}


legendannop <- function(xpos, ypos) {
  geom_text(data = annopdata, aes(x = x, label = p, vjust = 0, hjust = 0), x = xpos, y = ypos, colour = "black", inherit.aes = FALSE)
}

legendannoprel <- function(xpos, ypos) {
  
  geom_text(data = annopdata, aes(x = x, label = prel, vjust = 0, hjust = 0), x = xpos, y = ypos, colour = "black", inherit.aes = FALSE)
}



plotbasesurv2 <-
  survests_time2 %>%
    ggplot(aes(x = time, colour = year2, fill = year2, group = year2)) +
    scale_x_continuous(breaks = seq(0, 365, 100)) +
    scale_colour_manual(values = colourgrad) +
    scale_fill_manual(values = colourgrad) +
    facet_grid(indicationGrp ~ sex, scales = "free_y")
    #facet_grid(indicationGrp ~ sex)
    
    
    
# mark whether with CI or not, with free vertical axis or not
plottype <- " CI (free axis)"

plotsurv <-
  plotbasesurv2 +
    geom_step(aes(y = surv), linetype = "solid", size = 0.5) +
     #geom_step(aes(y=surv.ll), linetype='dotted', size=0.5)+
     #geom_step(aes(y=surv.ul), linetype='dotted', size=0.5)+
     geom_rect(aes(xmin=time, xmax=leadtime, ymin=surv.ll, ymax=surv.ul), alpha=0.2, colour=NA) +

    labs(x = "Days since procedure", y = "Survival rate") +
    # scale_y_continuous(limits=c(0.85,1.001))+
    #legendannop(10,c(0.971,0.971,0.933,0.933, 0.863,0.863))+ #CI location
    legendannop(10, c(0.975, 0.975, 0.941, 0.941, 0.865, 0.865)) + #  location when y free
    #legendannop(10, 0.865) + #  location when y free
    surv_theme
plotsurv
ggsave(paste0("./output/figures/sex-indication-year/survival", plottype, ".png"), plotsurv, width = 15, height = 20, units = "cm")


plotrelsurv <-
  plotbasesurv2 +
    geom_step(aes(y = rel.surv), linetype = "solid", size = 0.5) +
    # geom_step(aes(y=rel.surv.ll), linetype='dotted', size=0.5)+
    # geom_step(aes(y=rel.surv.ul), linetype='dotted', size=0.5)+
     geom_rect(aes(xmin=time, xmax=leadtime, ymin=rel.surv.ll, ymax=rel.surv.ul), alpha=0.2, colour=NA) +
    # geom_point(data=survunnest2 %>% filter(indicationGrp=='Elective') %>% mutate(rel.surv=1.002), aes(y=rel.surv), alpha=0)+

    labs(x = "Days since procedure", y = "Relative survival ratio") +
    # scale_y_continuous(limits=c(0.85,1.001))+

    #legendannoprel(10,c(0.990,0.990, 0.957,0.957, 0.883,0.883))+ #CI location
    legendannoprel(10, c(0.993, 0.993, 0.961, 0.961, 0.891, 0.891)) + # location when y free
    #legendannoprel(10, 0.891) + # location when y free
    surv_theme
plotrelsurv
ggsave(paste0("./output/figures/sex-indication-year/relative survival", plottype,".png"), plotrelsurv, width = 15, height = 20, units = "cm")




plotcmlhazs <-
  plotbasesurv2 +

    geom_line(aes(y = exp.cml.haz), linetype = "dashed", size = 0.2) +
    geom_step(aes(y = cml.haz), size = 0.5) +
    geom_rect(aes(xmin=time, xmax=leadtime, ymin=cml.haz.ll, ymax=cml.haz.ul), alpha=0.3, colour=NA) +

    labs(y = "Cumulative hazard rate", x = "Days since procedure") +

    #scale_y_continuous(limits = c(-.001, 0.16)) +
    surv_theme
plotcmlhazs
ggsave(paste0("./output/figures/sex-indication-year/cml haz", plottype, ".png"), plotcmlhazs, width = 15, height = 20, units = "cm")



plotcmlexchazs <-
  plotbasesurv2 +

    geom_step(aes(y = cml.exs.haz), size = 0.5) +
    geom_rect(aes(xmin = time, xmax = leadtime, ymin = cml.exs.haz.ll, ymax = cml.exs.haz.ul), alpha = 0.2, colour = NA) +
    labs(y = "Cumulative excess hazard rate", x = "Days since procedure") +

    # scale_y_continuous(limits=c(-.001,0.16))+

    surv_theme
plotcmlexchazs
ggsave(paste0("./output/figures/sex-indication-year/cml excess haz",plottype ,".png"), plotcmlexchazs, width = 15, height = 20, units = "cm")


###########################################################################################################





plothazs <-
  plotbasesurv2 +
  geom_line(aes(y = exp.haz), linetype = "dashed", size = 0.2) +
  # geom_point(aes(y=haz), size=0.1, alpha=0.3)+
  geom_line(aes(y = haz_smth), size = 0.5) +
  geom_ribbon(aes(ymin=haz.ll_smth, ymax=haz.ul_smth, colour=NULL), alpha=0.3)+
  
  labs(y = "Hazard rate", x = "Days since procedure") +
  
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    , limits = (c(0.00001, 0.1))
  ) +
  annotation_logticks(sides = "l", short = unit(.5, "mm"), mid = unit(1, "mm"), long = unit(2, "mm"), size = 0.1) +
  
  surv_theme

plothazs
ggsave(paste0("./output/figures/sex-indication-year/hazard",plottype ,".png"), plothazs, width = 15, height = 20, units = "cm")




plothazratios <-
  plotbasesurv2 +
  
  geom_hline(aes(yintercept = 1), colour = "black", linetype = "dashed", size = 0.2) +
  # geom_point(aes(y=rat.haz), size=0.1, alpha=0.3)+
  geom_line(aes(y = rat.haz_smth), size = 0.5) +
  geom_ribbon(aes(ymin=rat.haz.ll_smth, ymax=rat.haz.ul_smth, colour=NULL), alpha=0.3)+
  
  
  labs(y = "Hazard rate ratio", x = "Days since procedure") +
  
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    , limits = (c(0.3, 1000))
  ) +
  annotation_logticks(sides = "l", short = unit(.5, "mm"), mid = unit(1, "mm"), long = unit(2, "mm"), size = 0.1) +
  
  surv_theme

plothazratios
ggsave(paste0("./output/figures/sex-indication-year/hazard ratio", plottype, ".png"), plothazratios, width = 15, height = 20, units = "cm")



plothazdiffs <-
  plotbasesurv2 +
  
  # geom_point(aes(y=haz-exp.haz), size=0.1, alpha=0.3)+
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 0.2) +
  geom_line(aes(y = haz_smth - exp.haz), size = 0.5) +
  # geom_ribbon(aes(ymin=haz.ll_smth-exp.haz, ymax=haz.ul_smth-exp.haz, colour=NULL), alpha=0.3)+
  
  labs(y = "Hazard rate difference (additive excess hazard)", x = "Days since procedure") +
  
  surv_theme

plothazdiffs
ggsave(paste0("./output/figures/sex-indication-year/hazard diff",plottype ,".png"), plothazdiffs, width = 15, height = 20, units = "cm")





################################################################################
# estimates over age
################################################################################




plotbasemortage2 <-
  survests_age2 %>%
  ggplot(aes(x = age, colour = year2, fill = year2, group = year2)) +
  facet_grid(indicationGrp ~ sex) +
  # guides(fill=FALSE, alpha=FALSE)+
  scale_x_continuous(breaks = seq(20, 100, 20)) +
  scale_colour_manual(values = colourgrad) +
  scale_fill_manual(values = colourgrad) +
  annotation_logticks(sides = "l", short = unit(.5, "mm"), mid = unit(1, "mm"), long = unit(2, "mm"), size = 0.1)




####
plotmortbyage365 <-
  plotbasemortage2 +
  # geom_point(aes(y=meanmort365), size=0.5)+
  geom_line(aes(y = smthmort365), size = 0.5) +
  geom_ribbon(aes(ymin=smthmort.ll365, ymax=smthmort.ul365, colour=NULL), alpha=0.3)+
  geom_line(aes(y = 1 - exp(-365 * LThaz), linetype = "ONS national \nlife-table estimate"), linetype = "dashed", size = 0.2) +
  
  labs(x = "Age", y = "1-year mortality rate", colour = "Indication", linetype = NULL, fill = NULL, alpha = NULL) +
  
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    # ,limits=(c(0.0001,1))
  ) +
  coord_cartesian(xlim = c(15, 100), ylim = c(0.00001, 1)) +
  
  surv_theme
plotmortbyage365
ggsave("./output/figures/sex-indication-year/logmort365byage CI.png", plotmortbyage365, width = 15, height = 20, units = "cm")






plotmortbyage30 <-
  plotbasemortage2 +
  # geom_point(aes(y=meanmort30), size=0.5)+
  geom_line(aes(y = smthmort30), size = 0.5) +
  geom_ribbon(aes(ymin=smthmort.ll30, ymax=smthmort.ul30, colour=NULL), alpha=0.3)+
  geom_line(aes(y = 1 - exp(-30 * LThaz), linetype = "ONS national \nlife-table estimate"), linetype = "dashed", size = 0.2) +
  
  labs(x = "Age", y = "30-day mortality rate", colour = "Indication", linetype = NULL, fill = NULL, alpha = NULL) +
  
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    # ,limits=(c(0.0001,1))
  ) +
  coord_cartesian(xlim = c(15, 100), ylim = c(0.00001, 1)) +
  
  surv_theme
plotmortbyage30
ggsave("./output/figures/sex-indication-year/logmort30byage CI.png", plotmortbyage30, width = 15, height = 20, units = "cm")



plotmortratiobyage365 <-
  plotbasemortage2 +
  
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.2) +
  # geom_point(aes(y=meanmort365/(1-exp(-365*LThaz))), size=0.5)+
  geom_line(aes(y = smthmort365 / (1 - exp(-365 * LThaz))), size = 0.5) +
  geom_ribbon(aes(ymin = smthmort.ll365 / (1 - exp(-365 * LThaz)), ymax = smthmort.ul365 / (1 - exp(-365 * LThaz)), colour = NULL), alpha = 0.3) +
  
  labs(x = "Age", y = "1-year mortality rate ratio", colour = "Indication", linetype = NULL, fill = NULL, alpha = NULL) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    # ,limits=(c(0.0001,1))
  ) +
  coord_cartesian(xlim = c(15, 100), ylim = c(0.5, 1500)) +
  surv_theme
plotmortratiobyage365
ggsave("./output/figures/sex-indication-year/logmort365ratiobyage CI.png", plotmortratiobyage365, width = 15, height = 20, units = "cm")

plotmortratiobyage30 <-
  plotbasemortage2 +
  
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.2) +
  # geom_point(aes(y=meanmort30/(1-exp(-30*LThaz))), size=0.5)+
  geom_line(aes(y = smthmort30 / (1 - exp(-30 * LThaz))), size = 0.5) +
  geom_ribbon(aes(ymin=smthmort.ll30/(1-exp(-30*LThaz)), ymax=smthmort.ul30/(1-exp(-30*LThaz)), colour=NULL), alpha=0.3)+
  
  labs(x = "Age", y = "30-day mortality rate ratio", colour = "Indication", linetype = NULL, fill = NULL, alpha = NULL) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    # ,limits=(c(0.0001,1))
  ) +
  coord_cartesian(xlim = c(15, 100), ylim = c(0.5, 10000)) +
  surv_theme
plotmortratiobyage30
ggsave("./output/figures/sex-indication-year/logmort30ratiobyage CI.png", plotmortratiobyage30, width = 15, height = 20, units = "cm")






################################################################################
# sex * year * indication
################################################################################




ann_text <-
  tibble(
    # time=365,
    # y=c(0.1,0.01,0.001),
    indicationGrp = unique(survests_time2$indicationGrp),
    label = (as.character(indicationGrp)),
    colour = colourpal[1:3],
    sex = "Male",
    year = as.character(2007),
    vjust = 1, hjust = 1
  )
legendanno <- function(xpos, ypos) {
  geom_text(data = ann_text %>% mutate(x = xpos), aes(x = x, label = label, vjust = vjust, hjust = hjust, colour = indicationGrp), y = ypos, fontface = "bold", inherit.aes = FALSE)
}

##################################################################################################
#### central survival and relative survival estimates charts
##################################################################################################

ann_text_at <-
  tibble(
    # time=365,
    # y=c(0.1,0.01,0.001),
    # days = unique(survtable$days),
    # label = paste0(unique(survtable$days)," days"),
    year2 = unique(survtable2$year2),
    label = year2,
    colour = colourgrad[1:4],
    sex = "Male",
    indicationGrp = as.factor("Elective"),
    vjust = 1, hjust = 1
  )
legendanno_at <- function(xpos, ypos) {
  geom_text(data = ann_text_at %>% mutate(x = xpos), aes(x = x, label = label, vjust = vjust, hjust = hjust, colour = year2), y = ypos, fontface = "bold", inherit.aes = FALSE)
}



plotsurvat <-
  ggplot(data = survtable2, aes(x = as.factor(days), group = as.factor(year2), colour = as.factor(year2))) +
  
  geom_point(aes(y = surv), position = position_dodge(width = 0.6), size = 1) +
  # geom_line(aes(y=rel.surv), position=position_dodge(width=0.6), size=0.2, alpha=0.3)+
  geom_errorbar(aes(ymin = surv.ll, ymax = surv.ul), position = position_dodge(width = 0.6), size = 0.5, width = 0.5) +
  geom_point(data = survtable2 %>% filter(sex == "Male", indicationGrp == "Elective") %>% mutate(rel.surv = 1.0026), aes(y = rel.surv), alpha = 0) +
  # geom_abline(intercept=1, slope=0, colour='grey', linetype='dotted')+
  geom_vline(xintercept = seq(1, 2) + .5, colour = "grey") +
  facet_grid(indicationGrp ~ sex) +
  scale_colour_manual(values = colourgrad) +
  labs(x = "Days post-procedure", y = "Survival rate") +
  # scale_y_continuous(breaks=seq(0,2,0.01))+
  expand_limits(y = 1.0) +
  # legendanno_at(4, c(0.993, 0.992, 0.991, 0.990))+
  surv_theme +
  theme(
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()
  )
plotsurvat

ggsave("./output/figures/sex-indication-year/plotsurvat (fixed axis).png", plotsurvat, width = 15, height = 20, units = "cm")





plotexshazat <-
  ggplot(data = survtable2, aes(x = as.factor(days), group = year2, colour = year2)) +
  
  geom_point(aes(y = cml.exs.haz), position = position_dodge(width = 0.6), size = 1) +
  # geom_line(aes(y=1-surv), position=position_dodge(width=0.6), size=0.2, alpha=0.3)+
  geom_errorbar(aes(ymin = cml.exs.haz.ll, ymax = cml.exs.haz.ul), position = position_dodge(width = 0.6), size = 0.5, width = 0.4) +
  facet_grid(indicationGrp ~ sex) +
  scale_colour_manual(values = colourgrad) +
  scale_y_continuous(limits = c(-0.01, 0.15)) +
  labs(x = "Days post-procedure", y = "Cumulative excess hazard rate") +
  legendanno_at(3, c(0.15, 0.13, 0.11, 0.09)) +
  surv_theme
ggsave("./output/figures/sex-indication-year/plotexshazat (fixed axis).png", plotexshazat, width = 15, height = 20, units = "cm")


plotrelsurvat <-
  ggplot(data = survtable2, aes(x = as.factor(days), group = as.factor(year2), colour = as.factor(year2))) +
  
  geom_point(aes(y = rel.surv), position = position_dodge(width = 0.6), size = 1) +
  # geom_line(aes(y=rel.surv), position=position_dodge(width=0.6), size=0.2, alpha=0.3)+
  geom_errorbar(aes(ymin = rel.surv.ll, ymax = rel.surv.ul), position = position_dodge(width = 0.6), size = 0.5, width = 0.5) +
  geom_point(data = survtable2 %>% filter(sex == "Male", indicationGrp == "Elective") %>% mutate(rel.surv = 1.0026), aes(y = rel.surv), alpha = 0) +
  # geom_abline(intercept=1, slope=0, colour='grey', linetype='dotted')+
  geom_vline(xintercept = seq(1, 2) + .5, colour = "grey") +
  facet_grid(indicationGrp ~ sex) +
  scale_colour_manual(values = colourgrad) +
  labs(x = "Days post-procedure", y = "Relative survival ratio") +
  # scale_y_continuous(breaks=seq(0,2,0.01))+
  expand_limits(y = 1.0) +
  # legendanno_at(4, c(0.993, 0.992, 0.991, 0.990))+
  surv_theme +
  theme(
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()
  )
plotrelsurvat

ggsave("./output/figures/sex-indication-year/plotrelsurvat (fixed axis).png", plotrelsurvat, width = 15, height = 20, units = "cm")











