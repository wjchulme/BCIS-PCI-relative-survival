




##################################################################################################
#### survival-type charts
##################################################################################################

plotsurv <-
  survests_time %>%
  ggplot(aes(x = time, colour = indicationGrp)) +
  
  geom_step(aes(y = surv), linetype = "solid", size = 0.5) +
  geom_rect(aes(xmin = time, xmax = leadtime, ymin = surv.ll, ymax = surv.ul, fill = indicationGrp), alpha = 0.3, colour = NA) +
  
  facet_grid(year ~ sex) +
  labs(x = "Days since procedure", y = "Survival rate") +
  scale_x_continuous(breaks = seq(0, 365, 100)) +
  
  scale_y_continuous(limits = c(0.8, 1)) +
  
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  
  legendanno(360, (c(0.905, 0.865, 0.825))) +
  surv_theme
plotsurv
ggsave("./output/figures/sex-year-indication/survival.png", plotsurv, width = 15, height = 20, units = "cm")



plotrelsurv <-
  survests_time2 %>%
  ggplot(aes(x = time, colour = indicationGrp)) +
  
  geom_step(aes(y = rel.surv), linetype = "solid", size = 0.5) +
  geom_rect(aes(xmin = time, xmax = leadtime, ymin = rel.surv.ll, ymax = rel.surv.ul, fill = indicationGrp), alpha = 0.3, colour = NA) +
  
  facet_grid(year2 ~ sex) +
  labs(x = "Days since procedure", y = "Relative survival ratio") +
  scale_x_continuous(breaks = seq(0, 365, 100)) +
  
  scale_y_continuous(limits = c(0.80, 1.05)) +
  
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  
  # legendanno(360, (c(0.905,0.865,0.825)))+
  surv_theme
plotrelsurv
ggsave("./output/figures/sex-year-indication/relative survival.png", plotrelsurv, width = 15, height = 20, units = "cm")



plotmortbyage365 <-
  ggplot(data = survests_age, aes(x = age)) +
  
  geom_line(aes(y = smthmort365, colour = indicationGrp), size = 0.5) +
  # geom_ribbon(aes(ymin=mort365.ll, ymax=mort365.ul, colour=NULL, fill=indicationGrp), alpha=0.3)+
  geom_line(aes(y = 1 - exp(-365 * LThaz), linetype = "ONS national \nlife-table estimate"), linetype = "dashed", size = 0.2) +
  
  facet_grid(year ~ sex) +
  labs(x = "Age", y = "1-year mortality rate", colour = "Indication", linetype = NULL, fill = NULL, alpha = NULL) +
  guides(fill = FALSE, alpha = FALSE) +
  
  scale_x_continuous(breaks = seq(20, 100, 20)) +
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    # ,limits=(c(0.0001,1))
  ) +
  annotation_logticks(sides = "l", short = unit(.5, "mm"), mid = unit(1, "mm"), long = unit(2, "mm"), size = 0.1) +
  coord_cartesian(xlim = c(15, 100), ylim = c(0.00001, 1)) +
  legendanno(100, log10(c(0.005, 0.0005, 0.00005))) +
  surv_theme
plotmortbyage365
ggsave("./output/figures/sex-year-indication/logmort365byage.png", plotmortbyage365, width = 15, height = 20, units = "cm")


plotmortbyage30 <-
  ggplot(data = survests_age, aes(x = age)) +
  
  geom_line(aes(y = smthmort30, colour = indicationGrp), size = 0.5) +
  geom_ribbon(aes(ymin = smthmort.ll30, ymax = smthmort.ul30, colour = NULL, fill = indicationGrp), alpha = 0.3) +
  geom_line(aes(y = 1 - exp(-30 * LThaz), linetype = "ONS national \nlife-table estimate"), linetype = "dashed", size = 0.2) +
  
  facet_grid(year ~ sex) +
  labs(x = "Age", y = "30-day mortality rate", colour = "Indication", linetype = NULL, fill = NULL, alpha = NULL) +
  guides(fill = FALSE, alpha = FALSE) +
  
  scale_x_continuous(breaks = seq(20, 100, 20)) +
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    # ,limits=(c(0.0001,1))
  ) +
  annotation_logticks(sides = "l", short = unit(.5, "mm"), mid = unit(1, "mm"), long = unit(2, "mm"), size = 0.1) +
  coord_cartesian(xlim = c(15, 100), ylim = c(0.00001, 1)) +
  legendanno(100, log10(c(0.005, 0.0005, 0.00005))) +
  surv_theme
plotmortbyage30
ggsave("./output/figures/sex-year-indication/logmort30byage.png", plotmortbyage30, width = 15, height = 20, units = "cm")






plotmortratiobyage365 <-
  ggplot(data = survests_age, aes(x = age)) +
  
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.2) +
  geom_line(aes(y = smthmort365 / (1 - exp(-365 * LThaz)), colour = indicationGrp), size = 0.5) +
  geom_ribbon(aes(ymin = smthmort.ll365 / (1 - exp(-365 * LThaz)), ymax = smthmort.ul365 / (1 - exp(-365 * LThaz)), colour = NULL, fill = indicationGrp), alpha = 0.3) +
  # geom_line(aes(y=1-exp(-30*LThaz),linetype='ONS national \nlife-table estimate'))+
  
  facet_grid(year ~ sex) +
  labs(x = "Age", y = "1-year mortality rate ratio", colour = "Indication", linetype = NULL, fill = NULL, alpha = NULL) +
  guides(fill = FALSE, alpha = FALSE) +
  scale_x_continuous(breaks = seq(20, 100, 20)) +
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    # ,limits=(c(0.0001,1))
  ) +
  annotation_logticks(sides = "l", short = unit(.5, "mm"), mid = unit(1, "mm"), long = unit(2, "mm"), size = 0.1) +
  coord_cartesian(xlim = c(15, 100), ylim = c(0.1, 10000)) +
  legendanno(100, log10(c(10000, 1000, 100))) +
  surv_theme
plotmortratiobyage365
ggsave("./output/figures/sex-year-indication/logmort365ratiobyage.png", plotmortratiobyage365, width = 15, height = 20, units = "cm")


plotmortratiobyage30 <-
  ggplot(data = survests_age, aes(x = age)) +
  
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.2) +
  geom_line(aes(y = smthmort30 / (1 - exp(-30 * LThaz)), colour = indicationGrp), size = 0.5) +
  geom_ribbon(aes(ymin = smthmort.ll30 / (1 - exp(-30 * LThaz)), ymax = smthmort.ul30 / (1 - exp(-30 * LThaz)), colour = NULL, fill = indicationGrp), alpha = 0.3) +
  # geom_line(aes(y=1-exp(-30*LThaz),linetype='ONS national \nlife-table estimate'))+
  
  facet_grid(year ~ sex) +
  labs(x = "Age", y = "30-day mortality rate ratio", colour = "Indication", linetype = NULL, fill = NULL, alpha = NULL) +
  guides(fill = FALSE, alpha = FALSE) +
  scale_x_continuous(breaks = seq(20, 100, 20)) +
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    # ,limits=(c(0.0001,1))
  ) +
  annotation_logticks(sides = "l", short = unit(.5, "mm"), mid = unit(1, "mm"), long = unit(2, "mm"), size = 0.1) +
  coord_cartesian(xlim = c(15, 100), ylim = c(0.1, 10000)) +
  legendanno(100, log10(c(10000, 1000, 100))) +
  surv_theme
plotmortratiobyage30
ggsave("./output/figures/sex-year-indication/logmort30ratiobyage.png", plotmortratiobyage30, width = 15, height = 20, units = "cm")







plothazs <-
  survests_time %>%
  ggplot(aes(x = time, colour = indicationGrp, fill = indicationGrp)) +
  
  geom_line(aes(y = exp.haz), linetype = "dashed", size = 0.2) +
  # geom_point(aes(y=haz), size=0.1, alpha=0.3)+
  geom_line(aes(y = haz_smth), size = 0.5) +
  geom_ribbon(aes(ymin = haz.ll_smth, ymax = haz.ul_smth, colour = NULL), alpha = 0.3) +
  facet_grid(year ~ sex) +
  
  labs(
    y = "Hazard rate"
    # ,subtitle="(formula goes here)"
    , x = "Days since procedure"
    # ,y = NULL
  ) +
  
  scale_x_continuous(breaks = seq(0, 365, 100)) +
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    , limits = (c(0.00001, 0.1))
  ) +
  annotation_logticks(sides = "l", short = unit(.5, "mm"), mid = unit(1, "mm"), long = unit(2, "mm"), size = 0.1) +
  
  legendanno(360, log10(c(0.1, 0.01, 0.001))) +
  surv_theme

plothazs
ggsave("./output/figures/sex-year-indication/hazard.png", plothazs, width = 15, height = 20, units = "cm")



plothazratios <-
  survests_time %>%
  ggplot(aes(x = time, colour = indicationGrp, fill = indicationGrp)) +
  
  geom_hline(aes(yintercept = 1), colour = "black", linetype = "dashed", size = 0.2) +
  # geom_point(aes(y=rat.haz), size=0.1, alpha=0.3)+
  
  geom_line(aes(y = rat.haz_smth), size = 0.5) +
  geom_ribbon(aes(ymin = rat.haz.ll_smth, ymax = rat.haz.ul_smth, colour = NULL), alpha = 0.3) +
  facet_grid(year ~ sex) +
  
  labs(
    y = "Hazard rate ratio"
    # ,subtitle="(formula goes here)"
    , x = "Days since procedure"
    # ,y = NULL
  ) +
  
  scale_x_continuous(breaks = seq(0, 365, 100)) +
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x)
    , labels = trans_format("log10", scales::math_format(10^.x))
    , limits = (c(0.3, 1000))
  ) +
  annotation_logticks(sides = "l", short = unit(.5, "mm"), mid = unit(1, "mm"), long = unit(2, "mm"), size = 0.1) +
  
  legendanno(360, log10(c(1000, 100, 10))) +
  surv_theme

plothazratios
ggsave("./output/figures/sex-year-indication/hazard ratio.png", plothazratios, width = 15, height = 20, units = "cm")





plothazdiffs <-
  survests_time %>%
  ggplot(aes(x = time + 1, colour = indicationGrp, fill = indicationGrp)) +
  
  # geom_point(aes(y=haz-exp.haz), size=0.1, alpha=0.3)+
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 0.2) +
  geom_line(aes(y = haz_smth - exp.haz), size = 0.5) +
  geom_ribbon(aes(ymin = haz.ll_smth - exp.haz, ymax = haz.ul_smth - exp.haz, colour = NULL), alpha = 0.3) +
  
  facet_grid(year ~ sex) +
  labs(
    y = "Hazard rate difference (additive excess hazard)"
    # ,subtitle="(formula goes here)"
    , x = "Days since procedure"
    # ,y = NULL
  ) +
  
  scale_x_continuous(breaks = seq(0, 365, 100)) +
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  
  
  legendanno(360, (c(0.07, 0.05, 0.03))) +
  surv_theme

plothazdiffs
ggsave("./output/figures/sex-year-indication/hazard diff.png", plothazdiffs, width = 15, height = 20, units = "cm")


plotcmlhazs <-
  survests_time %>%
  ggplot(aes(x = time, colour = indicationGrp, fill = indicationGrp)) +
  
  geom_line(aes(y = exp.cml.haz), linetype = "dashed", size = 0.2) +
  geom_step(aes(y = cml.haz), size = 0.5) +
  geom_rect(aes(xmin = time, xmax = leadtime, ymin = cml.haz.ll, ymax = cml.haz.ul, fill = indicationGrp), alpha = 0.3, colour = NA) +
  
  
  facet_grid(year ~ sex) +
  labs(
    y = "Cumulative hazard rate"
    # ,subtitle="(formula goes here)"
    , x = "Days since procedure"
    # ,y = NULL
  ) +
  
  scale_x_continuous(breaks = seq(0, 365, 100)) +
  scale_y_continuous(limits = c(-.01, 0.2)) +
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  
  legendanno(360, (c(0.2, 0.165, 0.13))) +
  surv_theme
plotcmlhazs
ggsave("./output/figures/sex-year-indication/cml haz.png", plotcmlhazs, width = 15, height = 20, units = "cm")



plotcmlexchazs <-
  survests_time %>%
  ggplot(aes(x = time, colour = indicationGrp, fill = indicationGrp)) +
  
  geom_step(aes(y = cml.exs.haz), size = 0.5) +
  geom_rect(aes(xmin = time, xmax = leadtime, ymin = cml.exs.haz.ll, ymax = cml.exs.haz.ul, fill = indicationGrp), alpha = 0.3, colour = NA) +
  
  
  facet_grid(year ~ sex) +
  labs(
    y = "Cumulative excess hazard rate"
    # ,subtitle="(formula goes here)"
    , x = "Days since procedure"
    # ,y = NULL
  ) +
  
  scale_x_continuous(breaks = seq(0, 365, 100)) +
  scale_y_continuous(limits = c(-.01, 0.2)) +
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  
  legendanno(360, (c(0.2, 0.165, 0.13))) +
  surv_theme
plotcmlexchazs
ggsave("./output/figures/sex-year-indication/cml excess haz.png", plotcmlexchazs, width = 15, height = 20, units = "cm")





########################################################################################################################
## As in Glen Martin's RS paper
########################################################################################################################


plotmartin <-
  survests_time %>%
  filter(
    time < 366
    # ,lvl(sex)==1
    # ,year==2007
  ) %>%
  ggplot(aes(colour = indicationGrp, fill = indicationGrp)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = "black", linetype = "dashed") +
  geom_step(aes(x = 1 - exp.surv, y = 1 - surv), size = 0.1, alpha = 0.3) +
  geom_rect(aes(xmin = lag(1 - exp.surv, n = 1, default = 0), xmax = 1 - exp.surv, ymin = 1 - surv.ul, ymax = 1 - surv.ll, fill = indicationGrp), alpha = 0.3, colour = NA) +
  
  facet_grid(year ~ sex) +
  labs(x = "Expected cumulative mortlaity", y = "Cumulative observed mortality estimate") +
  scale_colour_manual(values = colourpal) +
  scale_fill_manual(values = colourpal) +
  
  theme_bw(base_size = 15) +
  guides(colour = guide_legend(title = "Indication"), linetype = FALSE, alpha = FALSE, fill = FALSE, point = FALSE, size = FALSE) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) #+theme(legend.position="none")
plotmartin


plotmartin <-
  survests_time %>%
  filter(
    time < 366
    # ,lvl(sex)==1
    # ,year==2007
  ) %>%
  ggplot(aes(colour = year, group = year)) +
  geom_abline(aes(intercept = 0, slope = 1), colour = "black", linetype = "dashed") +
  geom_step(aes(x = 1 - exp.surv, y = 1 - surv), size = 0.1, alpha = 0.3) +
  
  facet_grid(indicationGrp ~ sex) +
  labs(x = "Expected cumulative mortlaity", y = "Cumulative observed mortality estimate") +
  
  theme_bw(base_size = 15) +
  guides(colour = guide_legend(title = "Indication"), linetype = FALSE, alpha = FALSE, fill = FALSE, point = FALSE, size = FALSE) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) #+theme(legend.position="none")
plotmartin

