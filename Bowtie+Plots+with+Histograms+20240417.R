
# Creating bowtie Hazard ratio plots for 4 outcomes
# Date Created: 2/5/24 
# Author: Jennifer Cano

library(tidyverse)
#library(rlang)
library(haven)
library(gmodels)
library(survival)
library(rms)
library(splines)
library(Greg)
library(survminer)
library(dplyr)
library(ggplot2)
library(scales)
library(pspline)

setwd("filepath")

#import SAS dataset of primary cohort and save 
primary = read_sas("happi_primarycoh_labsdisch.sas7bdat")

coh = primary
dd = datadist(coh)
options(datadist="dd")
#warnings()


#Cox proportional hazards model

#90-day mort

#Neut
modneut_mort = cph(Surv(time2mort_new, follby_mort90 == 1) ~ rcs(final_neut_hosp, 4),
                 data = coh, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)

#Mon
modmon_mort = cph(Surv(time2mort_new, follby_mort90 == 1) ~ rcs(final_mon_hosp, 4),
                data = coh, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)

################################################

#Bowtie plots with histograms


###Neut

#Hazard ratio plot
#90-day mort
neut_mort = ggplot(Predict(modneut_mort, final_neut_hosp,fun=exp), xlab = "\nAbsolute Neutrophil Count (10\u00b3/\u00b5l)",
                 ylab = "Relative Hazard Ratio (95% CI) \n90-day Mortality ") +
  scale_x_continuous(breaks=seq(0,20,5)) + 
  coord_cartesian(ylim = c(0.5,3.5)) +
  geom_hline(yintercept = 1, linetype = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(vjust = -2, size=16), axis.text.y = element_text(hjust = -2, size=16),
        text = element_text(size=16))  +
  annotate("rect", xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = Inf, alpha = .2) +
  annotate("rect", xmin = 8, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .2)


#create character string of name of lab column for histogram
xvar_n = "final_neut_hosp"

#histogram
g2_n = ggplot2::ggplot(coh, ggplot2::aes(x = !!rlang::parse_expr(xvar_n))) +
  ggplot2::geom_histogram(fill= "black", bins = 101) +
  ggplot2::scale_x_continuous(limits = c(0,20), breaks = seq(0, 20, by = 5)) +
  ggplot2::xlab("") +
  ggplot2::ylab("") +
  ggplot2::theme_minimal() +
  ggeasy::easy_remove_y_axis() +
  ggeasy::easy_remove_x_axis() +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 0.1)

#combine plots
layout = c(patchwork::area(t = 1, b = 10, l = 1, r = 10),
           patchwork::area(t = 11, b = 12, l = 1, r = 10))


pdf(file="filepath", width = 8.5, height = 7, pointsize = 10)
neut_mort/g2_n
dev.off()


###Monocyte

#Hazard ratio plot
#90-day mort
mon_mort = ggplot(Predict(modmon_mort, final_mon_hosp,fun=exp), xlab = "\nAbsolute Monocyte Count (10\u00b3/\u00b5l)",
                ylab = "Relative Hazard Ratio (95% CI) \n90-day Mortality ") +
  scale_x_continuous(breaks=seq(0,3,.5)) + 
  coord_cartesian(ylim = c(0.5,3.5)) + 
  geom_hline(yintercept = 1, linetype = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(vjust = -2, size=16), axis.text.y = element_text(hjust = -2, size=16),
        text = element_text(size=16))  +
  annotate("rect", xmin = -Inf, xmax = 0.2, ymin = -Inf, ymax = Inf, alpha = .2) +
  annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .2)


#create character string of name of lab column for histogram
xvar_m = "final_mon_hosp"

#histogram
g2_m = ggplot2::ggplot(coh, ggplot2::aes(x = !!rlang::parse_expr(xvar_m))) +
  ggplot2::geom_histogram(fill= "black", bins = 26) +
  ggplot2::scale_x_continuous(limits = c(0,2.5), breaks = seq(0, 3, by = .1)) +
  ggplot2::xlab("") +
  ggplot2::ylab("") +
  ggplot2::theme_minimal() +
  ggeasy::easy_remove_y_axis() +
  ggeasy::easy_remove_x_axis() +
  ggplot2::theme_void() +
  ggplot2::theme(aspect.ratio = 0.1)

#combine plots
layout = c(patchwork::area(t = 1, b = 10, l = 1, r = 10),
           patchwork::area(t = 11, b = 12, l = 1, r = 10))



pdf(file="filepath", width = 8.5, height = 7, pointsize = 10)
mon_mort/g2_m
dev.off()


