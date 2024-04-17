# Creating tensor spline plots for 4 outcomes
# Date Created: 2/13/24 
# Author: Jennifer Cano


library(tidyverse)
library(haven)
library(survival)
library(rms) 
library(dplyr)
library(visreg)

setwd("filepath")

#import SAS dataset of primary cohort and save 
primary = read_sas("happi_primarycoh_labsdisch.sas7bdat")

coh = primary
dd = datadist(coh)
options(datadist="dd")
#warnings()

#Cox proportional hazards model

#90-day mort

mort = cph(Surv(time2mort_new, follby_mort90 == 1) ~ 
           rcs(final_mon_hosp, 4)*rcs(final_neut_hosp, 4), 
         data = coh, x=TRUE, y=TRUE, surv=TRUE, time.inc=5)


#Plots 

pdf(file="filepath", width = 7, height = 5, pointsize = 10)
tsp_mort = visreg2d(mort, "final_mon_hosp", "final_neut_hosp", 
                  plot.type="persp", ylab = "\n\nANC(10\u00b3/\u00b5l)",
                  xlab = "\nAMC(10\u00b3/\u00b5l)", zlab = "\n\n\nRelative Hazard \nRatio",
                  trans= function(x){exp(x)}, nn=20, zlim = c(0, 5.5))
dev.off()


