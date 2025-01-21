############ PSM to 1:1 create OC and non-OC pairs
load("/impdataR2.RData")
library(MatchIt)
library(tidyr)
library(openxlsx)
set.seed(123)
model <-matchit(OC ~  PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+sex+age_covs+agesex+edu_covs+BMI+TIV+mean_FD,data = meanimp,method ="nearest" ,caliper =0.2, distance ="bart")
summary(model)
matched_samples <- match.data(model)
rownames(matched_samples)<-NULL
matched2 <- reshape(matched_samples, direction = "wide", idvar = "subclass", timevar = "OC")
