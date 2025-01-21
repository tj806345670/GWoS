## for CMA analysis
library(mediation)
library(tidyr)
load('alldata.RData')
model1<-lm(med~OC,data = tmpdata)
model2<-lm(pheno~OC+med+OC:med+FSES+ASES+CSES+AP+SC,data = tmpdata)  
causm<-mediation::mediate(model1,model2,treat = 'OC',mediator = 'med', conf.level=.95, sims=10000,treat.value = 1,control.value =0)
Tint<-mediation::test.TMint(causm) #  test for treatment-mediator moderation/interaction
