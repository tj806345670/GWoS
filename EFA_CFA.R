library(psych)
library(lavaan)
library(openxlsx)
library(tidyr)
load('envdata.RData')
envcor<-cor(ddataforCMA[,2:34],use='pairwise.complete.obs')
KMO(envcor)#>0.8 is suitable for EFA  
cortest.bartlett(envcor,7013,diag=TRUE)
psych::vss(dataforCMA[,2:34],plot=TRUE, rotate = "varimax", diagonal = FALSE, fm = "pa")
env.varimax2<-fa(dataforCMA[,2:34],nfactors=5,max.iter = 1000,fm = "pa",rotate='varimax')# 5 is suitable
env.varimax2$loadings
## CFA model
#### ten-fold cross prediction
folds <- caret::createFolds(y=1:dim(envdata)[1],k=10)###分成10份
score_CFA<-data.frame()
fm<-matrix(0,10,3)
for (i in 1:10) {
  cfamod<-'
  FSES =~ ME+PE+PO+MO+FR+ITN+UrS
  ASES =~FID+FUS+FIC+HI
  CSES =~ EE+GDP+HB
   SC =~ MCA+FAS
  AP=~ a*NO2+a*PM2.5
  '  
  fit.cfamod<-cfa(cfamod,data = dataforCMA[-c(folds[[i]]),]) 
  fm[i,]<-t(as.vector(fitmeasures(fit.cfamod, c( "cfi", "rmsea",'srmr'))))
  estimate<-parameterestimates(fit.cfamod,boot.ci.type = 'bca.simple',standardized = TRUE,rsquare=T)
  score_CFA[folds[[i]],1:5]<-as.data.frame(lavPredict(fit.cfamod,dataforCMA[folds[[i]],]))
}
fm<-as.data.frame(fm)
names(fm)<-c( "cfi","rmsea",'srmr')
