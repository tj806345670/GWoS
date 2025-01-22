############ multiple imputation 
rm(list = ls())
library(mice)
library(tidyr)
library(openxlsx)
set.seed(22112)
envcogdata<-read.xlsx('/envcogdata.xlsx')
test<-list()
ignore <- caret::createFolds(y=1:dim(envcogdata)[1],k=10)### 分成10份
allind<-as.numeric(unlist(ignore))
for (i in 1:10) {
  imp <- mice(envcogdata[,-1],maxit = 20, m = 10, ignore = as.numeric(rownames(envcogdata)) %in% ignore[[i]], print = F, seed = 22112)
  test[[paste0('Fold',i)]] <- filter(imp, as.numeric(rownames(envcogdata)) %in% ignore[[i]])
}
# get 10 complete datasets
impdata<-list()
for (im in 1:10) {
  tmpc<-mice::complete(test[["Fold1"]], im)
  for (f in 2:10) {
    tmp<-mice::complete(test[[paste0('Fold',f)]], im)
    tmpc<- rbind(tmpc,tmp)
  }
  tmpc<-data.frame(subid= envcogdata[allind,'subid'], tmpc [,])
  tmpc<-tmpc[order(tmpc$subid),]
  impdata[[im]]<-tmpc
}
# check
sum(impdata[[1]][["sex"]]- envcogdata$sex)
########### mean imputed dataset 
meanimp<-impdata[[1]]
for ( i in 2:length(impdata)) {
  meanimp<-meanimp+impdata[[i]]
}
save.image("/impdataR2.RData")

  



