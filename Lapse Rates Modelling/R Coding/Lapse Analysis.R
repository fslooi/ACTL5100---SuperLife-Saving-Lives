setwd("/Users/fslooi/Documents/UNSW/Term 1 2024/ACTL5100/SOA Case Study/Lapse")

library(readr)
library(survival)
library(ggplot2)
library(ggfortify)
rawdataset <- read_csv("2024-srcsc-superlife-inforce-dataset.csv")
modeldataset = rawdataset[rawdataset$Policy.type == "T20",]
modeldataset$Death.indicator = as.numeric(modeldataset$Death.indicator)
modeldataset$Lapse.Indicator = as.numeric(modeldataset$Lapse.Indicator)
modeldataset$Death.indicator[is.na(modeldataset$Death.indicator)] = 0
modeldataset$Lapse.Indicator[is.na(modeldataset$Lapse.Indicator)] = 0
modeldataset$Censor.Indictor = 0
modeldataset$Censor.Indictor[modeldataset$Death.indicator == 1] = 1
modeldataset$SurvYears = 0
#Upper round the years from issue to death or lapse
modeldataset$SurvYears[modeldataset$Death.indicator == 1] = modeldataset$Year.of.Death[modeldataset$Death.indicator == 1] - modeldataset$Issue.year[modeldataset$Death.indicator == 1] + 1 
modeldataset$SurvYears[modeldataset$Lapse.Indicator == 1] = modeldataset$Year.of.Lapse[modeldataset$Lapse.Indicator == 1] - modeldataset$Issue.year[modeldataset$Lapse.Indicator == 1] + 1 
modeldataset$Lapse.Indicator[modeldataset$SurvYears == 20] = 0#For the surv function, status 0 means not choosing to lapse, including term end lapse; 1 means choosing to lapse
km_fit <- survfit(Surv(modeldataset$SurvYears, modeldataset$Lapse.Indicator) ~ 1)
summary(km_fit)
autoplot(km_fit, main = "Kaplan-Meier Estimator Plot for Policyholder Lapsing Behaviour", ylab = 'Survival Probability', xlab = "Time (Year)")

cox <- coxph(Surv(SurvYears, Lapse.Indicator) ~ Issue.age + Sex +Face.amount + Smoker.Status + Underwriting.Class + Urban.vs.Rural + Region + Distribution.Channel,data = modeldataset)
summary(cox)


predict(cox, newdata = modeldataset,type = "survival")
modeldataset$predsurv = predict(cox, newdata = modeldataset, type = "expected")
#predicted probabilities of the persons not choosing to lapse before their holding time as given in csv
modeldataset$p.not.lapse = predict(cox, newdata = modeldataset, type = "survival")


#Expected time before lapse. It has a positive bias as the data was collected after being rounded up.
modeldataset2 = modeldataset
#By this way the Cox model can predict the probability of a person lapsing in 1 year
modeldataset2$SurvYears = 1 #By changing the SurvYears we predict the probability of lapsing in n years.
modeldataset$p.oneyear = 1 - predict(cox, newdata = modeldataset2,type = "survival")