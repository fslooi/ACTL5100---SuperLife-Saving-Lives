History_data = read.csv("Historical data with and without additional products.csv")

History_data_T20 = History_data[History_data$Policy.type == "T20",]
History_data_SPWL = History_data[History_data$Policy.type == "SPWL",]

yrs = unique(History_data$ISSUE_YEAR)

mean_yrs_0_T20 = c()
mean_yrs_1_T20 = c()

mean_yrs_0_SPWL = c()
mean_yrs_1_SPWL = c()

for(i in yrs){
  mean_yrs_0_T20 = c(mean_yrs_0_T20, mean(History_data_T20$PROFIT0[History_data_T20$ISSUE_YEAR == i]))
  mean_yrs_1_T20 = c(mean_yrs_1_T20, mean(History_data_T20$PROFIT1[History_data_T20$ISSUE_YEAR == i]))
  
  mean_yrs_0_SPWL = c(mean_yrs_0_SPWL, mean(History_data_SPWL$PROFIT0[History_data_SPWL$ISSUE_YEAR == i]))
  mean_yrs_1_SPWL = c(mean_yrs_1_SPWL, mean(History_data_SPWL$PROFIT1[History_data_SPWL$ISSUE_YEAR == i]))
}

plot(yrs, mean_yrs_0_SPWL, type = "h")

lines(yrs, mean_yrs_0_T20, type = "h",col="red")
lines(yrs, mean_yrs_1_T20, col = "red")

mean_yrs_0_T20 = c()
mean_yrs_1_T20 = c()

mean_yrs_0_SPWL = c()
mean_yrs_1_SPWL = c()

for(i in yrs){
  mean_yrs_0_T20 = c(mean_yrs_0_T20, sum(History_data_T20$PROFIT0[History_data_T20$ISSUE_YEAR == i]))
  mean_yrs_1_T20 = c(mean_yrs_1_T20, sum(History_data_T20$PROFIT1[History_data_T20$ISSUE_YEAR == i]))
  
  mean_yrs_0_SPWL = c(mean_yrs_0_SPWL, sum(History_data_SPWL$PROFIT0[History_data_SPWL$ISSUE_YEAR == i]))
  mean_yrs_1_SPWL = c(mean_yrs_1_SPWL, sum(History_data_SPWL$PROFIT1[History_data_SPWL$ISSUE_YEAR == i]))
}


Sensitivity_data = read.csv("Sensitivity.CSV")
Sensitivity_data = cbind(History_data$Policy.type, History_data$ISSUE_YEAR, Sensitivity_data)

Sensitivity_data_T20 = Sensitivity_data[Sensitivity_data$`History_data$Policy.type` == "T20",]
Sensitivity_data_SPWL = Sensitivity_data[Sensitivity_data$`History_data$Policy.type` != "T20",]

mean_interest_lower_with_T20 = c()
mean_interest_upper_with_T20 = c()
mean_mortality_lower_with_T20 = c()
mean_mortality_upper_with_T20 = c()

mean_interest_lower_with_SPWL = c()
mean_interest_upper_with_SPWL = c()
mean_mortality_lower_with_SPWL = c()
mean_mortality_upper_with_SPWL = c()

mean_interest_lower_without_T20 = c()
mean_interest_upper_without_T20 = c()
mean_mortality_lower_without_T20 = c()
mean_mortality_upper_without_T20 = c()

mean_interest_lower_without_SPWL = c()
mean_interest_upper_without_SPWL = c()
mean_mortality_lower_without_SPWL = c()
mean_mortality_upper_without_SPWL = c()



for(i in yrs){
  mean_interest_lower_with_T20 = c(mean_interest_lower_with_T20, mean(Sensitivity_data_T20$Interest_lower_bound_with[Sensitivity_data_T20$`History_data$ISSUE_YEAR`==i]))
  mean_interest_lower_with_SPWL = c(mean_interest_lower_with_SPWL, mean(Sensitivity_data_SPWL$Interest_lower_bound_with[Sensitivity_data_SPWL$`History_data$ISSUE_YEAR`==i]))
  
  mean_mortality_lower_with_T20 = c(mean_mortality_lower_with_T20, mean(Sensitivity_data_T20$Mortality_lower_bound_with[Sensitivity_data_T20$`History_data$ISSUE_YEAR`==i]))
  mean_mortality_lower_with_SPWL = c(mean_mortality_lower_with_SPWL, mean(Sensitivity_data_SPWL$Mortality_lower_bound_with[Sensitivity_data_SPWL$`History_data$ISSUE_YEAR`==i]))
  
  mean_interest_upper_with_T20 = c(mean_interest_upper_with_T20, mean(Sensitivity_data_T20$Interest_upper_bound_with[Sensitivity_data_T20$`History_data$ISSUE_YEAR`==i]))
  mean_interest_upper_with_SPWL = c(mean_interest_upper_with_SPWL, mean(Sensitivity_data_SPWL$Interest_upper_bound_with[Sensitivity_data_SPWL$`History_data$ISSUE_YEAR`==i]))
  
  mean_mortality_upper_with_T20 = c(mean_mortality_upper_with_T20, mean(Sensitivity_data_T20$Mortality_upper_bound_with[Sensitivity_data_T20$`History_data$ISSUE_YEAR`==i]))
  mean_mortality_upper_with_SPWL = c(mean_mortality_upper_with_SPWL, mean(Sensitivity_data_SPWL$Mortality_upper_bound_with[Sensitivity_data_SPWL$`History_data$ISSUE_YEAR`==i]))
  
  
  
  mean_interest_lower_without_T20 = c(mean_interest_lower_without_T20, mean(Sensitivity_data_T20$Interest_lower_bound_without[Sensitivity_data_T20$`History_data$ISSUE_YEAR`==i]))
  mean_interest_lower_without_SPWL = c(mean_interest_lower_without_SPWL, mean(Sensitivity_data_SPWL$Interest_lower_bound_without[Sensitivity_data_SPWL$`History_data$ISSUE_YEAR`==i]))
  
  mean_mortality_lower_without_T20 = c(mean_mortality_lower_without_T20, mean(Sensitivity_data_T20$Mortality_lower_bound_without[Sensitivity_data_T20$`History_data$ISSUE_YEAR`==i]))
  mean_mortality_lower_without_SPWL = c(mean_mortality_lower_without_SPWL, mean(Sensitivity_data_SPWL$Mortality_lower_bound_without[Sensitivity_data_SPWL$`History_data$ISSUE_YEAR`==i]))
  
  mean_interest_upper_without_T20 = c(mean_interest_upper_without_T20, mean(Sensitivity_data_T20$Interest_upper_bound_without[Sensitivity_data_T20$`History_data$ISSUE_YEAR`==i]))
  mean_interest_upper_without_SPWL = c(mean_interest_upper_without_SPWL, mean(Sensitivity_data_SPWL$Interest_upper_bound_without[Sensitivity_data_SPWL$`History_data$ISSUE_YEAR`==i]))
  
  mean_mortality_upper_without_T20 = c(mean_mortality_upper_without_T20, mean(Sensitivity_data_T20$Mortality_upper_bound_without[Sensitivity_data_T20$`History_data$ISSUE_YEAR`==i]))
  mean_mortality_upper_without_SPWL = c(mean_mortality_upper_without_SPWL, mean(Sensitivity_data_SPWL$Mortality_upper_bound_without[Sensitivity_data_SPWL$`History_data$ISSUE_YEAR`==i]))
  
}


plot(yrs, mean_yrs_1_SPWL, type = "l", ylim = c(min(mean_interest_lower_with_SPWL), max(mean_interest_upper_with_SPWL)), col = "forestgreen", xlab = "Years", ylab = "Expected Profit", main = "Sensivity test for interest rate with Health Incentive Program (Whole-Life)")
lines(yrs, mean_interest_lower_with_SPWL, col="red")
lines(yrs, mean_interest_upper_with_SPWL, col="blue")
legend("bottomleft", legend = c("Interest Upper Bound", "Expected Interest", "Interest Lower Bound"), col = c('blue', "forestgreen", "red"), lwd = 2, xpd = T)

plot(yrs, mean_yrs_1_T20, type = "l", ylim = c(min(mean_interest_lower_with_T20), max(mean_interest_upper_with_T20)), col = "forestgreen", xlab = "Years", ylab = "Expected Profit", main = "Sensivity test for interest rate with Health Incentive Program (Term-Life)")
lines(yrs, mean_interest_lower_with_T20, col="red")
lines(yrs, mean_interest_upper_with_T20, col="blue")
legend("bottomleft", legend = c("Interest Upper Bound", "Expected Interest", "Interest Lower Bound"), col = c('blue', "forestgreen", "red"), lwd = 2, xpd = T)

plot(yrs, mean_yrs_1_T20, type = "l", ylim = c(min(mean_mortality_lower_with_T20), max(mean_mortality_upper_with_T20)), col = "forestgreen", xlab = "Years", ylab = "Expected Profit", main = "Sensivity test for survival rate with Health Incentive Program (Term-Life)")
lines(yrs, mean_mortality_lower_with_T20, col="red")
lines(yrs, mean_mortality_upper_with_T20, col="blue")
legend("bottomleft", legend = c("Survival Upper Bound", "Expected Survival", "Survival Lower Bound"), col = c('blue', "forestgreen", "red"), lwd = 2, xpd = T)

plot(yrs, mean_yrs_1_SPWL, type = "l", ylim = c(min(mean_mortality_lower_with_SPWL), max(mean_mortality_upper_with_SPWL)), col = "forestgreen", xlab = "Years", ylab = "Expected Profit", main = "Sensivity test for survival rate with Health Incentive Program (Whole-Life)")
lines(yrs, mean_mortality_lower_with_SPWL, col="red")
lines(yrs, mean_mortality_upper_with_SPWL, col="blue")
legend("topleft", legend = c("Survival Upper Bound", "Expected Survival", "Survival Lower Bound"), col = c('blue', "forestgreen", "red"), lwd = 2, xpd = T)





plot(yrs, mean_interest_lower_without_SPWL, type = "l", ylim = c(min(mean_interest_lower_without_SPWL), max(mean_interest_upper_without_SPWL)), col = "red", xlab = "Years", ylab = "Expected Profit", main = "Sensivity test for interest rate without Health Incentive Program (Whole-Life)")
abline(h = 0, col = "forestgreen")
lines(yrs, mean_interest_upper_without_SPWL, col="blue")
legend("bottomleft", legend = c("Interest Upper Bound", "Expected Interest", "Interest Lower Bound"), col = c('blue', "forestgreen", "red"), lwd = 2, xpd = T)

plot(yrs, mean_interest_lower_without_T20, type = "l", ylim = c(min(mean_interest_lower_without_T20), max(mean_interest_upper_without_T20)), col = "red", xlab = "Years", ylab = "Expected Profit", main = "Sensivity test for interest rate without Health Incentive Program (Term-Life)")
abline(h = 0, col = "forestgreen")
lines(yrs, mean_interest_upper_without_T20, col="blue")
legend("bottomleft", legend = c("Interest Upper Bound", "Expected Interest", "Interest Lower Bound"), col = c('blue', "forestgreen", "red"), lwd = 2, xpd = T)

par(mar = c(7, 4.1,4.1, 2.1))
plot(yrs, mean_mortality_lower_without_SPWL, type = "l", ylim = c(min(mean_mortality_lower_without_SPWL), max(mean_mortality_upper_without_SPWL)), col = "red", xlab = "Years", ylab = "Expected Profit", main = "Sensivity test for survival rate without Health Incentive Program (Whole-Life)")
abline(h = 0, col = "forestgreen")
lines(yrs, mean_mortality_upper_without_SPWL, col="blue")
legend("bottom", inset = c(0, -0.30), legend = c("Interest Upper Bound", "Expected Interest", "Interest Lower Bound"), col = c('blue', "forestgreen", "red"), lwd = 2, xpd = T, horiz = T)

plot(yrs, mean_mortality_lower_without_T20, type = "l", ylim = c(min(mean_mortality_lower_without_T20), max(mean_mortality_upper_without_T20)), col = "red", xlab = "Years", ylab = "Expected Profit", main = "Sensivity test for survival rate without Health Incentive Program (Term-Life)")
abline(h = 0, col = "forestgreen")
lines(yrs, mean_mortality_upper_without_T20, col="blue")
legend("bottom", inset = c(0, -0.30), legend = c("Interest Upper Bound", "Expected Interest", "Interest Lower Bound"), col = c('blue', "forestgreen", "red"), lwd = 2, xpd = T, horiz = T)
on.exit(par(opar))

