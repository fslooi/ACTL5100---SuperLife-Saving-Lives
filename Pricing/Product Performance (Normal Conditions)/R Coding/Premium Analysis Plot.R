library(readr)
min_premium <- read_csv("min_premium_fuben.csv")

premium_spwl = min_premium[min_premium$Policy.type != "T20",]
premium_t20 = min_premium[min_premium$Policy.type == "T20",]

years = 2001:2023
data <- data.frame(Year = years)
data$average = 0
data$averageBase = 0
for (i in 2001:2023){
  calcdf = premium_spwl[premium_spwl$ISSUE_YEAR == i,]
  a = mean(calcdf$PremiumFace0)
  b = mean(calcdf$PremiumFaceBase)
  data$average[i-2000] = a
  data$averageBase[i-2000] = b
}

datat20 = data.frame(Year = years)
datat20$average = 0
datat20$averageBase = 0

for (i in 2001:2023){
  calcdf = premium_t20[premium_t20$ISSUE_YEAR == i,]
  a = mean(calcdf$PremiumFace0)
  b = mean(calcdf$PremiumFaceBase)
  datat20$average[i-2000] = a
  datat20$averageBase[i-2000] = b
}

#spwlplot
plot(data$Year, data$average, type = "l", col = "blue", pch = 16, ylim = range(c(data$average, data$averageBase)), xlab = "Year", ylab = "Average premium per face amount",main = " SPWL Average premium per face amount over year")
lines(data$Year, data$averageBase, col = "red", pch = 16)
legend("bottomleft", y = c("Current Premium","Least Premium"), col = c("red","blue"), lty = 1)

#T20plot
plot(datat20$Year, datat20$average, type = "l", col = "blue", pch = 16, ylim = range(c(datat20$average, datat20$averageBase)), xlab = "Year", ylab = "Average premium per face amount over year",main = "T20 Average premium per face amount over year")
lines(datat20$Year, datat20$averageBase, col = "red", pch = 16)
legend("bottomleft", y = c("Current Premium","Least Premium"), col = c("red","blue"), lty = 1)

write_csv(data,"spwlaverage.csv")
write_csv(datat20,"t20average.csv")
