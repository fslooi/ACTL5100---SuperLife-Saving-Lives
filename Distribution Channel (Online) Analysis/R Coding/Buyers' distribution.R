data = read.csv("2024-srcsc-superlife-inforce-dataset.csv")

data = data[-c(1,2),]

colnames(data) = data[1,]

data = data[-1,]

yr = unique(data$Issue.year)
age = sort(unique(data$Issue.age))

count_yr = c()
totol_yr = c()
for (i in yr) {
  count_yr = c(count_yr, sum(data$Distribution.Channel[data$Issue.year == i] == "Online"))
  totol_yr = c(totol_yr, length(data$Distribution.Channel[data$Issue.year == i]))
}

plot(yr, totol_yr, type = "l", main ="Number of Buyers by Years", xlab = "Years", ylab = "Number of Buyers", ylim = c(0,100000), col = '#FFA900', lwd = 2)
lines(yr, count_yr, col = 'darkblue', lwd = 2)
legend("topright", legend = c("Total Buyers", "Online Buyers"), col = c('#FFA900', "darkblue"), lwd = 2)

plot(yr, count_yr/totol_yr, col="red",type = "l", lwd = 2, xlab = "Years", ylab = "Proportion", main = "Proportion of Policyholders Purchasing Online")

count_age = c()
totol_age = c()

for (i in age) {
  count_age = c(count_age, sum(data$Distribution.Channel[data$Issue.age == i] == "Online"))
  totol_age = c(totol_age, length(data$Distribution.Channel[data$Issue.age == i]))
}

plot(age, count_age/totol_age, col="red",type = "l", lwd = 2, xlab = "Ages", ylab = "Proportion", main = "Proportion of Policyholders Purchasing Online")

par(mar = c(5, 5, 4, 6.5))
plot(age, totol_age, type = "l", main ="Number of Buyers by Ages", xlab = "Age", ylab = "Number of Buyers", ylim = c(0, 40000),col = '#FFA900', lwd = 2)
lines(age, count_age, col = 'darkblue', lwd = 2)
legend("topright", inset = c(-0.35, 0 ), legend = c("Total Buyers", "Online Buyers"), col = c('#FFA900', "darkblue"), lwd = 2, xpd = T)

