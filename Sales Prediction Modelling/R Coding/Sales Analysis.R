#linear models of customer numbers
table = data.frame(table(rawdataset$Issue.year,rawdataset$Policy.type))
table$Var1 = as.integer(table$Var1)
spwlbiao = table[table$Var2 == "SPWL",]
t20biao = table[table$Var2 == "T20",]
plot(spwlbiao$Var1, spwlbiao$Freq, main = "Linear Regression Model for SPWL Policies", ylab = "No. of SPWL Customers", xlab = "Year", pch = 16)
abline(lm(Freq~Var1, data = spwlbiao), col = "maroon", lwd = 2)
plot(t20biao$Var1,t20biao$Freq, main = "Linear Regression Model for T20 Policies", ylab = "No. of T20 Customers", xlab = "Year", pch = 16)
abline(lm(Freq~Var1, data = t20biao), col = "darkblue", lwd = 2)
Flmwl = lm(Freq~Var1, data = spwlbiao)
Flmwl
lmtm = lm(Freq~Var1, data = t20biao)
lmtm