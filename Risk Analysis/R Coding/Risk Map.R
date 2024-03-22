#SOA Case Challenge 2024

# Risk and Risk Management
# Create an empty plot
plot(1, type = 'n', xlim = c(0, 5.5), ylim = c(0, 5.7), main = 'Risk Map - Health Incentive Program', xlab = 'Likelihood', ylab = 'Severity')
for (i in 0:5){
  abline(h = i, lty = 2,  col = "darkgrey")
  abline(v = i, lty = 2,  col = "darkgrey")
}


# Draw a circle
# Climate
symbols(0.8, 4, circles = 1, add = TRUE, inches = 0.2, bg = "#FDA900", lwd=2)
text(0.8, 4, '1', col = "black", cex = 1)
# Inflation
symbols(3, 1.93, circles = 1, add = TRUE, inches = 0.2, bg = "#FDA900", lwd=2)
text(3, 1.93, '2', col = "black", cex = 1)
# Model
symbols(1, 3, circles = 1, add = TRUE, inches = 0.2, bg = "#FDA900", lwd=2)
text(1, 3, '3', col = "black", cex = 1)
# Mortality
symbols(2, 3, circles = 1, add = TRUE, inches = 0.2, bg = "#FDA900", lwd=2)
text(2, 3, '4', col = "black", cex = 1)
# Pandemic
symbols(1, 5, circles = 1, add = TRUE, inches = 0.2, bg = "#FDA900", lwd=2)
text(1, 5, '5', col = "black", cex = 1)
# Regulatory
symbols(3, 3.07, circles = 1, add = TRUE, inches = 0.2, bg = "#FDA900", lwd=2)
text(3, 3.07, '6', col = "black", cex = 1)
# War
symbols(1.2, 4, circles = 1, add = TRUE, inches = 0.2, bg = "#FDA900", lwd=2)
text(1.2, 4, '7', col = "black", cex = 1)

legend('topright',  legend = c('1: Climate', '2: Inflation', '3: Model', '4: Mortality', '5: Pandemic', '6: Regulatory', '7: War'), cex = 0.8)
