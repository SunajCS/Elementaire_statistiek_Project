# constants
folder_path <- "vragen/vraag1/images/"
color_blue <- "#1f77b4"
siq_level <- 0.05

# Read the CSV file into a data frame 'eik'
eik <- read.csv("eik.csv", sep = ";", dec = ",")

# Use column 'boom' as the row names
rownames(eik) <- eik$boom

# Remove rows to  last 3 numbers of student number: 20215256 -> 256
i <- 2
j <- 5
k <- 6

# del: k + 1, j + 1, i + 1, jk + 1, ij + 1, ik + 1, ijk + 1, and i + j + k + 1
# One entry is not possible to remove: ijk + 1 = 61, due to index out of bounds
rows_to_remove <- c(
    k + 1, j + 1, i + 1, j * k + 1,
    i * j + 1, i * k + 1, i + j + k + 1
)
eik <- eik[-rows_to_remove, ]


# Plot histogram for the column 'Grootte'
h_grootte <- hist(eik$Grootte,
    main = "Histogram Grootte", xlab = "Grootte", ylab = "Frequentie",
    col = color_blue
)
xfit_grootte <- seq(min(eik$Grootte), max(eik$Grootte), length = 40)
yfit_grootte <- dnorm(xfit_grootte,
    mean = mean(eik$Grootte),
    sd = sd(eik$Grootte)
)
yfit_grootte <- yfit_grootte * diff(h_grootte$mids[1:2]) * length(eik$Grootte)
lines(xfit_grootte, yfit_grootte, col = "red", lwd = 2)

# Legende with name of the distribution
legend("topright",
    inset = 0.05, legend = c("Normaal"),
    fill = "red", bty = "n"
)

# Save the plot
dev.copy(png, paste(folder_path, "hist_norm_grootte.png", sep = ""))
dev.off()

# Plot histogram for the column 'Volume'
h_volume <- hist(eik$Volume,
    main = "Histogram Volume",
    xlab = "Volume", ylab = "Frequentie",
    col = color_blue
)
xfit_volume <- seq(min(eik$Volume), max(eik$Volume), length = 40)
yfit_volume <- dnorm(xfit_volume,
    mean = mean(eik$Volume),
    sd = sd(eik$Volume)
)
yfit_volume <- yfit_volume * diff(h_volume$mids[1:2]) * length(eik$Volume)
lines(xfit_volume, yfit_volume, col = "red", lwd = 2)

# Legende with name of the distribution
legend("topright",
    inset = 0.05, legend = c("Normaal"),
    fill = "red", bty = "n"
)

# Save the plot
dev.copy(png, paste(folder_path, "hist_norm_volume.png", sep = ""))
dev.off()

# Boxplot for the column 'Grootte'
boxplot(eik$Grootte,
    main = "Boxplot Grootte", xlab = "Grootte", ylab = "Frequentie",
    col = color_blue
)
# print all 5 boxplot information it holds 
info_grootte <- boxplot.stats(eik$Grootte)
print(paste("Min: ", info_grootte$stats[1]))
print(paste("Q1: ", info_grootte$stats[2]))
print(paste("Median: ", info_grootte$stats[3]))
print(paste("Q3: ", info_grootte$stats[4]))
print(paste("Max: ", info_grootte$stats[5]))


# Save the plot
dev.copy(png, paste(folder_path, "box_grootte.png", sep = ""))
dev.off()

# Boxplot for the column 'Volume'
boxplot(eik$Volume,
    main = "Boxplot Volume", xlab = "Volume", ylab = "Frequentie",
    col = color_blue
)

# Save the plot
dev.copy(png, paste(folder_path, "box_volume.png", sep = ""))
dev.off()

# QQ plot for the column 'Grootte'
qqnorm(eik$Grootte,
    main = "Q-Q plot Grootte", xlab = "Theoretische Kwantielen",
    ylab = "Kwantielen van de steekproef", col = color_blue, pch = 19
)
qqline(eik$Grootte, col = "red", lwd = 2)

# Save the plot
dev.copy(png, paste(folder_path, "qq_grootte.png", sep = ""))
dev.off()

# QQ plot for the column 'Volume'
qqnorm(eik$Volume,
    main = "Q-Q plot Volume", xlab = "Theoretische Kwantielen",
    ylab = "Kwantielen van de steekproef", col = color_blue, pch = 19
)
qqline(eik$Volume, col = "red", lwd = 2)

# Save the plot
dev.copy(png, paste(folder_path, "qq_volume.png", sep = ""))
dev.off()

library(e1071)
# Calculate skewness and kurtosis for both 'Grootte' and 'Volume'
skewness_grootte <- skewness(eik$Grootte)
skewness_volume <- skewness(eik$Volume)
kurtosis_grootte <- kurtosis(eik$Grootte)
kurtosis_volume <- kurtosis(eik$Volume)

# if skewness is between -0.5 and 0.5, the distribution is approximately symmetric
# if skewness is between -1 and -0.5 or between 0.5 and 1, the distribution is moderately skewed
# if skewness is less than -1 or greater than 1, the distribution is highly skewed
# if skewness is 0, the distribution is perfectly symmetric
result_skewness_grootte <- ifelse(
    skewness_grootte > -0.5 & skewness_grootte < 0.5,
    "approximately symmetric",
    ifelse(
        skewness_grootte > -1 & skewness_grootte < -0.5 |
            skewness_grootte > 0.5 & skewness_grootte < 1,
        "moderately skewed",
        ifelse(
            skewness_grootte < -1 | skewness_grootte > 1,
            "highly skewed",
            "perfectly symmetric"
        )
    )
)
result_skewness_volume <- ifelse(
    skewness_volume > -0.5 & skewness_volume < 0.5,
    "approximately symmetric",
    ifelse(
        skewness_volume > -1 & skewness_volume < -0.5 |
            skewness_volume > 0.5 & skewness_volume < 1,
        "moderately skewed",
        ifelse(
            skewness_volume < -1 | skewness_volume > 1,
            "highly skewed",
            "perfectly symmetric"
        )
    )
)

# if kurtosis is between -1 and 1, the distribution is mesokurtic, which means that the distribution has a normal shape
# if kurtosis is less than -1, the distribution is platykurtic, which means that the distribution is flatter than a normal distribution
# if kurtosis is greater than 1, the distribution is leptokurtic, which means that the distribution is more peaked than a normal distribution
result_kurtosis_grootte <- ifelse(
    kurtosis_grootte > -1 & kurtosis_grootte < 1,
    "mesokurtic",
    ifelse(
        kurtosis_grootte < -1,
        "platykurtic",
        "leptokurtic"
    )
)
result_kurtosis_volume <- ifelse(
    kurtosis_volume > -1 & kurtosis_volume < 1,
    "mesokurtic",
    ifelse(
        kurtosis_volume < -1,
        "platykurtic",
        "leptokurtic"
    )
)

# print the results e.g. print('Skewness Grootte: ' + str(skewness_grootte) + ' (' + result_skew_grootte + ')')
print(paste("Skewness Grootte:", skewness_grootte, "(", result_skewness_grootte, ")", sep = " "))
print(paste("Kurtosis Grootte:", kurtosis_grootte, "(", result_kurtosis_grootte, ")", sep = " "))
print(paste("Skewness Volume:", skewness_volume, "(", result_skewness_volume, ")", sep = " "))
print(paste("Kurtosis Volume:", kurtosis_volume, "(", result_kurtosis_volume, ")", sep = " "))

# Perform the Wilks-Shapiro test for normality on 'Grootte'
shap_grootte <- shapiro.test(eik$Grootte)
result_shape_grootte <- ifelse(
    shap_grootte$p.value < siq_level,
    "not normal",
    "normal"
)
print(paste("Shapiro-Wilk test Grootte:", shap_grootte$p.value, "(", result_shape_grootte, ")", sep = " "))
# Perform the Wilks-Shapiro test for normality on 'Volume'
shap_volume <- shapiro.test(eik$Volume)
result_shape_volume <- ifelse(
    shap_volume$p.value < siq_level,
    "not normal",
    "normal"
)
print(paste("Shapiro-Wilk test Volume:", shap_volume$p.value, "(", result_shape_volume, ")", sep = " "))

# Transform 'Grootte' by taking the fourth root
eik$Transformed_Grootte <- eik$Grootte^(1/4)

# Transform 'Volume' by taking the logarithm
eik$Transformed_Volume <- log(eik$Volume)

# Perform the skewness, kurtosis, and Shapiro-Wilk test for normality on 'Grootte'
skew_grootte <- skewness(eik$Transformed_Grootte)
kurt_grootte <- kurtosis(eik$Transformed_Grootte)
shapiro_grootte <- shapiro.test(eik$Transformed_Grootte)

# Perform the skewness, kurtosis, and Shapiro-Wilk test for normality on 'Volume'
skew_volume <- skewness(eik$Transformed_Volume)
kurt_volume <- kurtosis(eik$Transformed_Volume)
shapiro_volume <- shapiro.test(eik$Transformed_Volume)

# Print the results
cat("Skewness Grootte (trans):", skew_grootte, "\n")
cat("Kurtosis Grootte (trans):", kurt_grootte, "\n")
cat("Shapiro-Wilk Grootte (trans):", shapiro_grootte$p.value, "\n")

cat("Skewness Volume (trans):", skew_volume, "\n")
cat("Kurtosis Volume (trans):", kurt_volume, "\n")
cat("Shapiro-Wilk Volume (trans):", shapiro_volume$p.value, "\n")
