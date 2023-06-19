# constants
folder_path <- "vragen/vraag3/images/"
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

# make a new column 'log_volume' with the log of the volume
eik$log_volume <- log(eik$Volume)

# make a scatter plot of the "log_volume" and "Hoogte"
plot(eik$log_volume, eik$Hoogte,
    main = "Log volume vs. Hoogte",
    xlab = "Log volume",
    ylab = "Hoogte",
    col = color_blue,
    pch = 19
)

# Save the scatter plot as a PNG file
dev.copy(png, paste(folder_path, "scatter_plot.png", sep = ""))
dev.off()

# Calculate the correlation coefficient
correlation_coefficient <- cor(eik$log_volume, eik$Hoogte)
# print(correlation_coefficient)

# Calculate the linear regression
linear_regression <- lm(Hoogte ~ log_volume, data = eik)
# print(linear_regression)

#  R-kwadraatwaarde
r_squared <- summary(linear_regression)$r.squared
print(r_squared*100)

# Calculate the Spearmans Rhos correlation coefficient
spearmans_rho <- cor(eik$log_volume, eik$Hoogte, method = "spearman")
# print(spearmans_rho)
