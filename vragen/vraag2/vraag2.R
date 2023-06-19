library(plotrix)
# constants
folder_path <- "vragen/vraag2/images/"
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

# Maak nieuwe variabele 'dikke eikel' aan, waarbij de waarde 'True' is als de diameter van de eikel groter is dan 3 cm anders 'False'
eik$dikke_eikel <- eik$Volume > 3.0

# Maak kruistabel van 'dikke_eikel' en 'Regio'
kruistabel <- table(eik$dikke_eikel, eik$Regio)

# Bereken de chi-kwadraat waarde
chi_kwadraat <- chisq.test(kruistabel)

# print resultaat Chi-kwadraat statistiek en p-waarde
print(chi_kwadraat)

thick_acorns_count <- table(eik$Regio, eik$dikke_eikel)[, 2]

# Create a pie chart using pie3D() function
# pie3D(thick_acorns_count,
#     labels = paste(
#         names(thick_acorns_count),
#         "(", sprintf("%0.2f", thick_acorns_count / sum(thick_acorns_count) * 100), "%", ")",
#         sep = ""
#     ),
#     main = "Dikke eikels per regio",
#     col = rainbow(length(thick_acorns_count))
# )

# # Save the pie chart as a PNG file
# dev.copy(png, paste(folder_path, "pie_chart.png", sep = ""))

# # Display the pie chart
# dev.off()
# create Kruistabel voor regio en dikke eikel and met verwachte waarden
print(kruistabel)
kruistabel_verwacht <- kruistabel
kruistabel_verwacht[1, ] <- sum(kruistabel[1, ]) / sum(kruistabel) * sum(kruistabel[, 1])
kruistabel_verwacht[2, ] <- sum(kruistabel[2, ]) / sum(kruistabel) * sum(kruistabel[, 1])
print(kruistabel_verwacht)



# Save the bar chart as a PNG file
dev.copy(png, paste(folder_path, "bar_chart.png", sep = ""))
dev.off()

