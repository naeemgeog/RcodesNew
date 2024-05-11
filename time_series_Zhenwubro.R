library(readxl)
LAC <- read.xlsx("E:/test08.05/Lake_area1.xlsx", sheet = 1)

# Step 1: Calculate the average surface area of each lake
LAC_avg <- rowMeans(LAC[, -(1:2)], na.rm = TRUE)

# Step 2: Determine the water surface area of each lake in the base period (2000, 2001, 2002)
base_periods <- c(2000, 2001, 2002)  # Choose the base periods
LAC_base <- LAC[, match(base_periods, names(LAC))]

# Step 3: Calculate the relative water area for each lake
relative_water_area <- (LAC_avg / rowMeans(LAC_base, na.rm = TRUE)) * 100

# Step 4: Calculate the average of relative water areas
average_relative_water_area <- mean(relative_water_area, na.rm = TRUE)

# Print the average relative water area
print(average_relative_water_area)



prefix_num <- 2


# Initialize vectors to store results
relative_water_areas <- matrix(NA, nrow = nrow(LAC), ncol = ncol(LAC) - prefix_num)
average_relative_water_areas <- numeric()


# Step 5: Calculate relative water area for each lake for each year

for (i in 1:nrow(LAC)) {
  for (j in (prefix_num + 1):ncol(LAC)) {
    relative_water_areas[i, j - prefix_num] <- (LAC[i, j]/ rowMeans(LAC[i, match(base_periods, names(LAC))], na.rm = TRUE)) * 100
  }
}

# Step 6: Calculate average relative water area across all lakes for each year
for (j in (prefix_num + 1):ncol(LAC)) {
  average_relative_water_areas[j - prefix_num] <- mean(relative_water_areas[, j - prefix_num], na.rm = TRUE)
}

# Convert the average relative water areas to a data frame with years as row names
average_relative_water_areas_df <- data.frame(Year = as.numeric(colnames(LAC)[(prefix_num + 1):ncol(LAC)]), 
                                              Average_Relative_Water_Area = average_relative_water_areas)

# Read the Excel file
absolute_water_area <- read_excel("E:/test08.05/sum of all lakes.xlsx")

# Extract the years and water area data
years <- absolute_water_area$Year
water_area <- absolute_water_area$`Lakes Water Area`


# Read the relative water area dataframe
relative_water_area_df <- average_relative_water_areas_df

# Read the Excel file for absolute water area
absolute_water_area <- read_excel("E:/test08.05/sum of all lakes.xlsx")

# Extract the years and water area data
years <- absolute_water_area$Year
water_area <- absolute_water_area$`Lakes Water Area`

# Reference water area (2000)
reference_water_area <- water_area[1]

# Calculate percentage change relative to water area in 2000
percentage_change <- (water_area / reference_water_area) * 100

# Create a tibble with years and percentage change
absolute_water_area_percentage <- tibble(Year = years, `Lakes Water Area` = percentage_change)

# Combine both dataframes
combined_data <- merge(relative_water_area_df, absolute_water_area_percentage, by = "Year")




p <- ggplot(combined_data, aes(x = Year)) +
  geom_line(aes(y = Average_Relative_Water_Area, color = "Relative Water Area"), size = 1.5,linetype = "dashed") +
  geom_line(aes(y = `Lakes Water Area`, color = "Absolute Water Area"), size = 1.5) +
  geom_point(aes(y = Average_Relative_Water_Area, color = "Relative Water Area"), size = 3) +
  geom_point(aes(y = `Lakes Water Area`, color = "Absolute Water Area"), size = 3) +
  scale_color_manual(values = c("Relative Water Area" = "#6528F7", "Absolute Water Area" = "#FF0080"),
                     name = "") +
  labs(title = "",
       x = "Year",
       y = "Water Area (%)") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Set black border
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", size = 0.1),  # Set axis lines
    axis.text = element_text(face = "bold", color = "black", size = 18),  # Make axis text bold
    axis.title = element_text(face = "bold", color = "black", size = 15),  # Make axis titles bold
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = c(0.8, 0.9),  # Adjust the legend position inside the plot
    legend.title = element_text(face = "bold")  # Make legend title bold
  )

# Display the plot
print(p)

# Define the full file path
file_path <- "E:/test08.05/Rprojtest/Lakeanalysis/water_area_comparison_plot1.png"

# Save the plot to the specified path
ggsave(filename = file_path, plot = p, width = 10, height = 6, units = "in",bg="white")

