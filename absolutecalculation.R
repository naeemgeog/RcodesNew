#--Sum of lakes in Abs------
library(readxl)
library(tibble)
# Read the Excel file
absolute_water_area <- read_excel("E:/test08.05/sum of all lakes.xlsx")

# Extract the years and water area data
years <- absolute_water_area$Year
water_area <- absolute_water_area$`Lakes Water Area`

# Reference water area (2000)
reference_water_area <- water_area[1]

# Calculate percentage change relative to water area in 2000
percentage_change <- (water_area / reference_water_area) * 100

# Create a tibble with years and percentage change
Absolute_water_area_percentage <- tibble(Year = years, `Lakes Water Area` = percentage_change)

# Print the tibble
print(Absolute_water_area_percentage)
