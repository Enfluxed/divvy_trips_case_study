# Set up environment.
install.packages("tidyverse")
library(tidyverse)

# Load cleaned trips data.
trips_05_to_07_2023 <- read_csv("~/Coursera Capstone Project/data/clean_data/clean_trips_05_to_07_2023")

# Create and add new column ride_length_min to dataframe.
trips_05_to_07_2023['ride_length_min'] <- difftime(trips_05_to_07_2023$ended_at, trips_05_to_07_2023$started_at, units = "mins")

# Create and add new column day_of_week to dataframe Sunday=1, Monday=2, Tuesday=3,...
trips_05_to_07_2023['day_of_week'] <- wday(trips_05_to_07_2023$started_at)

# Write updated dataframe to csv
write.csv(trips_05_to_07_2023, "C:/Users/Cole Espinola/Documents/Coursera Capstone Project/data/clean_data/clean_new_trips_05_to_07_2023.csv", row.names=TRUE)