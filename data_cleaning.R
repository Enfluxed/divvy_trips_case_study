# set up environment
install.packages("tidyverse")
library(tidyverse)

# loading unclean trips data
trips_05_2023 <- read_csv("~/Coursera Capstone Project/data/unclean_data/202305-divvy-tripdata.csv")
trips_06_2023 <- read_csv("~/Coursera Capstone Project/data/unclean_data/202306-divvy-tripdata.csv")
trips_07_2023 <- read_csv("~/Coursera Capstone Project/data/unclean_data/202307-divvy-tripdata.csv")

# preview data
colnames(trips_05_2023)
head(trips_05_2023)

# combine dataframes into one
temp_df <- rbind(trips_05_2023, trips_06_2023)
trips_05_to_07_2023 <- rbind(temp_df, trips_07_2023)

# we only care about 9 of the 13 variables and won't pay attention
# to the variables recording latitude and longitude
# count number of NAs
sum(is.na(trips_05_to_07_2023$ride_id))            # NA of ride_id = 0
sum(is.na(trips_05_to_07_2023$rideable_type))      # NA of rideable_type = 0
sum(is.na(trips_05_to_07_2023$started_at))         # NA of started_at = 0
sum(is.na(trips_05_to_07_2023$ended_at))           # NA of ended_at = 0
sum(is.na(trips_05_to_07_2023$start_station_name)) # Na of start_station_name = 328442
sum(is.na(trips_05_to_07_2023$start_station_id))   # NA of start_station_id = 328442
sum(is.na(trips_05_to_07_2023$end_station_name))   # NA of end_station_name = 349621
sum(is.na(trips_05_to_07_2023$end_station_id))     # NA of end_station_id = 349621
sum(is.na(trips_05_to_07_2023$member_casual))      # NA of member_casual = 0

# checking that each start station only has one corresponding id
trips_05_to_07_2023 %>% group_by(start_station_name) %>%
  summarise(distinct_ids = n_distinct(start_station_id))

trips_05_to_07_2023 %>% group_by(start_station_name) %>%
  summarise(distinct_ids = n_distinct(start_station_id)) %>%
  filter(distinct_ids > 1)

# table is empty for the filtered version, so it appears each
# start_station_name corresponds to only one id

# checking that each end station name only has one corresponding id
trips_05_to_07_2023 %>% group_by(end_station_name) %>%
  summarise(distinct_ids = n_distinct(end_station_id))

trips_05_to_07_2023 %>% group_by(end_station_name) %>%
  summarise(distinct_ids = n_distinct(end_station_id)) %>%
  filter(distinct_ids > 1)

# table is empty for the filtered version, so it appears each
# end_station_name corresponds to only one id

# ride_id is presumably the primary key thus, there should be no 
# duplicates (aka they should all be distinct)
trips_05_to_07_2023 %>% summarise(distinct_ride_ids = n_distinct(ride_id))

# number of distinct ride_ids equal to number of objects in the 
# dataframe suggesting every ride_id is distinct as expected

# we want to confirm that when start_station_name is null
# then start_station_id is null as well
# the matching numbers suggest this, but it is best to check
NA_station_df <- subset(trips_05_to_07_2023, is.na(trips_05_to_07_2023$start_station_name))     # create subset df with only NA values in start_station_name
if(sum(is.na(NA_station_df$start_station_id)) == nrow(NA_station_df)) {
  print("True")
}

# confirmed that start_station_name is NA only when start_station_id
# is NA

# we want to confirm that when end_station_name is null
# then end_station_id is null as well
NA_station_df <- subset(trips_05_to_07_2023, is.na(trips_05_to_07_2023$end_station_name))     # create subset df with only NA values in start_station_name
if(sum(is.na(NA_station_df$end_station_id)) == nrow(NA_station_df)) {
  print("True")
}

# confirmed that end_station_name is NA only when end_station_id
# is NA

# it is unknown why some columns such as start_station_name,
# start_station_id and such have NA values, thus we will be
# dropping them from the dataframe
# we don't care about coordinates for this analysis thus
# we will not be dropping rows with NA coordinate values

trips_05_to_07_2023 <- drop_na(trips_05_to_07_2023, start_station_name, start_station_id, end_station_name, end_station_id)

write.csv(trips_05_to_07_2023, "C:/Users/Cole Espinola/Documents/Coursera Capstone Project/data/clean_data/clean_trips_05_to_07_2023.csv", row.names=TRUE)
