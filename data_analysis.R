# Set up environment.
install.packages("tidyverse")
install.packages("scales")
library(tidyverse)
library(scales)

# Load cleaned and transformed trips data.
trips_05_to_07_2023 <- read_csv("~/Coursera Capstone Project/data/clean_data/clean_new_trips_05_to_07_2023.csv")

# Recall question: how do annual members and casual riders use Cyclistic bikes differently?

# Subquestion 1: what days do annual members and casual rider use cyclistic bikes the most
most_popular_day <- trips_05_to_07_2023 %>% group_by(day_of_week) %>%
  summarise(member = sum(member_casual == "member"), casual = sum(member_casual == "casual"))

# Bar Chart of most popular day of week to use cyclist bike annual members
week_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
ggplot(data = most_popular_day, aes(x = factor(day_of_week), y = member)) + geom_bar(stat = "identity", fill='#72B8AD') +
  labs(title = "Annual Member Ridership by Day of the Week", x = "Day of the Week", y = "Count",
       caption = "Data from: May 2023 to July 2023") +
  scale_x_discrete(labels = week_names) 

ggplot(data = most_popular_day, aes(x = factor(day_of_week), y = casual)) + geom_bar(stat = "identity", fill='#BF4B43') +
  labs(title = "Casual Ridership by Day of the Week", x = "Day of the Week", y = "Count",
       caption = "Data from: May 2023 to July 2023") +
  scale_x_discrete(labels = week_names) + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Subquestion 2: how many of the rides in past three months are from members and how many from casual riders?
ggplot(data = trips_05_to_07_2023, aes(x = member_casual)) + geom_bar(fill='#72B8AD') +
  labs(title = "Number of Rides by Casual Riders vs Annual Member Riders", x = "Membership Type", y = "Count",
       caption = "Data from: May 2023 to July 2023")

# group by count of annual member and casual riders
mem_cas_ride_count <- trips_05_to_07_2023 %>% group_by(member_casual) %>%
summarise(values = n())

# Pie chart
ggplot(data = mem_cas_ride_count, aes(x = "", y = values, fill = member_casual)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0)

#Subquestion 3: what is the most popular start_station_name among casual riders vs annual riders
popular_member_start_station <- trips_05_to_07_2023 %>% group_by(start_station_name) %>%
  summarise(member = sum(member_casual == "member")) %>%
  arrange(desc(member))

popular_casual_start_station <- trips_05_to_07_2023 %>% group_by(start_station_name) %>%
  summarise(casual = sum(member_casual == "casual")) %>%
  arrange(desc(casual))

# Subquestion 4: how long do annual members rides tend to last compared to casual riders?

# Get min ride duration and max
min_ride <- min(trips_05_to_07_2023$ride_length_min)
max_ride <- max(trips_05_to_07_2023$ride_length_min)

# Get mean of annual member and casual member ride duration
mean_mem_cas <- trips_05_to_07_2023 %>% group_by(member_casual) %>%
  summarise(mean_type = mean(ride_length_min))

# Plot ride duration via histogram
ggplot(data = trips_05_to_07_2023, aes(x = ride_length_min, color = member_casual)) +
  geom_histogram(binwidth = 1, fill = "white") +
  geom_vline(data = mean_mem_cas, aes(xintercept = mean_type, color = member_casual), linetype = "dashed", size = 1) +
  xlim(0, 60) + facet_wrap(~member_casual) +
  labs(title = "Ride Length Distribution of Casual Riders and Annual Members", x = "Ride Length (Minutes)", y = "Count",
       caption = "Data from: May 2023 to July 2023") +
  guides(color = guide_legend(title = "Member Type"))
  

# Histogram plot by density
ggplot(data = trips_05_to_07_2023, aes(x = ride_length_min, y = ..density.., color = member_casual)) +
  geom_histogram(binwidth = 3, fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") + xlim(0, 120) +
  facet_wrap(~member_casual)


# Subquestion 5: what is the average ride_length_min for users by day_of_week?
mean_ride_day <- trips_05_to_07_2023 %>% group_by(day_of_week) %>%
  summarise(mean_ride_length = mean(ride_length_min))

ggplot(data = mean_ride_day, aes(x = factor(day_of_week), y = mean_ride_length)) + geom_bar(stat = "identity", fill='#72B8AD') +
  labs(title = "Mean Ride Length by Day of the Week", x = "Day of the Week", y = "Mean Ride Length (minutes)",
       caption = "Data from: May 2023 to July 2023") +
  scale_x_discrete(labels = week_names) 