## CYCLISTIC DATA PROJECT
library(tidyverse)
library(janitor)
library(lubridate)
#library(readr) # readr, dplyr and ggplot2 are already attached when you load tidyverse
#library(dplyr)
library(skimr)
#library(ggplot2)
#files <- dir("csv/", full.names = T) # It's not a good practice to use T and F instead for TRUE and FALSE; those are only aliases that can be overriden by other variables (e.g. T = 0).


## Import previous 12 months data, downloaded from https://divvy-tripdata.s3.amazonaws.com/index.html
## October, 2021 to September, 2022 were the months used.

# It's legitimate to download datasets manually but it should be avoided, because it's not reproducible. I would definitely not rename the csv files manually, in case you want to use the original filename.

options(timeout = max(300, getOption("timeout"))) # See ?download.file

# You should create Data directories in your script if you download files here too
dir.create("Data", showWarnings = FALSE)

# The zip names on the AWS server have all the same format, so we can easily create them in R.
zip_names <- c(paste0("2021", 10:12, "-divvy-tripdata.zip"), paste0("2022", stringr::str_pad(1:9, 2L, "left", "0"), "-divvy-tripdata.zip"))

base_url <- "https://divvy-tripdata.s3.amazonaws.com/"

# it seems that by default, download.file() overwrites any files it encounters. Since we do not want to re-download everything, we can loop over our list of URLs and filenames and check if the zip file already exists in the data folder. If it does, we exit the current loop, if not, we download the file and then unzip it. Unzip does not overwrite any files by default. NB: We use the function walk here because it is made for side-effects, i.e. anything that is not a function return value, such as input/output or databases, or downloads or plotting.

#NB: This could maybe fail on Windows, due to a lack of system built-in zip/unzip tool.
purrr::walk2(paste0(base_url, zip_names), zip_names, function(x, y) {
  existing_files <- dir("Data")
  if(y %in% existing_files) return()
  download.file(url = x, destfile = paste0("DATA/", y), method = "libcurl")
  unzip(paste0("DATA/", y), exdir = "./DATA")
})

# We know that the extracted files are (hopefully) CSV files, so we can get all existing CSV files from the directory
extracted_csv_filenames <- dir("Data", pattern = "\\.csv", full.names = TRUE)

# We can use map (similar to base R lapply) and especially its scoped variant map_dfr() to read all the CSV files and bind them by row to a big dataframe. No need to create temporary dataframes.
bike_rides <- purrr::map_dfr(extracted_csv_filenames, readr::read_csv)

# Note: variable names should always be self-explanatory, so it's better to not use tmp, df1, df2 etc. Instead, you could use data_bikes_2021_10 etc. It also does not make sense to use the inferior base R read.csv() function here, if you already loaded the tidyverse. Use the tidyverse version readr::read_csv() instead, because it is more stable. If this is too slow, you can also have a look at data.table::fread() which is MUCH faster (but data.table has a very different syntax compared to the tidyverse).

# df1 <- read.csv("./Data/1t1021.csv")
# df2 <- read.csv("./Data/2t1121.csv")
# df3 <- read.csv("./Data/3t1221.csv")
# df4 <- read.csv("./Data/4t0122.csv")
# df5 <- read.csv("./Data/5t0222.csv")
# df6 <- read.csv("./Data/6t0322.csv")
# df7 <- read.csv("./Data/7t0422.csv")
# df8 <- read.csv("./Data/8t0522.csv")
# df9 <- read.csv("./Data/9t0622.csv")
# df10 <- read.csv("./Data/10t0722.csv")
# df11 <- read.csv("./Data/11t0822.csv")
# df12 <- read.csv("./Data/12t0922.csv")
# 
# #Combining the above files into a CSV file

# Note: Same here: rbind() is the slower row-binding function, whereas bind_rows() from the tidyverse (used by map_dfr()) is faster and more intelligent.
# bike_rides <-
#   rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

bike_rides <- janitor::remove_empty(bike_rides, which = c("cols")) # Does nothing? Maybe read_csv() ignores empty columns...
bike_rides <- janitor::remove_empty(bike_rides, which = c("rows"))
View(bike_rides)
str(bike_rides)
#Due to computer limitation, we need to take a random
# sample without replacement from total 4,073,561 observations
# Here is how the sample size was calculated
# Population size : 5828235
# Confidence level : 99.99%
# Margin of Error : 0.2
# Sample size: 606149

# Note: Be consistent with namespaces, i.e. use dplyr::sample_n below or leave the namespaces away. The big advantage of using namespace::function is that you can also usked "masked" functions and you will need that notation if you want to release your code in an R package.

# That's bad variable naming. The variable above is called bike_rides and the one here bikerides? Better name it something like bike_rides_sampled
# bikerides <- sample_n(bike_rides, 606149, replace = F)
bikerides <- sample_n(bike_rides, 606149, replace = FALSE)

str(bikerides)
colnames(bikerides)
nrow(bikerides)
dim(bikerides)
head(bikerides)
str(bikerides)
summary(bikerides)

# Note: What's with all the different date/time variables below? Some seem to be unused, e.g. bikerides$Ymd. Also bikerides$date does not need to be converted to a date multiple times (for year, month, day).
bikerides$date <- as.Date(bikerides$started_at)
bikerides$year <- format(as.Date(bikerides$date), "%Y") # as.Date() unnecessary, since you used it above already
bikerides$month <- format(as.Date(bikerides$date), "%m")
bikerides$day <- format(as.Date(bikerides$date), "%d")
bikerides$Ymd <- as.Date(bikerides$started_at) # Exactly the same as line 90
bikerides$started_at <- lubridate::ymd_hms(bikerides$started_at)
bikerides$ended_at <- lubridate::ymd_hms(bikerides$ended_at)

bikerides$start_hour <- lubridate::hour(bikerides$started_at)
bikerides$end_hour <- lubridate::hour(bikerides$ended_at)
head(bikerides)

# Note: Don't forget: If you overwrite a variable name, your script will only run correctly if you start a fresh session and re-run the whole script? it's much better to use new variable names for actions that change your data.
bikerides <- bikerides %>%
  mutate(ride_length = ended_at - started_at) %>%
  mutate(day_of_week = weekdays(as.Date(bikerides$started_at)))

# Remove rows which have negative ride_length
bikerides <- bikerides %>%
  filter(ride_length > 0)
# Clean column names and remove duplicates
bikerides <- bikerides %>%
  #clean_names() %>% # tidyverse/readr functions by default create clean names
  unique()
bikerides <- bikerides[!duplicated(bikerides$ride_id), ] # this you could add in the pipeline above: %>% filter(distinct(ride_id))

# This code below makes no sense...did you mean nrow(bike_rides) - nrow(bikerides)  ?
print(paste("Removed", nrow(bikerides) - nrow(bikerides), "duplicated rows"))

# Descriptive Analysis

# Descriptive analysis on ride_length (all figures in seconds)
summary(bikerides$ride_length)
# Compare members and casual users

# Note: The following code with aggregate can be done a bit easier even with the tidyverse:
# bikerides %>% 
#   group_by(member_casual) %>% 
#   summarise(mean = mean(ride_length), median = median(ride_length), max = max(ride_length), min = min(ride_length))

aggregate(bikerides$ride_length ~ bikerides$member_casual, FUN = mean)
aggregate(bikerides$ride_length ~ bikerides$member_casual, FUN = median)
aggregate(bikerides$ride_length ~ bikerides$member_casual, FUN = max)
aggregate(bikerides$ride_length ~ bikerides$member_casual, FUN = min)

# Here is the average ride time by each day for members vs casual users
aggregate(bikerides$ride_length ~ bikerides$member_casual + bikerides$day_of_week, FUN = mean)

# Tidyverse version:
# bikerides %>% 
#   group_by(member_casual, day_of_week) %>% 
#   summarise(mean = mean(ride_length))

# Sort days of the week
bikerides$day_of_week <- ordered(bikerides$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Your following code does not make sense to me...the code from line 146-151 does the same thing as the code in 154-158 ? And you don't assign the result to any variable?
# bikerides %>% 
#   mutate(weekday = wday(started_at, label = TRUE)) %>%  # Note: why are you getting the weekday here again? you just reordered it above? that makes no sense
#   group_by(member_casual, weekday) %>%  
#   summarise(number_of_rides = n()
#             ,average_duration = mean(ride_length)) %>%    
#   arrange(member_casual, weekday)


bike_rides_sampled_summary <- bikerides %>% 
  group_by(member_casual, day_of_week) %>%  
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length), total_duration = sum(ride_length)) %>%    
  arrange(member_casual, day_of_week)

# Visualize the number of rides by rider type
# bikerides %>% 
#   mutate(weekday = wday(started_at, label = TRUE)) %>% 
#   group_by(member_casual, weekday)%>%
#   dplyr::summarise(number_of_rides = n() ,average_duration = mean(ride_length)) %>%
#   arrange(member_casual, .group = "drop")

# Note: Why? bikerides is already a dataframe?
#br <- as.data.frame(bikerides)

# What did you want to plot here? the average duration? the sum?
ggplot(bike_rides_sampled_summary, aes(x = day_of_week, y = total_duration, fill = member_casual)) +
  geom_col(position = "dodge")
# Visualization for average duration

# Note: More duplicated code...why? clean this up, it will make your life easier

# bikerides %>% 
#   mutate(weekday = wday(started_at, label = TRUE)) %>% 
#   group_by(member_casual, weekday) %>%
#   summarise(number_of_rides = n()
#             ,average_duration = mean(ride_length)) %>% 
#   arrange(member_casual, weekday)
# br <- as.data.frame(bikerides)
ggplot(bike_rides_sampled_summary, aes(x = day_of_week, y = as.numeric(average_duration), fill = member_casual)) +
  geom_col(position = "dodge")
# Slight error persists


# Some general hints:
# More than anything else, make sure that your script is reproducible. That means it needs to run from top to bottom without errors and the results must be the same (except of course where sampling/bootstrapping is involved)
# You create many new variables, some of them do exactly the same thing (dates/times). Be aware that every variable will slow down computations. It's usually better to stick to the important variables and remove the ones you don't need
# If you're loading the tidyverse, there's no reason to use the base R functions, as these sometimes behave unstable and error messages of tidyverse functions are often much better.
