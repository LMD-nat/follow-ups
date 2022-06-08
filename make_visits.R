### This is the file for creating date and time data mimicking how I think patients would visit and leave the Emergency Department
# credit for the original instructions: https://towardsdatascience.com/how-to-build-a-complete-classification-model-in-r-and-caret-73c68c3392e1

### Import set of about 80,000 dates from a real-estate dataset to start (https://www.kaggle.com/datasets/new-york-city/nyc-property-sales)
### cause I'm sure as hell not creating dates myself, I just wanted a set of random dates

#import the data and load some libraries
library(readr) # imports files
library(dplyr) # great for data cleaning
library(chron) # work with time data

iv <- read_csv("~/Downloads/index_visits.csv")

# set a "seed" in order to reproduce any randomization that I end up doing
set.seed(514) # get it, montreal? hee hee

### Randomly generate how many days it's been since the index visit to the ER
iv$days_since_index <- sample(0:90, 84548, replace=TRUE)
table(iv$days_since_index)

### Generate the return dates
iv$return_date <- iv$index_visit + iv$days_since_index

### Generate groups based on the days between visits
iv$visit_type <- ifelse(iv$days_since_index < 7 & iv$days_since_index > 2, "early revisit",
       ifelse(iv$days_since_index > 8 & iv$days_since_index < 30, "late-revisit",
       ifelse(iv$days_since_index > 31 , "normal-revisit", "follow-up")))
       
### Generate the days of the week
iv$weekday <- weekdays(as.Date(iv$index_visit))
iv$return_day <- weekdays(as.Date(iv$return_date))

### Find people who are early OR follow-ups who had a visit over the weekend
# clinics are not open and specialists generally aren't available for non-urgent cases on weekends,
# so for follow-ups patients generally have to come back on Monday (depends on the hospital but that's the rule I'm going with)

iv$weekend_ER <- ifelse(iv$weekday == 'Friday' & iv$days_since_index < 3, 1, 
                        ifelse(iv$weekday == 'Saturday' & iv$days_since_index < 2, 2,
                               ifelse(iv$weekday == 'Sunday' & iv$days_since_index < 1, 3, 0)))
                               
### check if behaving properly
table(iv$weekend_ER, iv$weekday) 
# looks good, but need to be careful with Fridays, because you can arrive
# and come back on the same Friday (4:00 AM and then like a return at 8:00 would be a follow-up)

### create timestamps for index visits
iv$time_hours <- sample(0:23, 84548, replace=TRUE)
iv$time_minutes <- sample(00:59, 84548, replace=TRUE)
iv$index_arrival_time <- paste(iv$time_hours, iv$time_minutes, sep = ":" )
iv$index_arrival_time <- paste(iv$index_visit, iv$index_arrival_time, sep = " ")
iv$index_arrival_time <- strptime(iv$index_arrival_time, format = "%Y-%m-%d %H:%M", tz = "EST")

### create timestamps for return visits
iv$time_hours_rv <- sample(0:23, 84548, replace=TRUE)
iv$time_minutes_rv <- sample(00:59, 84548, replace=TRUE)
iv$arrival_time_rv <- paste(iv$time_hours_rv, iv$time_minutes_rv, sep = ":" )
iv$arrival_time_rv <- paste(iv$return_date, iv$arrival_time_rv, sep = " ")
iv$arrival_time_rv <- strptime(iv$arrival_time_rv, format = "%Y-%m-%d %H:%M", tz = "EST")

### time between visits in hours
iv$time_btw_visits <- (iv$arrival_time_rv - iv$index_arrival_time)/3600
iv$time_btw_visits <- as.numeric(iv$time_btw_visits)

###########################################

### Save as CSV file to fix negative time values (how did that even happen???)
#write.csv(iv, "~/Downloads/index_visits3.csv")
index_visits4 <- read_csv("index_visits4.csv")

# name some variables
colnames(index_visits4)[1] <- "patient_id"
#index_visits4 = iv
#iv = index_visits4
iv$time_btw_visits <- (iv$arrival_time_rv - iv$index_arrival_time)/3600
iv$time_btw_visits <- as.numeric(iv$time_btw_visits)

# fix the patients with negative wait times
iv_neg <- subset(iv, time_btw_visits < 1)
iv_neg$arrival_time_rv = iv_neg$arrival_time_rv+86400
iv_neg$index_arrival_time = iv_neg$index_arrival_time+0
iv_neg$time_btw_visits = (iv_neg$arrival_time_rv - iv_neg$index_arrival_time)
iv_neg$time_btw_visits <- as.numeric(iv_neg$time_btw_visits)
iv_pos <- subset(iv, time_btw_visits > 1)

### Select only columns we care about

iv_neg$index_time_clock <- paste(iv_neg$time_hours, iv_neg$time_minutes, "00", sep = ":" )
iv_neg$it_clock <- chron(times=iv_neg$index_time_clock)

iv_neg$rv_time_clock <- paste(iv_neg$time_hours_rv, iv_neg$time_minutes_rv, "00", sep = ":" )
iv_neg$rv_clock <- chron(times=iv_neg$rv_time_clock)

neg <- iv_neg %>% select(index_arrival_time, arrival_time_rv, time_btw_visits, it_clock, rv_clock, 
                         days_since_index, weekday, return_day, visit_type, weekend_ER)

iv_pos$index_time_clock <- paste(iv_pos$time_hours, iv_pos$time_minutes, "00", sep = ":" )
iv_pos$it_clock <- chron(times=iv_pos$index_time_clock)

iv_pos$rv_time_clock <- paste(iv_pos$time_hours_rv, iv_pos$time_minutes_rv, "00", sep = ":" )
iv_pos$rv_clock <- chron(times=iv_pos$rv_time_clock)

pos <- iv_pos %>% select(index_arrival_time, arrival_time_rv, time_btw_visits, it_clock, rv_clock, 
                         days_since_index, weekday, return_day, visit_type, weekend_ER)

### join these two sets together
join <- bind_rows(neg, pos)

### write a function that will create a unique alphanumeric id
create_unique_ids <- function(n, seed_no = 1, char_len = 8){
  set.seed(seed_no)
  pool <- c(letters, LETTERS, 0:9)
  
  res <- character(n)
  for(i in seq(n)){
    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    while(this_res %in% res){ # if there was a duplicate, re-assign an id
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    }
    res[i] <- this_res
  }
  res
}

### this will take a moment, takes about 30s to one minute, it's a lot of ids
ids <- create_unique_ids(84546)
join$id <- ids

### make sure all ids are unique
df_uniq <- unique(join$id)
length(df_uniq) # if this is equal to 84546, that's great!

### select only the variables we care about
join <- join %>% select(id, index_arrival_time, arrival_time_rv, time_btw_visits, it_clock, rv_clock, 
                         days_since_index, weekday, return_day, visit_type, weekend_ER)

###########################

### I think i needed a break here so I saved my file to come back to it later

### save file
write.csv(join, "~/Downloads/index_visits5.csv")

### classify times into categories
index_visits6 <- read_csv("index_visits6.csv", 
                          col_types = cols(it_clock = col_time(format = "%H:%M:%S"), 
                                           rv_clock = col_time(format = "%H:%M:%S")))

library(lubridate)
# create breaks for time "categories"
breaks <- hour(hm("00:00", "8:00", "11:59", "17:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")
iv_cats$index_cat <- cut(x=hour(iv_cats$index_arrival_time), breaks = breaks, labels = labels, include.lowest=TRUE)
iv_cats$return_cat <- cut(x=hour(iv_cats$arrival_time_rv), breaks = breaks, labels = labels, include.lowest=TRUE)

visits <- iv_cats %>% select(id, index_arrival_time, arrival_time_rv, time_btw_visits_hours, it_clock, rv_clock, 
                        days_since_index, weekday, return_day, visit_type, weekend_ER, index_cat, return_cat)

table(visits$return_cat) # shows you how many are in each category

### Classify the types of returns to the ER as follow-up or NOT follow up

##### GOLD STANDARD RULES ########
#if return category is morning, and index category is night, and time between the two is less than 24 hours, this is a follow up!
#except on weekends, where the follow up would happen on monday!

visits$ER_follow_up <- ifelse(visits$index_cat == 'Night' & visits$return_cat == 'Morning' & visits$time_btw_visits_hours < 24, 1, 0) 

# only 37 follow ups? seems way rarer than I'd expect

table(visits$ER_follow_up)

#write.csv(visits, "~/Downloads/index_visits7.csv")

#I added more by hand, and some noise too
visits <- read_csv("index_visits8.csv")
table(visits$ER_follow_up, visits$return_cat)

# follow-ups only happen in the morning, so here's a check, looks good, 130 follow-up revisits
# I added some noise, but the follow-up visits are all very plausible

# Planned Re-Visits KMM Model
# NOTE THAT THIS IS NOT REAL PATIENT DATA!!!
library(readr)
urlfile="https://raw.githubusercontent.com/LMD-nat/follow-ups/main/index_visits8.csv"
mydata<-read_csv(url(urlfile))

index_visits8 <- mydata

# library(readxl)
# index_visits8 <- read_excel("index_visits8.xlsx", 
#                             col_types = c("text", "date", "date", 
#                                           "skip", "skip", "numeric", "skip", 
#                                           "skip", "skip", "numeric", "text", 
#                                           "text", "text", "numeric", "text", 
#                                           "text", "numeric"))

index_visits8$index_arrival_time <- strptime(index_visits8$index_arrival_time, format = "%Y-%m-%d %H:%M")
index_visits8$arrival_time_rv <- strptime(index_visits8$arrival_time_rv, format = "%Y-%m-%d %H:%M")

### BTW days is the time between (in days) between visits, accounting for hours, minutes, etc.
index_visits8$btw_days <- ((index_visits8$arrival_date_rv-index_visits8$index_arrival_date)/60)/24
index_visits8$btw_days <- as.numeric(index_visits8$btw_days) 

followups <- subset(index_visits8, ER_follow_up == 1)
not_followups <- subset(index_visits8, ER_follow_up == 0)

# sample 500 random non-follow up data points from the model
sample_not_followups <- not_followups[sample(nrow(not_followups), 500), ]

join2 <- bind_rows(followups, sample_not_followups)
hc = hclust(join2, method = "complete")

# Ok so join2 is the set to use for making the model, save it as a .csv file too
# write.csv(join2, "~/Downloads/ER_visits_set.csv")
