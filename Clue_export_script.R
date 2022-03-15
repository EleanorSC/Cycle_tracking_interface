## packages
install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("tidyverse")
install.packages("skimr")

library(jsonlite)
library(tidyverse)
library(skimr)

###

data <- fromJSON('ClueDataExport.cluedata')

#data_flat <- flatten(data)


str(data)

# Combining unlist() and tibble::enframe(), we are able to get a (very) 
# long data.frame without any nested elements!
# Note that the would-have-been-nested elements are joined by “.” in the “name” column, 
# and the values associated with these elements are in the “value” column. 
# (These are the default column names that tibble::enframe() 
# assigns to the tibble that it creates from a list.)

data_raw <- enframe(unlist(data))

# Given the format of the implicit variable sin the “name” column,
# We can use tidyr::separate() to create columns for different nested elements
# that, with my specification of (dummy) column names with the into argument, 
# I guessed that there we would need 7 columns. Why 7? 
# Because I expected that 7 would be more than I needed, and it’s better to over-estimate
# and remove the extra columns in a subsequent step than to under-estimate and 
# lose data because there are not enough columns to put the “separated” data in.

data_raw2 <- data_raw %>% separate(name, 
                      into = c(paste0("x", 1:7)), 
                      fill = "right")

skim(data_raw2)




# How to we specify the correct number of columns to create with separate()?
# We can do that by identifying the name with the most number of .s.

rgx_split <- "\\."
n_cols_max <-
  data_raw %>%
  pull(name) %>% 
  str_split(rgx_split) %>% 
  map_dbl(~length(.)) %>% 
  max()
n_cols_max

# ouput [1] 5, so we need 5 separator columns 
# With this number (5) identified, 
# we can now choose the “correct” number of columns to create with separate().
# Note that we’ll still be left with LOTS of NA values 
# (corresponding to rows that don’t have the maximum number of variables). This is expected.

nms_sep <- paste0("name", 1:n_cols_max)

data_sep <-
  data_raw %>% 
  separate(name, into = nms_sep, sep = rgx_split, fill = "right")
data_sep


data_sep$name2_f <- as.factor(data_sep$name2)

skim(data_2$name)

# nb column X2 contains 'day' variable
# read this to understand rest 
# https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
# ML model using CLue data https://academic.oup.com/jamia/article/29/1/3/6371799

# Need to align each number string at the end of name2 with each other in separate columns (e.g. sex2, day2,)

x <- filter(data_sep, 
            grepl('129', name2)
            )


y <- filter(data_sep, 
            grepl('day', name2)
            )


# Create a 'cycle_index' column
# Separate name2 column into two columns - namely one that just contains the number (e.g. 8, 129)
# and one that contains class (e.g. mood, skin)

Date_cycle<- data_sep %>%
  mutate(cycle_index = readr::parse_number(as.character(name2)))

# tidy

Date_cycle2 <- Date_cycle %>% select(name1, name2, cycle_index, value)

# Create a date column which specifies the day
# Note: The timezone is always zero UTC offset, as denoted by the suffix “Z”.
# 2017-08-10T00:00:00Z

Date <- filter(Date_cycle2, 
            grepl('day', name2)
            )

# rename value column to date 

Date <- Date %>% 
  rename(date = value)


# Now change the format of the date column
Date$MyDate <- as.Date(Date$date)



#Date_cycle<- Date_cycle %>%
#  mutate(date = readr::parse_datetime(as.character(name2)))

#f <- x %>% mutate_all(~gsub("sex|day|digestion|skin|pain|tags|motivation|mental|mood|social|period|poop", 
#                                    "", 
#                                    .)
#                              )




#data_filt <-
#  data_sep %>%
#  filter(
#    (
#      name1 == "data" &
#        name2 == str_detect(name1, '1')
#    ))
#
#
#
#
#data_filt <-
#  data_sep %>%
#  filter(
#    (
#      name1 == "data" &
#        name2 == "sex"
#    ) |
#      (
#        name1 == "data" &
#          name2 == "sex" &
#          name3 == "day"
#      ) | (
#        name1 == "events" &
#          name2 == "competitions" &
#          name3 == "status" &
#          name4 == "type" &
#          name5 == "name"
#      ) |
#      (
#        name1 == "events" &
#          name2 == "competitions" &
#          name3 == "competitors" &
#          name4 == "score"
#      )
#  )
#data_filt
#