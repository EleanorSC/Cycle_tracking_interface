## packages
install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("tidyverse")
install.packages("skimr")

library(jsonlite)
library(tidyverse)
library(skimr)

###

data <- fromJSON('ClueDataExport.cluedata')

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
#  that, with my specification of (dummy) column names with the into argument, 
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

# nb column X2 contains 'day' variable
# read this to understand rest 
# https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
# ML model using CLue data https://academic.oup.com/jamia/article/29/1/3/6371799

data_filt <-
  data_sep %>%
  filter(
    (
      name1 == "data" &
        name2 == "sex"
    ) |
      (
        name1 == "data" &
          name2 == "sex" &
          name3 == "day"
      ) | (
        name1 == "events" &
          name2 == "competitions" &
          name3 == "status" &
          name4 == "type" &
          name5 == "name"
      ) |
      (
        name1 == "events" &
          name2 == "competitions" &
          name3 == "competitors" &
          name4 == "score"
      )
  )
data_filt
