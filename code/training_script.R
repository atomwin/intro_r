# Install (if necessary) and load packages for use
library(pacman)


pacman::p_load(rio, tidyverse, here)


sum(1, 2)

df <- read.csv("C:/Users/adam.nguyen/Downloads/example_data.csv")
df

summary(df$Race)

getwd()

df_2 <- read.csv(
  paste0(getwd(),
         "/data/example_data.csv")
  )


setwd()

paste0(getwd(),"/data/example_data.csv")

# install.packages("tidyverse")

library(tidyverse)

glimpse(df)

sum <- "1"+ "2"

sum <- 1 + 2 

# why use char
name <- "adam"

name_upper <- toupper(name)
name_upper

df_base_r_dob <- df$dob
str(df_base_r_dob)

str(as.data.frame(df_base_r_dob))

df_tidy_dob <- df %>% select(dob)
df_tidy_dob

# why use pipe op
df_squat_sum <- df %>% count(max_squat)

df_squat_sum_no_pipe <- count(df, max_squat)
  
# operators
name <- "adam"
name

vec_num <- c(1:10) 
vec_num

one_three <- vec_num %in% c(1:3)
one_three

a <- 1
a
b <- c(2:4)
b

a == b
a != b

#################################################### post break ########################################################################


