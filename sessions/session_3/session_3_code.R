################################################################################
# Session 1: import and clean data #############################################
################################################################################
# tip move console to top right (defaults to bottom left)
# writing in console is like writing a sticky note then forgetting about it
# writing in source is like writing in word doc and saving instructions

# library and functions: tool set, whats in the tool set- hammer, chainsaw
# install.packages("tidyverse")
library(tidyverse)
library(janitor)

# import data-------------------------------------------------------------------
#  why not \ ? Because \ are arguments like < > + - ;ex "\d" in regex
## you can use \\ as well

# > < + *

df_csv <- read.csv(paste0(getwd(), "/data/example_data.csv")) 
# view(df_csv)

# quick info datatypes
glimpse(df_csv) # tidyverse function

str(df_csv) # base r function 

# base r vs libraries: creator vs contributors: inventors vs improvements

# what are objects/data structures? for now we focus data frames (tables)
# what are characters? 
# what are integers? 1, 2, 3, 4 
# what are numeric? 1.11, 2.333
# what are logic? TRUE or FALSE
# what are datetimes? 2001-01-15 , the most diffucult bc time is precise (decimal matters)
# link obj explanation: https://www.geeksforgeeks.org/data-structures-in-r-programming/#dataframes
# link datatypes: https://www.geeksforgeeks.org/r-data-types/?ref=shm

# start of cleaning, standardize the data! Follow a consistent format
colnames(df_csv)
# import data-------------------------------------------------------------------

# clean col names---------------------------------------------------------------
# use clean_names() to clean column names
df_csv_clean_names <- df_csv %>% clean_names() # pipe operator
colnames(df_csv_clean_names)

df_csv_clean_names <- clean_names(df_csv, case = "screaming_snake") # no pipe operator

df_csv_clean_names <- clean_names(df_csv) # no pipe operator, default

# df_csv_clean_names <- df_csv %>% clean_names(case = "screaming_snake") # show how to read documentation, why and how ?
# # clean_names() is like a tradional hammer, clean_names(case = "screaming_snake") is a modified hammer ie slege hammer
# colnames(df_csv_clean_names)

colnames(df_csv)
colnames(df_csv_clean_names)

# use rename to rename column names
# show rename documentation, ex petal.width -> petal_width
df_csv_new_column_names <- df_csv_clean_names %>% 
  rename("ethnicity" = "hispanic", "exercise" = "excerise")

# reorganize columns or select which columns what we want
df_csv_select <- df_csv_new_column_names %>% 
  select(dob, ethnicity, race, sex, zip_code, insurance, max_squat, exercise)

# how can we consolidate this code? pipe operator
df_csv_formating <- df_csv %>%clean_names() %>% rename("ethnicity"= "hispanic", "exercise"= "excerise") %>% select(dob,ethnicity,race,sex, zip_code, insurance, exercise, max_squat)

# writing clean code is important! Good code = understandable code
# ctrl + shift + a
df_csv_formating <- df_csv %>% 
  clean_names() %>% 
  rename("ethnicity" = "hispanic", "exercise" = "excerise") %>% 
  select(dob,
         ethnicity,
         race,
         sex,
         zip_code,
         insurance,
         exercise,
         max_squat)
# clean col names---------------------------------------------------------------

# FINAL CODE--------------------------------------------------------------------
# add into k drive so everyone can access/run

df_csv <- read.csv(paste0(getwd(), "/data/example_data.csv")) 

df_csv_clean_names <- df_csv %>% clean_names()

df_csv_new_column_names <- df_csv_clean_names %>% 
  rename("ethnicity" = "hispanic", "exercise" = "excerise")

df_csv_select <- df_csv_new_column_names %>% 
  select(dob, ethnicity, race, sex, zip_code, insurance, exercise, everything())

df_csv_formating <- df_csv_select

# df_csv_formating <- df_csv %>% 
#   clean_names() %>% 
#   rename("ethnicity" = "hispanic", "exercise" = "excerise") %>% 
#   select(dob, ethnicity, race, sex, zip_code, insurance, exercise, max_squat, everything())

# FINAL CODE--------------------------------------------------------------------
################################################################################
# Session 1: import and clean data #############################################
################################################################################

# Session 2: 2 hr intro to R training session

################################################################################
# Session 3: column manipulation  ##############################################
################################################################################
library(tidyverse)
# library(insight), we will use but dont call bc conlict with janitor lib. use ::

# cleaning data in data frames
# use mutate, case_when to clean the race column
unique(df_csv_formating$race)

df_csv_race_edit <- df_csv_formating %>%
  mutate(
    edit_race = case_when(
      race == "White" ~ "white",
      race == "WHITE" ~ "white",
      race == "Other" ~ "other",
      race == "Black" ~ "black",
      race == "BLACK" ~ "black",
      race == "Mixed" ~ "mixed",
      race == "Caucasian" ~ "white",
      race == "CaucasiaN" ~ "white",
      race == "am-af" ~ "black",
      race == "Asian" ~ "asian",
      race == "ASiaN" ~ "asian",
      TRUE ~ race
    )
  ) %>% 
  select(race, edit_race)

# see the unique combinations to check work
unique(df_csv_race_edit[,c('race','edit_race')])

# why do more work? STANDARIZE!, show why we dont need white ~ white bc of TRUE
df_csv_race_edit_lower <- df_csv_formating %>%
  mutate(
    lower_case_race = tolower(race),
    edit_race = case_when(
      lower_case_race == "white" ~ "white", # comment out later to show better code
      lower_case_race == "other" ~ "other", # comment out later to show better code
      lower_case_race == "black" ~ "black", # comment out later to show better code
      lower_case_race == "mixed" ~ "mixed", # comment out later to show better code
      lower_case_race == "asian" ~ "asian", # comment out later to show better code
      lower_case_race == "caucasian" ~ "white",
      lower_case_race == "am-af" ~ "black",
      TRUE ~ lower_case_race
    )
  )  # %>% 
   # select(race, lower_case_race, edit_race)

unique(df_csv_race_edit_lower[,c('race','edit_race')])
# 
# # how can you test if correct? QA ie unit test?
# # create vector of what we expect
vec_race <- c("white","black", "asian", "mixed", "other") %>% sort()
# 
# df to show combination, just like unique
qa_race_edit <- df_csv_race_edit_lower %>%
  group_by(edit_race) %>%
  reframe(og = unique(race)) %>%
  arrange(edit_race)
# 
if (identical(unique(qa_race_edit$edit_race), vec_race) == TRUE){
  insight::print_color("PASS: vectors match", "green")
} else {
  insight::print_color("!!FAIL: vectors NO match!!", "red")
}

df_csv_race_clean <- df_csv_race_edit_lower %>% 
  mutate(race = edit_race) %>% 
  select(-c("lower_case_race", "edit_race"))
# now in scenarios where there are a lot of unique values for race;
# regular expression (gsub()) or
# string mathing (lib(fuzzywuzzyR)) may help find the best replacement


# calc rowMean.... do mean() and old school way ie sum/3 or sum/length(of unqiue col)

# rowMeans
df_csv_average <- df_csv_race_clean %>%
  mutate(
    avg_strength_manual = (max_squat + max_bench + max_deadlift) / 3,
    avg_strength_func = round(rowMeans(select(., c(8:10))))
  ) # %>%  #  select(., max_squat,max_bench ,max_deadlift) | select(., c(max_squat,max_bench ,max_deadlift)) 
  # why use function vs manual? easier to create into a function tbh, for next week
  # select(-avg_strength_manual)

# average strengh of contestants
# mean_strenth_overall <- sum(df_csv_race_clean$max_squat, df_csv_race_clean$max_deadlift, df_csv_race_clean$max_bench) / 20 # nrow
mean_strenth_overall <- sum(df_csv_race_clean[,8:10]) / nrow(df_csv_race_clean) # nrow 
mean_strenth_overall

# no the correct output
mean_bench <- mean(c(df_csv_race_clean$max_squat, df_csv_race_clean$max_deadlift, df_csv_race_clean$max_bench))
mean_bench   # mean(sum(c(df_csv_race_clean$max_squat, df_csv_race_clean$max_deadlift, df_csv_race_clean$max_bench)))




# FINAL CODE--------------------------------------------------------------------
df_clean_race_strength <- df_csv_formating %>%
  mutate(
    lower_case_race = tolower(race),
    edit_race = case_when(
      lower_case_race == "caucasian" ~ "white",
      lower_case_race == "am-af" ~ "black",
      TRUE ~ lower_case_race
    ),
    race = edit_race,
    avg_strength = round(rowMeans(select(., max_squat, max_bench ,max_deadlift)))
  ) %>% 
  select(-c("lower_case_race", "edit_race")) 

# qa
# unique(df_clean_race_strength[,c('race','edit_race')])
# unique(df_clean_race_strength[,c('lower_case_race','edit_race')])

vec_race <- c("white","black", "asian", "mixed", "other") %>% sort() 

if (identical(sort(unique(df_clean_race_strength$race)), vec_race) == TRUE){
  insight::print_color("PASS: vectors match", "green")
} else {
  insight::print_color("!!FAIL: vectors NO match!!", "red")
}

# average strengh of contestants
mean_strenth_overall <- sum(df_csv_race_clean[,8:10]) / nrow(df_csv_race_clean) # nrow 
mean_strenth_overall
# FINAL CODE--------------------------------------------------------------------

################################################################################
# Session 3: column manipulation  ##############################################
################################################################################


################################################################################
# WK 3: datetime WORK IN PROGRESSS##############################################
################################################################################


################################################################################
# WK 3: datetime WORK IN PROGRESSS##############################################
################################################################################

################################################################################
# WK 4: for logical value for x, create function using casewhen? refer to sex,
# insurance col ################################################################

glimpse(df_clean_race_strength)

df_sex <- df_clean_race_strength %>%
  mutate(new_sex = case_when(
    grepl('%m', tolower(sex)) ~ "m",
    grepl('%f', tolower(sex)) ~ "f"
  ))

df_sex


# then complete with race to create race_ethnicity
################################################################################

################################################################################
# WK 4: for logical value for x, create function using casewhen? refer to sex,
# insurance col ################################################################
################################################################################

# group by for standard dev of sex strenghth

