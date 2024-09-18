################################################################################
# WK 1: import and clean data ##################################################
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

> < + *

df_csv <- read.csv("C:/GitHub Repo/Adams_Devops/ri_training/draft_wk/example_data.csv") 
view(df_csv)

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
df_csv <- read.csv("C:/GitHub Repo/Adams_Devops/ri_training/draft_wk/example_data.csv")  

df_csv_formating <- df_csv %>% 
  clean_names() %>% 
  rename("ethnicity" = "hispanic", "exercise" = "excerise") %>% 
  select(dob, ethnicity, race, sex, zip_code, insurance, exercise, max_squat)

# or u can write it like this:
df_csv <- read.csv("C:/GitHub Repo/Adams_Devops/ri_training/draft_wk/example_data.csv")  

df_csv_clean_names <- df_csv %>% clean_names()

df_csv_new_column_names <- df_csv_clean_names %>% 
  rename("ethnicity" = "hispanic", "exercise" = "excerise")

df_csv_select <- df_csv_new_column_names %>% 
  select(dob, ethnicity, race, sex, zip_code, insurance, exercise, max_squat)

df_csv_formating <- df_csv_select


# FINAL CODE--------------------------------------------------------------------
################################################################################
# WK 1: import and clean data ##################################################
################################################################################


################################################################################
# WK 2: clean data WORK IN PROGRESSS#############################################################
################################################################################

# cleaning data in data frames
# use mutate, case_when to clean the race column
unique(df_csv_formating$race)

df_wk2_case_when <- df_csv_formating %>%
  mutate(
    edit_race = case_when(
      race == "White" ~ "white",
      race == "Other" ~ "other",
      race == "Black" ~ "black",
      race == "Mixed" ~ "mixed",
      race == "Caucasian" ~ "white",
      race == "Af-Am" ~ "black",
      race == "Asian" ~ "asian",
      race == "WHITE" ~ "white",
      TRUE ~ race
    )
  )

unique()
# why do more work? STANDARIZE!
df_wk2_case_when_lower <- df_csv_formating %>%
  mutate(
    lower_case_race = tolower(race)
  )

unique(df_wk2_case_when_lower$lower_case_race)
unique(df_csv_formating$race)

# calc mean.... do mean() and old school way ie sum/3 or sum/length(of unqiue col)

# FINAL CODE--------------------------------------------------------------------
################################################################################
# WK 2: clean data WORK IN PROGRESSS#############################################################
################################################################################


################################################################################
# WK 3: datetime WORK IN PROGRESSS###############################################################
################################################################################


################################################################################
# WK 3: datetime WORK IN PROGRESSS###############################################################
################################################################################

