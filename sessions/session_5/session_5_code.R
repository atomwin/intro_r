# lib
library(tidyverse)
library(janitor)
library(DescTools)

# Session 1: import and clean data #############################################
df_csv <- read.csv(paste0(getwd(), "/data/example_data.csv")) 

df_csv_clean_names <- df_csv %>% clean_names()

df_csv_new_column_names <- df_csv_clean_names %>% 
  rename("ethnicity" = "hispanic", "exercise" = "excerise")

df_csv_select <- df_csv_new_column_names %>% 
  select(dob, ethnicity, race, sex, zip_code, insurance, exercise, everything())

df_csv_formating <- df_csv_select
# Session 1: import and clean data #############################################

# Session 3: column manipulation  ##############################################
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
mean_strenth_overall <- sum(df_clean_race_strength[,8:10]) / nrow(df_clean_race_strength) # nrow 
mean_strenth_overall
# Session 3: column manipulation  ##############################################

# Session 4: Function, code in session 3 hw solutions. #########################
func_clean_col <- function(df, col_name, regex, output){
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  lower_col <- paste0("tolow_", col_name)
  edit_col <- paste0("edit_", col_name)
  
  df_test <- df %>% 
    mutate(!!lower_col := trimws(tolower(!!sym(col_name))),
           !!edit_col := case_when(
             !!sym(lower_col) %like% regex[1] ~ output[1], 
             !!sym(lower_col) %like% regex[2] ~ output[2], 
             TRUE ~ !!sym(lower_col))
    )
  
  output_sort <- output %>% sort()
  
  # if case_when can not cover everything- then NA, run this before df_final for ex: 
  # unique(df_clean_race_strength$exercise)
  df_final <- df_test %>%
    mutate(!!edit_col := ifelse(!!sym(edit_col) %!in% output_sort, NA, !!sym(edit_col))) %>% 
    select(
      #  !!col_name
      # ,!!lower_col
      !!edit_col)
  
  return(df_final)
}

# call function
# ^ indicates beginning of string, % is anything after
test_df_eth <- func_clean_col(df_clean_race_strength, "ethnicity", c("^h%", "^not%"), c("h","nh"))
test_df_sex <- func_clean_col(df_clean_race_strength, "sex", c("^fe%", "^ma%"), c("f","m"))
test_df_exe <- func_clean_col(df_clean_race_strength, "exercise", c("^ye%", "^no"), c("y","n"))
test_df_ins <- func_clean_col(df_clean_race_strength, "insurance", c("^ye%", "^no"), c("y","n"))

# use cbind() to join the new columns
df_bind_clean_columns <- cbind(df_clean_race_strength, test_df_eth, test_df_sex, test_df_exe, test_df_ins) %>% 
  select(dob, ethnicity, edit_ethnicity, race, sex, edit_sex, zip_code, insurance, edit_insurance, exercise, edit_exercise,everything())
# Session 4: Function, code in session 3 hw solutions. #########################

# Session 5: Datetimes #########################################################

# go over ppt, go over online book

# why datetime, we can view it in char but can not order ex =

char_dates <- c("01-01-2000", "01-02-1999", "01-03-1998", "09-01-1900")

# order from oldest date to closest date
char_dates %>% sort() # fail

# order from oldest date to closest date
dt_dates <- as.Date(char_dates, tryFormats = "%m-%d-%Y")
dt_dates %>% sort() # pass

# start coding to clean
# func: clean dates
func_clean_dates <- function(dates) {
  # Fixing the "5=11-1984" entry to a valid format
  dates <- gsub("=", "/", dates)
  
  # Convert different date formats using lubridate's parse_date_time
  dates_cleaned <- parse_date_time(dates, 
                                   orders = c("mdy", "dmy"), 
                                   exact = FALSE)
  
  # Format the cleaned dates in MM/DD/YYYY
  dates_final <- format(dates_cleaned, "%m/%d/%Y")
  return(dates_final)
}

vec_dob_clean <- func_clean_dates(df_bind_clean_columns$dob)
vec_dob_clean

# show side by side
df_dob_clean <- df_bind_clean_columns %>% 
  mutate(dob_clean = vec_dob_clean) %>% 
  select(dob, dob_clean, everything())

glimpse(df_dob_clean)

# now convert to dt
df_dob_dt_clean <- df_dob_clean %>% 
  mutate(dob_dt = as.Date(dob_clean, format = "%m/%d/%Y"),
         dob_format = format(dob_dt, "%m-%d-%Y")) %>% 
  select(dob, dob_clean, dob_dt, dob_format, everything())
# Session 5: Datetimes #########################################################
