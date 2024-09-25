library(tidyverse)
library(janitor)
library(DescTools)

# import data
df_csv <- read.csv(paste0(getwd(), "/data/example_data.csv")) 

# session 1; standardize cols
df_csv_formating <- df_csv %>%
  clean_names() %>%
  rename("ethnicity" = "hispanic", "exercise" = "excerise") %>%
  select(dob, ethnicity, race, sex, zip_code, insurance, exercise, max_squat, everything())

# session 2; two hr training (no code)
# 

# session 3; col manipulation and functions
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

# average strengh of contestants
mean_strenth_overall <- sum(df_csv_race_clean[,8:10]) / nrow(df_csv_race_clean) # nrow 
mean_strenth_overall

# hw 3 solutions below: --------------------------------------------------------
df_clean_cols <- df_clean_race_strength %>%
  mutate(
    tolow_eth = trimws(tolower(ethnicity), which = "both"),
    # can add more standardizing clean like removing trailing whitespaces
    tolow_sex = trimws(tolower(sex), which = "both"),
    tolow_exe = trimws(tolower(exercise), which = "both"),
    tolow_ins = trimws(tolower(insurance), which = "both"),
    edit_eth = case_when(
      tolow_eth %like% "^h%" ~ "h",
      # what are limitations in using regex? if output is "henry" can we consider that as hispanic?
      tolow_eth == "not hispanic" ~ "nh",
      # what are the limitation for this? what if output is "no his"
      TRUE ~ tolow_eth
    ),
    edit_sex = case_when(
      tolow_sex == "female" ~ "f",
      tolow_sex == "male" ~ "m",
      TRUE ~ tolow_sex
    ),
    edit_exe = case_when(
      tolow_exe == "yes" ~ "y",
      tolow_exe == "no" ~ "n",
      tolow_exe %in%  c("many", "") ~ NA,
      TRUE ~ tolow_exe
    ),
    edit_ins = case_when(
      tolow_ins == "yes" ~ "y", 
      tolow_ins == "no" ~ "n", 
      TRUE ~ tolow_ins),
  ) #%>% 
  # select(ethnicity,
  #        sex,
  #        exercise,
  #        insurance,
  #        edit_eth,
  #        edit_sex,
  #        edit_exe,
  #        edit_ins)
df_clean_cols

# bonus, create race & ethnicity column
df_race_eth_combined <- df_clean_cols %>% 
  mutate(re_paste = paste(edit_eth, race),
         race_ethnicity = case_when(
           re_paste %like% "^h %" ~ "h",
           TRUE ~ re_paste)
         )

# bonus bonus: -----------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

# this function is only designed for 2 outputs y | n
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
  
  # if statment if two regex can not cover everything, run this for ex: 
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
test_df_eth <- func_clean_col(df_clean_race_strength, "ethnicity", c("^h%", "^not%"), c("h","nh"))
test_df_sex <- func_clean_col(df_clean_race_strength, "sex", c("^fe%", "^ma%"), c("f","m"))
test_df_exe <- func_clean_col(df_clean_race_strength, "exercise", c("^ye%", "^no"), c("y","n"))
test_df_ins <- func_clean_col(df_clean_race_strength, "insurance", c("^ye%", "^no"), c("y","n"))


# use cbind() to join the new columns
df_bind_clean_columns <- cbind(df_clean_race_strength, test_df_eth, test_df_sex, test_df_exe, test_df_ins)

as.logical.factor(df_bind_clean_columns$edit_exercise)

# bonus bonus bonus: -----------------------------------------------------------------
# function for calculating average strength of combined 3 lifts in dataset (combine all contestants)
func_overall_avg_str <- function(df, col_index =  c(8:10)){
  
  avg_str <- sum(df[, col_index]) / nrow(df)
  
  return(avg_str)
}

# avg for deadlift, squat, bench
source_avg_str <- func_overall_avg_str(df_race_eth_combined)
source_avg_str

# avg for squat & deadlift
# note how func is applicable to not only find average of 3 lifts, but also 2 lifts. 
# good practice to write code that is versatile and maintainable. 
source_avg_dea_sqa <-func_overall_avg_str(df_race_eth_combined, col_index = c(8,10))
source_avg_dea_sqa