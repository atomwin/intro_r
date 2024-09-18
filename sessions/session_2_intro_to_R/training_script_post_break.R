library(tidyverse)

# post break, exploring datasets
df <- read.csv("C:/Users/adam.nguyen/Downloads/example_data.csv")

df_ins <- df %>% select(insurance)

df_in_low <- df_ins %>% mutate(insur_lower = tolower(insurance))

df_clean <- df_in_low %>% mutate(insur_clean = case_when(
  insur_lower == "n" ~ "no",
  insur_lower == "y" ~ "yes",
  TRUE ~ insur_lower
))

df_clean_insur <- df_clean %>% select(insur_clean)

df_add_clean <- cbind(df, df_clean_insur)
df_add_clean

df_final <- df_add_clean %>% select(-insurance)

write.csv(df_final, "C:/Users/adam.nguyen/Downloads/final.csv")

# regex--------------------------------------------------------
# vector <- unique(df_in_low$insur_lower)
# 
# vector_clean <- gsub('^.*n$', 'no', vector)  #^ start of the line, $ end of line, .* is searching until n
# regex--------------------------------------------------------

# ifelse for cleaning no
# df_clean <- df_in_low %>% mutate(insur_clean = ifelse(insur_lower == "n", "no", insur_lower))
# df_clean
