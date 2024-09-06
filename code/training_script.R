sum(1, 2)

df <- read.csv("C:/Users/adam.nguyen/Downloads/example_data.csv")
df

summary(df$Race)

getwd()

df_2 <- read.csv(paste0(getwd(),"/data/example_data.csv"))
setwd()

paste0(getwd(),"/data/example_data.csv")

# install.packages("tidyverse")

library(tidyverse)

glimpse(df)

