library(tidyverse)
library(kernlab)
library(kknn)

# import data
data <- as.matrix(read.delim(
  "C:/repos/isye6501/hw/wk_1/data 2.2/credit_card_data-headers.txt"
))

data_import <- data
# data1 <- read.delim("C:/repos/isye_6501/hw/wk_1/data 2.2/credit_card_data.txt")

# choose margin for c (the gap between; higher more gap but more moe, lower gap less moe)
# why higher gap? less say u want high threshold bc u try trying to classify
# bad mushroom vs good; higher risk bc death therefore higher gap!
c <- 100

model_vanilla <- ksvm(
  data_import[, 1:10],
  # x
  data_import[, 11],
  # y, results
  type = "C-svc",
  # model we learned in class
  kernel = "vanilladot",
  C = c,
  # margin of classifier
  scaled = TRUE
)

pred <- predict(model_vanilla, # the trained model with classifier
                data_import[, 1:10]) # the test data

# show what percentage of pass
output_pass <- sum(pred == data_import[, 11]) / nrow(data_import)
print(output_pass)
print(c)
