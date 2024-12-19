library(tidyverse)
library(kernlab)
library(kknn)

# import data
data <- as.matrix(read.delim(
  "C:/repos/isye6501/hw/wk_1/data 2.2/credit_card_data-headers.txt"
))

data_import <- data

# create func
func_kvsm <- function(data_import, c) {
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
  
  # model_vanilla, pred, output_pass
  return(pred)
}

# margin
c <- 10

# call func; get output
pass_output <- func_kvsm(data, c)
pass_output
