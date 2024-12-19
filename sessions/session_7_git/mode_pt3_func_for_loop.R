library(tidyverse)
library(kernlab)
library(kknn)

# import data
data <- as.matrix(read.delim("C:/repos/isye6501/hw/wk_1/data 2.2/credit_card_data-headers.txt"))

data_import <- data

# intialize list
empty_list <- c()

c_list <- c(1, 100, 1000, 10000)

i <- 0

# iterate function w/ diff class
# c = value in c_list
# c_list is the list of classifier distance (margin)
for (c in c_list) {
  
  # for debug
  i <- i + 1
  cat(paste0("index position and value: ", i, ";", as.character(c_list[i])))
  cat("\n")
  
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
    
    return(output_pass)
  }
  
  pass_output <- func_kvsm(data, c)
  
  empty_list <- append(empty_list, pass_output)
}

# output data in df
list_output <- empty_list
df_output <- as.data.frame(list_output)
df_output
