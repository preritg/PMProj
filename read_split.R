library(readr)
diab_data <- read_csv("data/no_code_data.csv",
                      col_types = cols(X1 = col_skip(), diag_1 = col_double(),
                                       diag_3 = col_double(), encounter_id = col_character(),
                                       patient_nbr = col_character(), time_in_hospital = col_number(),
                                       weight = col_number()))
# View(dataset)

library(plotly)
library(nnet)
nrow(diab_data)
library(dplyr)

class_cnt <- diab_data %>%
  select(readmitted) %>%
  group_by(readmitted) %>%
  summarise(n = n())

class_hist <- plot_ly(class_cnt, x = ~readmitted, y = ~n, type = "bar")
class_hist

# removing encounter ID and patient ID
useful_data <- diab_data[, -c(1,2)]

# stratified sampling
# this will be training validation split
library(caret)
train_index <- createDataPartition(diab_data$readmitted, p = 0.7, list = FALSE)
# str(diab_data)

useful_data$readmitted <- as.factor(useful_data$readmitted)

useful_data$result <- relevel(useful_data$readmitted, ref = 'NO')



# We chose the multinom function because it does not require the data to be reshaped 
# (as the mlogit package does) and to mirror the example code found in Hilbe's Logistic Regression Models.

cols <- colnames(useful_data)
for (column in colnames(useful_data)){
  if(nrow(unique(useful_data[, column]))==1){
    print(column)
  }
}

# dropping these columns because they have only one level
drop_cols <- c("examide", "citoglipton")
useful_data <- subset(useful_data, select = -c(examide, citoglipton))


train_diab <- useful_data[train_index, ]
test_diab <- useful_data[-train_index,]


for (column in colnames(useful_data)){
  print(column)
  print(nrow(unique(useful_data[, column])))
}

# logit_model <- multinom(result~., data = train_diab)

require(neuralnet)
require(nnet)
require(ggplot2)
set.seed(10)

char_cols <- c("To be removed")

for (column in colnames(useful_data)){
  print(column)
  print(typeof(useful_data$column))
  # print(nrow(unique(useful_data[, column])))
}
