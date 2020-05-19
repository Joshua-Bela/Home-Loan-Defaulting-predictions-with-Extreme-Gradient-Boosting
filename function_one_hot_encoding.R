input_directory = 'C:/Users/jgbel/Desktop/data/Loan Defaulting/application_train.csv'
output_directory = 'C:/Users/jgbel/Desktop/data/Loan Defaulting/application_train_onehot.csv'
#########################################################################################################
data = read.csv(input_directory)
library(magrittr)

one_hot_encode_column = function(data, name){
  column = which(names(data) == name)
  
  if(length(levels(data[,column])) > 100){
    print("Error:  More than 100 levels.")
    return(data)
  }
  if(!is.factor(data[,column])){
    print("Error:  Column is not a factor.")
    return(data)
  }
  
  levels0 = levels(data[,column])
  data[,column] = ifelse(
    is.na(data[,column]),
    "NA",
    data[,column] %>% as.character()
  ) %>% as.factor()
  for(level in levels0){
    data = cbind(
      data,
      ifelse(
        data[,column] == level,
        1,
        0
      )
    )
  }
  n = ncol(data)
  l = length(levels0)
  names(data)[(n - l + 1):n] = paste(
    names(data)[column],
    levels0,
    sep = "_"
  )
  data = data[,-column]
  return(data)
}
one_hot_encode = function(data){
  names_original = names(data)
  
  for(name in names_original){
    if(is.factor(data[,name])){
      data = one_hot_encode_column(data, name)
    }
  }
  return(data)
}
# Pass in a data frame.
# Function returns a new data frame. where
# all factors are replaced with binary variables,
# if the number of levels is less than or equal to 100.
data0 = one_hot_encode(data)
#########################################################################################################
write.csv(
  data0,
  output_directory,
  row.names = F
)