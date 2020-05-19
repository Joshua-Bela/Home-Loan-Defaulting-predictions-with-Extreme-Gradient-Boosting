input_directory = 'C:/Users/jgbel/Desktop/data/Loan Defaulting/application_train_onehot.csv'
output_directory = 'C:/Users/jgbel/Desktop/data/Loan Defaulting/application_train_onehot_imputed.csv'
#########################################################################################################
data = read.csv(input_directory)
library(magrittr)

impute_mean = function(data){
  for(i in 1:ncol(data)){
    if(is.numeric(data[,i])){
      mean0 = data[,i] %>% mean(na.rm = T)
      data[,i] = ifelse(
        is.na(data[,i]),
        mean0,
        data[,i]
      )
    }
  }
  return(data)
}
data0 = impute_mean(data)
#########################################################################################################
write.csv(
  data0,
  output_directory,
  row.names = F
)