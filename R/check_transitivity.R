check_transitivity <- function(data){
  temp_list <- combinat::combn(data, 3)
  counter <- 0

  for(i in 1:ncol(temp_list)){
    if(temp_list[1,i] >= temp_list[2,i] && temp_list[2,i] >= temp_list[3,i] && temp_list[1,i] <= temp_list[3,i]){
      counter <- counter + 1
    }
    else if(temp_list[1,i] <= temp_list[2,i] && temp_list[2,i] <= temp_list[3,i] && temp_list[1,i] >= temp_list[3,i]){
      counter <- counter + 1
    }
  }

  print(paste("number of violations", counter))
}
