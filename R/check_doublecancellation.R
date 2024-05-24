check_doublecancellation <- function(data){

  #if data entered is matrix data
  if(is.matrix(data)){

    #declare and initialize all relevant objects
    column_combinations <- as.data.frame(combinat::combn(ncol(data), 3))
    row_combinations <- as.data.frame(combinat::combn(nrow(data), 3))

    temp_list <- list()

    counter <- 0

    row_column_permutations <- as.data.frame(combinat::permn(c(1,2,3)))


    #what are the possible combinations of the mxn matrix
    for(i in 1:ncol(column_combinations)){
      for(j in 1:ncol(row_combinations)) {

        column_vec <- column_combinations[ , i]
        row_vec <- row_combinations[, j]

        temp_mat <- data[row_vec, column_vec]



        temp_list[[length(temp_list) + 1]] <- temp_mat
      }
    }


    for(k in 1:length(temp_list)){

      #gets every submatrix of the temporary list containing all possible 3x3 submatrices

      specificsubmatrix <- temp_list[k]
      specificsubmatrix <- do.call(rbind, as.matrix(specificsubmatrix))


      # new temporary list for all row and column combinations of every 3x3 submatrix
      neu_temp <- list()

      for(i in 1:length(row_column_permutations)){
        row_permutation <- row_column_permutations[, i]
        for(j in 1:length(row_column_permutations)){
          column_permutation <- row_column_permutations[ , j]
          specificsubmatrix1 <- specificsubmatrix[row_permutation, column_permutation]

          neu_temp[[length(neu_temp) + 1]] <- specificsubmatrix1
        }
      }

      #checks every row and column combination of the l-th 3x3 submatrix
      for(l in 1:length(neu_temp)) {

        specificsubmatrix <- neu_temp[l]
        specificsubmatrix <- do.call(rbind, as.matrix(specificsubmatrix))


        if(specificsubmatrix[2,1] <= specificsubmatrix[3,2] && specificsubmatrix[1,2] <= specificsubmatrix[2,3] && specificsubmatrix[1,1]>= specificsubmatrix[3,3] ) {
          counter <- counter + 1
        }
        else if(specificsubmatrix[2,1] >= specificsubmatrix[3,2] && specificsubmatrix[1,2] >= specificsubmatrix[2,3] && specificsubmatrix[1,1]<= specificsubmatrix[3,3]){
          counter <- counter + 1
        }
      }





    }
    print(paste0("Number of violations: ", counter))
  }

}
