###############################################################################
# Inversion function for data frame when memory allocation problem
# Homemade loop
###############################################################################

RC_transpose = function(Data, verbose = T){
  tmp = matrix(ncol = nrow(Data), nrow = ncol(Data), NA) %>% as.data.frame
  # We just loop to transfert row in col and col in row
  for(i in 1:nrow(tmp)){
    tmp[i,] = Data[,i]
    if(verbose == T & i%%100 == 0){print(i)}
  }
  colnames(tmp) = row.names(Data)
  rownames(tmp) = colnames(Data)
  return(tmp)
}
