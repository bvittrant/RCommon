###############################################################################
# Design Check function
###############################################################################
RC_designCheck = function(Data, Design){

  # Data checking and error handling
  row_Data = dim(Data)[1]
  col_Data = dim(Data)[2]
  row_Design = dim(Design)[1]
  col_Design = dim(Design)[2]

  if(row_Data != row_Design){
    return("Your design file has not the same number of rows than your data file")
  }

  print("I tested your rownames from Data and Design with the all.equal function:")
  print(all.equal(row.names(Data), row.names(Design)))

  # Everything good function can continue
  print(paste("You have",row_Data,"samples with",col_Data,"features in your Data file.",sep=" "))
  print(paste("And",col_Design,"descriptors for your sample in your Design file:", sep = " "))
  print(colnames(Design))
}
