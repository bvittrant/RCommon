###############################################################################
# Design Check function
###############################################################################
RC_annotationCheck = function(Data, Annotation){

  # Data checking and error handling
  #row_Data = dim(Data)[1]
  col_Data = dim(Data)[2]
  row_Annotation = dim(Annotation)[1]
  col_Annotation = dim(Annotation)[2]

  if(col_Data != row_Annotation){
    return("Your annotation file has not the same number of rows than your number of columns in your data file")
  }
  if(colnames(Annotation)[1] != "Feature"){
    return("The first column of your annotation file shoudl be called Feature and be the same as the colnames of your data file.")
  }
  if(!all(colnames(Data) == Annotation[,1]) ){
    return("Your rownames in Data don't match feature name in Annotation or are not in the same order !")
  }
  # Everything good function can continue
  print(paste("You have", (col_Annotation-1),"descriptors for your features in your annotation file plus the first column Feature:", sep = " "))
  print(colnames(Annotation))
  #return()
}
