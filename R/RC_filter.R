###############################################################################
# Filter data frame function
###############################################################################
RC_filter = function(Data, Filter, NSample){
  # Filter the row in Data which have at least Nsample with more
  # than (2^Filter-1) counts
  # Because we usually choose a filter from a log2 transfo density graph
  # We used this value as filter and then we re-transformed it to be
  # coherent with raw counts
  # Data_t = as.data.frame(t(Data))
  tmp = apply(Data>Filter, 2, FUN = function(x)sum(x == "TRUE")) > NSample
  tmp = colnames(Data)[tmp]
  Data_filtered = Data[, colnames(Data) %in% tmp]
  # Some printing
  print(paste("Start : ", ncol(Data), sep=""))
  print(paste("End : ", ncol(Data_filtered), sep=""))
  print(paste("Diff : ", (ncol(Data)-ncol(Data_filtered)), sep=""))
  return(Data_filtered)
}
