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
  tmp = apply(Data>Filter,1,FUN=function(x)sum(x=="TRUE")) > NSample
  Data_filtered = Data[tmp,]
  # Some printing
  print(paste("Start : ", ncol(Data), sep=""))
  print(paste("End : ", nrow(Data_filtered), sep=""))
  print(paste("Diff : ", (ncol(Data)-nrow(Data_filtered)), sep=""))
  return(Data_filtered)
}
