###############################################################################
# PCA
###############################################################################
RC_PCA_scree = function(Data){
  # PCA cumulative
  res.pca = PCA(Data, graph = FALSE)
  return(fviz_screeplot(res.pca, addlabels = TRUE ))
}
