###############################################################################
# PCA
###############################################################################
RC_PCA = function(Data, Design, Project_Name, ColorVar = NULL){
  tmp = ColorVar
  # PCA run
  res.pca = PCA(Data,  graph = FALSE)

  # PCA plot
  if(is.null(tmp)) {
    p = fviz_pca_ind(res.pca, label = "none") # hide individual labels)
  }else{
    p = fviz_pca_ind(res.pca,
                     label = "none", # hide individual labels
                     col.ind = Design[, tmp], # color by groups
                     palette = Design[, tmp] %>% unique %>% length %>% viridis,
                     addEllipses = TRUE # Concentration ellipses
    )
  }

  # The function will return:
  return(p)
}
