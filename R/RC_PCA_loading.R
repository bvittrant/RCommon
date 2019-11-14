###############################################################################
# PCA Loading
###############################################################################
RC_PCA_loading = function(data, cos2){
  res.pca = PCA(data, graph = FALSE)
  tmp = median(res.pca$var$contrib)

  p = fviz_pca_var(res.pca, col.var="contrib", select.var = list(cos2=cos2)) +
    scale_color_gradient2(low="white", mid="blue",
                          high="red", midpoint=tmp, space = "Lab") +
    theme_minimal()
  return(p)
}
