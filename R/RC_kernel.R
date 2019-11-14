###############################################################################
# Kernel function
###############################################################################
RC_kernel = function(Data_melt, color1, vertical1, Title, log2 = TRUE){
  # Log2 the value
  if(log2 == TRUE){Data_melt$value = log2(Data_melt$value+1) }
  # kernel plot
  p = ggplot(Data_melt, aes(value)) +
    geom_density(aes_string(fill = color1), alpha = 0.1) +
    theme(legend.position="right", legend.text=element_text(size=5)) +
    geom_vline(xintercept = vertical1, linetype="dashed", size = 0.3) +
    labs(x = Title)

  # The function will return:
  return(p)
}
