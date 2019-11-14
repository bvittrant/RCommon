###############################################################################
# Volcano plot
###############################################################################
RC_volcano = function(res, t_pvalue, t_FC, contrast2, Fold, Stat, xlim, np, list_names = NULL){
  # res, a dataframe containing DE results from any package
  # t_pvalue & t_FC are defined threshold by yourself
  # contrast2, just a name
  # Fold, the colname to use as FC value from your res
  # Stat, the colname to use a statistics from your res,
  # Number of name to plot in the graph
  # xlim, just xlim for the plot
  # np = number of point label to show
  # list_name, a vector of name to color point in green
  res_VP = res %>% as.data.frame
  res_VP = na.omit(res_VP)
  res_VP$names = row.names(res_VP) %>% as.factor
  res_VP$names = row.names(res_VP)

  # Color for up, none and down features
  Up = "#E64B35"
  None = "Gray"
  Down = "#3182bd"
  common = "green"

  # Add a color column
  res_VP$color = "None"
  res_VP[(res_VP[, Stat] < t_pvalue & res_VP[, Fold] > t_FC),"color"] = "Up"
  res_VP[(res_VP[, Stat] < t_pvalue & res_VP[, Fold] < -t_FC),"color"] = "Down"
  res_VP$Y = -log10(res_VP[, Stat])

  # transform some point in green if they match a vector of names
  if(!is.null(list_names)){
    res_VP[list_names,"color"] = "common"
  }

  g = ggplot(res_VP, aes_string(x=Fold, y = "Y")) +
    # Add points
    geom_point(aes(colour = factor(color))) +
    # Add horizontal line
    geom_hline(yintercept=-log10(t_pvalue), linetype="dashed", size = 0.3) +
    # Add vertical lines
    geom_vline(xintercept = -t_FC, linetype="dashed", size = 0.3  ) +
    geom_vline(xintercept = t_FC, linetype="dashed", size = 0.3) +
    # Add axes labels
    xlab(Fold) + ylab(paste("-log10", Stat, sep = " ")) +
    # Define xlim
    xlim(c(-xlim, xlim)) +
    # Chose theme and color
    theme_bw() + scale_color_manual(values = c("Up" = Up,
                                               "Down" = Down,
                                               "None" = None,
                                               "common" = common)) +
    theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
    # Add title
    ggtitle(paste(contrast2, collapse = " vs ")) +
    # Add point names
    #geom_text(aes(label=ifelse(abs(FC)> n, names,'')), nudge_x = 1, nudge_y = 1)
    geom_text_repel(data = top_n(res_VP, n = np, wt = abs(res_VP[, Fold])),
                    mapping = aes(label = names),
                    fontface = 'bold',color = 'black',
                    box.padding = unit(0.5, "lines"), point.padding = unit(0.5, "lines"))

  return(g)
}
