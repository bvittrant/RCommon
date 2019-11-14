###############################################################################
# Boxplot function
###############################################################################
RC_boxplot = function(Data_melt, Name_Project, xVar, colVar, Hline = NULL){
  # choice1, a value from the column variable from data_melt (e.g. a gene, a proteine etc)
  # choice2, any column from your design file that was used to create the data_melt
  #Hline, value to plot on horizontal line
  tmp = ggplot(Data_melt[Data_melt$variable == xVar,], aes(x = variable, y = value)) +
    geom_boxplot(aes_string(col = colVar),position=position_dodge(1) ) +
    ggtitle(Name_Project)+ xlab("") + ylab("")

  if(!is.null(Hline)){
    tmp = tmp +
      geom_hline(yintercept=Hline, linetype="dashed", color = "black")
  }

  return(tmp)
}
