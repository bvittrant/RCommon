###############################################################################
# Line function
###############################################################################
RC_line = function(Data_melt, Name_Project, choice1, choice2, choice3){
  # choice1, a value from the column variable from data_melt (e.g. a gene, a proteine etc)
  # choice2 & choice3, any column from your design file that was used to create the data_melt
  tmp = ggplot(Data_melt[Data_melt$variable == choice1,],aes(y = value)) +
    geom_line(aes_string(x = choice3, group = choice2 , color = choice2)) +
    geom_point(aes_string(x = choice3, color = choice2)) +
    ggtitle(Name_Project) + xlab(choice3) + ylab("")
  return(tmp)
}
