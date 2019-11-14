###############################################################################
# PLSDA function
###############################################################################
RC_plsda = function(Data, Design, Name_Project, ColorVar, CorrectVar = NULL, ...){
  # ColorVar & CorrectVar, any column from your design file
  # If only ColorVar is provided then it's a single level PLS-DA
  # If CorrectVar is provided it's a pls-da on ColorVar corrected for CorrectVar
  tmp1 = ColorVar
  tmp2 = CorrectVar

  # PLSDA
  if(is.null(tmp2)) {
    # Simple level PLSDA mixOmics
    plsda = plsda(Data, Design[,tmp1], ncomp = 10, ...)
    p = plotIndiv(plsda , comp = 1:2,
                  group = Design[,tmp1], ind.names = FALSE,
                  ellipse = TRUE, legend = TRUE, title = paste("Simple PLSDA - ",tmp1, sep=""))
    return(p)
  }

  if(!is.null(tmp2)){
    # Multilevel PLSDA mixOmics
    plsda = plsda(Data, Design[,tmp1], ncomp = 10, multilevel = Design[,tmp2], ...)
    p = plotIndiv(plsda , comp = 1:2,
                  group = Design[,tmp1], ind.names = FALSE,
                  ellipse = TRUE, legend = TRUE,
                  title = paste("Multilevel PLSDA - ",tmp1," vs ", tmp2, sep="")
    )
    return(p)
  }
}
