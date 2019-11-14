###############################################################################
# PLSDA function
###############################################################################
RC_plsda_loading = function(Data, Design, Name_Project, comp, ColorVar, CorrectVar = NULL, ...){
  # ColorVar & CorrectVar, any column from your design file
  # If only ColorVar is provided then it's a single level PLS-DA
  # If CorrectVar is provided it's a pls-da on ColorVar corrected for CorrectVar
  # comp is the component you want to plot

  tmp1 = ColorVar
  tmp2 = CorrectVar
  comp = comp

  # PLSDA
  if(is.null(tmp2)) {
    # Simple level PLSDA mixOmics
    plsda = plsda(Data, Design[,tmp1], ncomp = 10, ...)
    p = plotLoadings(plsda , comp = comp,
                     contrib = 'max',
                     method = 'mean',
                     title = paste("Simple PLSDA - Comp ", comp, sep="")
                     )
    return(p)
  }

  if(!is.null(tmp2)){
    # Multilevel PLSDA mixOmics
    plsda = plsda(Data, Design[,tmp1], ncomp = 10, multilevel = Design[,tmp2], ...)
    p = plotLoadings(plsda, comp = comp,
                     contrib = 'max',
                     method = 'mean',
                     title = paste("Multilevel PLSDA - Comp ", comp, sep="")
                     )
    return(p)
  }
}
