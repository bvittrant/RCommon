###############################################################################
# DE summary
###############################################################################
RC_DESummary = function(res, FC, r_FC, padj, r_pval){
  # We create an empty dataframe
  df_sum = matrix(nrow = length(r_FC), ncol = length(r_pval), NA) %>% as.data.frame
  # Row iteration initializing
  ci = 0
  for(i in r_FC){
    ci = ci + 1
    # Col iteration initialzing
    cj = 1
    for(j in r_pval){
      # Find the number of DE features in Res with respect to the iterating values
      nDE = nrow(res[ abs(res[,FC]) > i & res[,padj] < j, ])
      # Save the value
      df_sum[ci,cj] = nDE
      cj = cj + 1
    }
  }

  colnames(df_sum) = paste("Stat",r_pval,sep="_")
  rownames(df_sum) = paste("Change",r_FC,sep="_")

  # return the DF
  return(df_sum)

}
