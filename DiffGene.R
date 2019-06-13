DiffGene <- function(df, x=df$baseMean_x, y = df$baseMean_y ,FC_threshold = 2, padj_threshold=0.05, UpOrDown = "Up") {
  
  df.plot <- as.data.frame(cbind(as.data.frame(x),as.data.frame(y)))
  
  
  df.plot$padj <- df$padj
  df.plot$log2FoldChange <- log((df.plot[,2]+0.1)/(df.plot[,1]+0.1),2) # Y/X
  
  fc_log2_threshold <- log(FC_threshold,2)
  
  
  df.plot$Change <- "NoChange"
  df.plot$Change[df$padj <= padj_threshold & df.plot$log2FoldChange > fc_log2_threshold] <- "Up" 
  df.plot$Change[df$padj <= padj_threshold & df.plot$log2FoldChange < -fc_log2_threshold] <- "Down" 
  
  return(df$id[df.plot$Change == UpOrDown])
  
}
