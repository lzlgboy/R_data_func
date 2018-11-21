MakeScatterDESeq <- function(df, x=df$baseMean_x, y = df$baseMean_y, xy_lab= c("X","Y"),flag_logged = FALSE,flag_anno = FALSE, col_anno = c(), FC_threshold = 1.5, padj_threshold=0.05) {
    
    require(ggplot2)
    
    df.plot <- as.data.frame(cbind(as.data.frame(x),as.data.frame(y)))
    
    
    df.plot$padj <- df$padj
    df.plot$log2FoldChange <- log((df.plot[,2]+0.1)/(df.plot[,1]+0.1),2) # Y/X
    
    fc_log2_threshold <- log(FC_threshold,2)
    
    
    df.plot$Change <- "NoChange"
    df.plot$Change[df$padj <= padj_threshold & df.plot$log2FoldChange > fc_log2_threshold] <- "Up" 
    df.plot$Change[df$padj <= padj_threshold & df.plot$log2FoldChange < -fc_log2_threshold] <- "Down" 
    
    count.Up <- length(which(df.plot$Change == "Up"))
    count.Down <- length(which(df.plot$Change == "Down"))
    count.NoChange <- length(which(df.plot$Change == "NoChange"))
    
    df.for.list <- df[,c("PeakID","Chr","Start","End")]
    
    list.Up        <- df.for.list[which(df.plot$Change == "Up"),]
    list.Down      <- df.for.list[which(df.plot$Change == "Down"),]
    
    fileName.list.Up   <- paste('./',xy_lab[2],'.vs.',xy_lab[1],'.padj.',padj_threshold,'.FC.',FC_threshold,'.Up.txt',sep="")
    fileName.list.Down <- paste('./',xy_lab[2],'.vs.',xy_lab[1],'.padj.',padj_threshold,'.FC.',FC_threshold,'.Down.txt',sep="")
    
    write.table(list.Up,file=fileName.list.Up,quote=F,row.names=F,col.names=F,sep="\t")
    write.table(list.Down,file=fileName.list.Down,quote=F,row.names=F,col.names=F,sep="\t")
    
    lab.Up <- paste("Up: ",count.Up)
    lab.Down <- paste("Down: ",count.Down)
    lab.NoChange <- paste("NoChange: ",count.NoChange)
    #lab.NoChange <- paste("NoChange")
    
    
    legend_title <- paste("FC: ",FC_threshold,"\n","Padj: ",padj_threshold,sep="")
    
    ggplot(df.plot,aes(x=log(df.plot[,1]+1,2),y=log(df.plot[,2]+1,2))) +
        geom_point(aes(alpha=-log10(padj),color=abs(log2FoldChange),size=-log10(padj))) +
        geom_abline(linetype = "dashed",slope=1) +
        
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_color_gradientn(colours=pal(70)) +
        labs(colour=legend_title) + 
        
        xlab(xy_lab[1]) +
        ylab(xy_lab[2]) }
