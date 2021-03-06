makeScatterPlot <- function(df, x=df$col_x, y = df$col_y, xy_lab= c("X","Y"),flag_logged = FALSE,flag_anno = FALSE, col_anno = c(), FC_threshold = 1.5, padj_threshold=0.05) {

require(ggplot2)

df.plot <- as.data.frame(cbind(x,y))
df.plot$padj <- df$padj
df.plot$log2FoldChange <- log((df.plot[,2]+1)/(df.plot[,1]+1),2) # Y/X

fc_log2_threshold <- log(FC_threshold,2)


df.plot$Change <- "NoChange"
df.plot$Change[df$padj <= padj_threshold & df.plot$log2FoldChange > fc_log2_threshold] <- "Up" 
df.plot$Change[df$padj <= padj_threshold & df.plot$log2FoldChange < -fc_log2_threshold] <- "Down" 

count.Up <- length(which(df.plot$Change == "Up"))
count.Down <- length(which(df.plot$Change == "Down"))
count.NoChange <- length(which(df.plot$Change == "NoChange"))

lab.Up <- paste("Up: ",count.Up)
lab.Down <- paste("Down: ",count.Down)
lab.NoChange <- paste("Up: ",count.NoChange)

legend_title <- paste("FC: ",FC_threshold,"\n","Padj: ",padj_threshold,sep="")

ggplot(df.plot,aes(x=log(df.plot[,1]+1,2),y=log(df.plot[,2]+1,2))) +
    geom_point(aes(color=Change),size=0.5) +
	
	theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_color_manual(values = c("blue", "azure4","red"),labels = c(lab.Down,lab.NoChange,lab.Up)) +
	labs(colour=legend_title) + 

	xlab(xy_lab[1]) +
	ylab(xy_lab[2]) +
	guides(colour = guide_legend(override.aes = list(size=5)))
}
