MakeScatterDESeq <- function(df, x=df$baseMean_x, y = df$baseMean_y, xy_lab= c("X","Y"),flag_logged = FALSE,flag_anno = FALSE, col_anno = c(), FC_threshold = 1.5, padj_threshold=0.05) {

require(ggplot2)

df.plot <- as.data.frame(cbind(as.data.frame(x),as.data.frame(y)))

df.plot$padj <- df$padj	


df.plot$log2FoldChange <- log((df.plot[,2]+0.01)/(df.plot[,1]+0.01),2) # Y/X

fc_log2_threshold <- log(FC_threshold,2)


df.plot$Change <- "NoChange"
df.plot$Change[df$padj <= padj_threshold & df.plot$log2FoldChange > fc_log2_threshold] <- "Up" 
df.plot$Change[df$padj <= padj_threshold & df.plot$log2FoldChange < -fc_log2_threshold] <- "Down" 

count.Up <- length(which(df.plot$Change == "Up"))
count.Down <- length(which(df.plot$Change == "Down"))
count.NoChange <- length(which(df.plot$Change == "NoChange"))


df$Change <- df.plot$Change
df.ordered.by.FC <- df[order(df$log2FoldChange),]
df.for.list <- df.ordered.by.FC[,c("Chr","Start","End","PeakID","log2FoldChange")]
	
list.Up        <- df.for.list[which(df.ordered.by.FC$Change == "Up"),]
list.Down      <- df.for.list[which(df.ordered.by.FC$Change == "Down"),]
list.Change   <- rbind(list.Up,list.Down)

list.NoChange <- df.for.list[which(df.ordered.by.FC$Change == "NoChange"),]
	
list.Change   <- list.Change[rev(order(list.Change$log2FoldChange)),]
list.Up   <- list.Up[rev(order(list.Up$log2FoldChange)),]
list.Down   <- list.Down[order(list.Down$log2FoldChange),]
	
list.NoChange   <- list.NoChange[rev(order(list.NoChange$log2FoldChange)),]

	
fileName.list.Up   <- paste('./',xy_lab[2],'.vs.',xy_lab[1],'.padj.',padj_threshold,'.FC.',FC_threshold,'.Up.order.by.log2FC.txt',sep="")
fileName.list.Down <- paste('./',xy_lab[2],'.vs.',xy_lab[1],'.padj.',padj_threshold,'.FC.',FC_threshold,'.Down.order.by.log2FC.txt',sep="")
fileName.list.Change <- paste('./',xy_lab[2],'.vs.',xy_lab[1],'.padj.',padj_threshold,'.FC.',FC_threshold,'.UpDown.order.by.log2FC.txt',sep="")
fileName.list.NoChange <- paste('./',xy_lab[2],'.vs.',xy_lab[1],'.padj.',padj_threshold,'.FC.',FC_threshold,'.NoChange.order.by.log2FC.txt',sep="")

write.table(list.Up,file=fileName.list.Up,quote=F,row.names=F,col.names=F,sep="\t")
write.table(list.Down,file=fileName.list.Down,quote=F,row.names=F,col.names=F,sep="\t")
write.table(list.Change,file=fileName.list.Change,quote=F,row.names=F,col.names=F,sep="\t")
write.table(list.NoChange,file=fileName.list.NoChange,quote=F,row.names=F,col.names=F,sep="\t")


lab.Up <- paste("Up: ",count.Up)
lab.Down <- paste("Down: ",count.Down)
lab.NoChange <- paste("NoChange: ",count.NoChange)
#lab.NoChange <- paste("NoChange")

	
color_DEG <- c("blue", "azure4","red")
lab_DEG <- c(lab.Down,lab.NoChange,lab.Up)
	
if (count.Up == 0) {
	color_DEG <- c("blue", "azure4")
	lab_DEG <- c(lab.Down,lab.NoChange)
}

if (count.Down == 0) {
	color_DEG <- c("azure4","red")
	lab_DEG <- c(lab.NoChange,lab.Up)
}

if (count.Down == 0 & count.Up == 0 ) {
	color_DEG <- c("azure4")
	lab_DEG <- c(lab.NoChange)
}

legend_title <- paste("FC: ",FC_threshold,"\n","Padj: ",padj_threshold,sep="")

ggplot(df.plot,aes(x=log(df.plot[,1]+1,2),y=log(df.plot[,2]+1,2))) +
    geom_point(aes(color=Change),size=0.5) +
	
	theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_color_manual(values = color_DEG,labels = lab_DEG) +
	labs(colour=legend_title) + 

	xlab(xy_lab[1]) +
	ylab(xy_lab[2]) +
	guides(colour = guide_legend(override.aes = list(size=5)))
}
