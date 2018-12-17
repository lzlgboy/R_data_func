
MakeScatterDESeq <- function(df, x=df$baseMean_x, y = df$baseMean_y, xy_lab= c("X","Y"), anno_FC_threshold = 20, anno_padj_threshold = 0.01, col_anno = c(), FC_threshold = 2, padj_threshold=0.05) {

    
require(ggplot2)
require(ggrepel)

color_pal_1 <- c(
		 "#009D9A","#00A299","#00A698","#00AB96","#00B095","#00B492","#00B890","#00BC8D", 
		 "#00C089","#0AC486","#31C881","#47CB7D","#59CF78","#69D273","#77D46E","#85D768", 
		 "#93DA62","#A0DC5C","#ADDE56","#B9E050","#C5E14A","#D1E245","#DDE340","#E8E43C", 
		 "#F3E438","#FDE333")
	
df.plot <- as.data.frame(cbind(as.data.frame(x),as.data.frame(y)))


df.plot$padj <- df$padj
df.plot$log2FoldChange <- log((df.plot[,2]+0.1)/(df.plot[,1]+0.1),2) # Y/X

fc_log2_threshold <- log(FC_threshold,2)

df.plot$Row.names <- df$Row.names

df.plot$Change <- "NoChange"
df.plot$Change[df$padj <= padj_threshold & df.plot$log2FoldChange > fc_log2_threshold] <- "Up" 
df.plot$Change[df$padj <= padj_threshold & df.plot$log2FoldChange < -fc_log2_threshold] <- "Down" 


count.Up <- length(which(df.plot$Change == "Up"))
count.Down <- length(which(df.plot$Change == "Down"))
count.NoChange <- length(which(df.plot$Change == "NoChange"))


df$Change <- df.plot$Change
df$log2FoldChange <- df.plot$log2FoldChange
df.ordered.by.FC <- df[order(df$log2FoldChange),]
df.for.list <- df.ordered.by.FC[,c("Row.names","pval","padj","log2FoldChange")]
	
list.Up        <- df.for.list[which(df.ordered.by.FC$Change == "Up"),]
list.Down      <- df.for.list[which(df.ordered.by.FC$Change == "Down"),]
list.Change   <- rbind(list.Up,list.Down)
	
list.Change   <- list.Change[order(list.Change$log2FoldChange),]
list.Up   <- list.Up[rev(order(list.Up$log2FoldChange)),]
list.Down   <- list.Down[order(list.Down$log2FoldChange),]
	
fileName.list.Up   <- paste('./',xy_lab[2],'.vs.',xy_lab[1],'.padj.',padj_threshold,'.FC.',FC_threshold,'.Up.order.by.log2FC.txt',sep="")
fileName.list.Down <- paste('./',xy_lab[2],'.vs.',xy_lab[1],'.padj.',padj_threshold,'.FC.',FC_threshold,'.Down.order.by.log2FC.txt',sep="")
fileName.list.Change <- paste('./',xy_lab[2],'.vs.',xy_lab[1],'.padj.',padj_threshold,'.FC.',FC_threshold,'.UpDown.order.by.log2FC.txt',sep="")

write.table(list.Up,file=fileName.list.Up,quote=F,row.names=F,col.names=F,sep="\t")
write.table(list.Down,file=fileName.list.Down,quote=F,row.names=F,col.names=F,sep="\t")
write.table(list.Change,file=fileName.list.Change,quote=F,row.names=F,col.names=F,sep="\t")

lab.Up <- paste("Up: ",count.Up)
lab.Down <- paste("Down: ",count.Down)
lab.NoChange <- paste("NoChange: ",count.NoChange)
#lab.NoChange <- paste("NoChange")


legend_title <- paste("FC: ",FC_threshold,"\n","Padj: ",padj_threshold,sep="")

# for plot setting
df.plot$Alpha[df.plot$Change == "NoChange"] <- 0 

df.plot$pointSize <- 0.01
#df.plot$pointSize[df.plot$Change == "NoChange"] <- 0.01
#df.plot$pointSize[df.plot$Change != "NoChange" & -log10(df.plot$padj) < 10] <- 0.1
#df.plot$pointSize[df.plot$Change != "NoChange" & -log10(df.plot$padj) >= 10 & -log10(df.plot$padj) < 50] <- 0.3
#df.plot$pointSize[df.plot$Change != "NoChange" & -log10(df.plot$padj) >= 50 & -log10(df.plot$padj) < 100] <- 0.5
#df.plot$pointSize[df.plot$Change != "NoChange" & -log10(df.plot$padj) >= 100 & -log10(df.plot$padj) < 200] <- 1
#df.plot$pointSize[df.plot$Change != "NoChange" & -log10(df.plot$padj) >= 200 ] <- 2


#df.plot$padj[df.plot$Change == "NoChange"] <- 1 
df.plot <- df.plot[df.plot$Change != "NoChange",] 
df.plot$Alpha[df.plot$Change == "Up"] <- 0.5 
df.plot$Alpha[df.plot$Change == "Down"] <- 0.5 
df.plot$padj <- df.plot$padj + 1e-300   # so if padj == 0 then -log10 will not have value
# for anno text
#log2_anno_FC_threshold <- log2(anno_FC_threshold)
#df.plot$Row.names[df$padj > anno_padj_threshold || abs(df.plot$log2FoldChange) < anno_FC_threshold] <- ""

#df.plot[order(list.Change$log2FoldChange),]
df.plot <- df.plot[rev(order(df.plot$log2FoldChange)),]
plotNameVector <- df.plot$Row.names
plotNameVector[10:length(plotNameVector)] <- ""

ggplot(df.plot,aes(x=log(df.plot[,1]+1,2),y=log(df.plot[,2]+1,2))) +
        geom_point(aes(alpha=Alpha,color=abs(log2FoldChange),size=-log10(padj))) +
        #geom_point(aes(alpha=Alpha,color=abs(log2FoldChange),size=pointSize)) +
	geom_text_repel(aes(label = plotNameVector),nudge_y = 3,segment.color = 'grey80' ,size = 2,color="black") + 
	
        geom_abline(linetype = "dashed",slope=1) +
        
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_color_gradientn(colours=color_pal_1) +
        labs(colour=legend_title) + 
        
        xlab(xy_lab[1]) +
        ylab(xy_lab[2])
}


