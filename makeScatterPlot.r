makeScatterPlot <- function(x = df$col_x, y = df$col_y, xy_lab= c("X","Y"),flag_logged = TRUE,flag_anno = FALSE, col_anno = c(), fc_log_threshold = 2) {
require(ggplot2)
require(ggrepel)
df <- as.data.frame(cbind(x,y))
colnames(df) <- xy_lab
df$logFoldChange <- abs(df[,1]-df[,2])
df$IsChange <- ifelse(df$logFoldChange > fc_log_threshold, "change","not change")
ggplot(df,aes(x=df[,1],y=df[,2])) +
geom_point(aes(color=IsChange),size=0.5) +
scale_color_manual(values = c("#d61d2f", "black")) +
theme_bw() +
theme(legend.position="none") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
xlab(xy_lab[1]) +
ylab(xy_lab[2])
}
