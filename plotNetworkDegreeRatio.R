plotDegreeRatio <- function(df = data.frame,geneNameVector=NULL,selectNameVector=NULL,title="",shapeSize=1,alphaShape=0.7,fontSzie=3) {
    require(ggplot2)
    require(ggrepel)

    NameVector <- as.character(geneNameVector)
    in.list <- NameVector %in% selectNameVector
    plotNameVector <- NameVector
    plotNameVector[!in.list] <- ""
    ggplot(df, aes(Rank,Ratio,color=Label)) +
        theme_bw() +
        xlab("Rank") +
        ylab("Relative change in number\nof connections between\nMM and PC (log2)") +
        ggtitle(title) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(plot.title = element_text(lineheight= .8, face = "bold")) +
        theme(plot.title = element_text(hjust = .5)) +
        geom_point(size = shapeSize, alpha = alphaShape ) +
        scale_colour_manual(values=c("#d25f00","#808080","#006eac")) +
        #geom_text_repel(aes(label = plotNameVector),box.padding=unit(0.3,"lines"),size = fontSzie) +
        geom_text_repel(aes(label = plotNameVector),nudge_y = 3,segment.color = 'grey80' ,size = fontSzie,color="black") +
        xlim(c(0,23000)) +
        geom_hline(yintercept=c(-1,1),linetype="dotted")

}
