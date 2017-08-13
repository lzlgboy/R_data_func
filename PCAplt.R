PCAplt <- function(df = data.frame,pc=c(1,2),groupVector=NULL,sampleNameVector=NULL,selectNameVector=NULL,colorVector = NULL,title="PCA \n Plot",shapeSize=3,alphaShape=0.7,fontSzie=3) {
    require(ggplot2)
    require(ggrepel)
    #data <- t(apply(df,1,scale)) # scale the data
    #data <- df

    result.PCA <- prcomp(t(df),scale. = T)

    #percent.PCA <- round((((result.PCA$sdev)^2 / sum(result.PCA$sdev^2))*100)[1:length(result.PCA$sdev)])
    percent.PCA <- round((((result.PCA$sdev)^2 / sum(result.PCA$sdev^2))*100))
    loading <- result.PCA$rotation
    scores.PCA <- result.PCA$x

    x.PCA <- scores.PCA[,pc[1]]
    y.PCA <- scores.PCA[,pc[2]]

    df.for.plot <- as.data.frame(cbind(x.PCA,y.PCA))

    p <-ggplot(df.for.plot, aes(x.PCA,y.PCA)) +
        theme_bw() +
        xlab(paste("PC",pc[1]," (", percent.PCA[pc[1]], "%)", sep = "")) +
        ylab(paste("PC",pc[2]," (", percent.PCA[pc[2]], "%)", sep = "")) +
        ggtitle(title) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(plot.title = element_text(lineheight= .8, face = "bold")) +
        theme(plot.title = element_text(hjust = .5)) 

    #if (!is.null(colorVector)) {
    #    p <- p + scale_fill_manual(values = colorVector)
    #}

    if (is.null(groupVector)) {
        p <- p + geom_point(size = shapeSize, alpha = alphaShape ) 
        if (!is.null(sampleNameVector)) {
            if (!is.null(selectNameVector)) {
                in.list <- sampleNameVector %in% selectNameVector
                sampleNameVector[!in.list] <- ""
            }
            p <- p + geom_text_repel(aes(label = sampleNameVector),box.padding=unit(0.5,"lines"),size = fontSzie)
        }
        p
    }else{
        p1 <- p + geom_point(aes(colour=groupVector),size = shapeSize, alpha = alphaShape) +
        theme(legend.title=element_blank())
        if (!is.null(sampleNameVector)) {
            #label.list <-rep("",length(sampleNameVector))
            if (!is.null(selectNameVector)) {
                in.list <- sampleNameVector %in% selectNameVector
                sampleNameVector[!in.list] <- ""
            }
            p1 <- p1 + geom_text_repel(aes(label = sampleNameVector),box.padding=unit(0.5,"lines"),size = fontSzie)
        }
        if (!is.null(colorVector)) {
            p1 <- p1 + scale_colour_manual(values = colorVector)
        }
        p1
    }
}
