# return  Human Mouse RawCount Data for Seurat

Split_HumanMouse_10X_Count <- function( RawCount ,pct_threshold = 0.05) {
    require(Matrix)
    require(ggplot2)
    

    df_human <- as.data.frame(Matrix::colSums(RawCount[grep("^GRCh38_",  rownames(RawCount)),]))
    df_mouse <- as.data.frame(Matrix::colSums(RawCount[grep("^mm10__",  rownames(RawCount)),]))

    df <- cbind(df_human,df_mouse)
    colnames(df) <- c('Human','Mouse')
    
    # For ggplot split species scatter plot 
    df$Species <- 'Mixed'
    df[which( df$Human/(df$Mouse+df$Human) < pct_threshold ),]$Species <- 'Mouse'
    df[which( df$Mouse/(df$Mouse+df$Human) < pct_threshold ),]$Species <- 'Human'
    
    plt <- ggplot(df,aes(x=Human,y=Mouse)) + 
            geom_point(aes(color=Species),size=0.5) +
            scale_color_manual(values=c("red", "grey", "blue")) +
            theme_bw() + 
            xlim(c(0,max(df$Human)+5)) +
            ylim(c(0,max(df$Mouse)+5))
    
    plt_hist <- ggplot(df,aes(x=Mouse/(Mouse+Human))) + geom_histogram(breaks=seq(0,1,0.0005)) + 
                                        geom_vline(xintercept = pct_threshold,lty=2,col='red') + 
                                        geom_vline(xintercept = (1-pct_threshold),lty=2,col='red') + 
                                        xlab("Fraction of reads from mouse") + 
                                        ylab("Cell Count") +
                                        theme_classic() +
                                        theme(axis.text=element_text(size=16),
                                              axis.title=element_text(size=14,face="bold"))
    
    Human_Count <- RawCount[grep("^GRCh38_",  rownames(RawCount)),which( df$Mouse/(df$Mouse+df$Human) < pct_threshold)]
    Mouse_Count <- RawCount[grep("^mm10__",  rownames(RawCount)),which( df$Human/(df$Mouse+df$Human) < pct_threshold)]
    
    rownames(Mouse_Count) <- gsub("mm10___","",rownames(Mouse_Count))
    rownames(Human_Count) <- gsub("GRCh38_","",rownames(Human_Count))
    
    numMouseCell <- length(Mouse_Count[1,])
    numHumanCell <- length(Human_Count[1,])
    numDoublet   <- length(df$Human) - numMouseCell - numHumanCell
    
    doublet_rate = numDoublet / (numMouseCell + numHumanCell + numDoublet)


    returnList <- list("HumanCount" = Human_Count, "MouseCount" = Mouse_Count, "doubletRate"=doublet_rate,"plot"=plt, "plot_hist"=plt_hist,"NumDoublet"=numDoublet,"NumMouseCell"=numMouseCell,"NumHumanCell"=numHumanCell)
    
    return(returnList)
}
