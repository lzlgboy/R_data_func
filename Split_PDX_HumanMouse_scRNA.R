# return  Human Mouse RawCount Data for Seurat

Split_HumanMouse_10X_Count <- function( RawCount ,pct_threshold = 0.05) {
    require(Matrix)
    require(ggplot2)
    
    df_human <- as.data.frame(Matrix::colSums(RawCount[grep("^GRCh38_",  rownames(RawCount)),]))
    df_mouse <- as.data.frame(Matrix::colSums(RawCount[grep("^mm10__",  rownames(RawCount)),]))

    df <- cbind(df_human,df_mouse)
    colnames(df) <- c('Human','Mouse')
    
    # For ggplot split species scatter plot 
    df$Species_ByPct <- 'Mixed'
    df[which( df$Human/(df$Mouse+df$Human) < pct_threshold ),]$Species_ByPct <- 'Mouse'
    df[which( df$Mouse/(df$Mouse+df$Human) < pct_threshold ),]$Species_ByPct <- 'Human'
    
    # Using 10X algorithm
    # ======
    df$Species_By10x <- "Mouse"
    df$Species_By10x[which(df$Human > df$Mouse)] <- 'Human'
    
    qt_10th_mouse <- quantile(df$Mouse[which(df$Species_By10x == 'Mouse')],0.1)
    qt_10th_human <- quantile(df$Human[which(df$Species_By10x == 'Human')],0.1)
    
    df$Species_By10x[which(df$Human >= qt_10th_human & df$Mouse >= qt_10th_mouse)] <- 'Mixed'
    # ======
    
    # Using Both algorithm
    # ======
    # df$Species
    
    plt_scatter_by_Percent <- ggplot(df,aes(x=Human,y=Mouse)) + 
            geom_point(aes(color=Species_ByPct),size=0.5) +
            scale_color_manual(values=c("red", "grey", "blue")) +
            theme_bw() + 
            xlim(c(0,max(df$Human)+5)) +
            ylim(c(0,max(df$Mouse)+5))
    
    plt_scatter_by_10X <- ggplot(df,aes(x=Human,y=Mouse)) + 
            geom_point(aes(color=Species_By10x),size=0.5) +
            scale_color_manual(values=c("red", "grey", "blue")) +
            theme_bw() + 
            xlim(c(0,max(df$Human)+5)) +
            ylim(c(0,max(df$Mouse)+5))
    
    plt_ratio_hist <- ggplot(df,aes(x=Mouse/(Mouse+Human))) + geom_histogram(breaks=seq(0,1,0.0005)) + 
                                        geom_vline(xintercept = pct_threshold,lty=2,col='red') + 
                                        geom_vline(xintercept = (1-pct_threshold),lty=2,col='red') + 
                                        xlab("Fraction of reads from mouse") + 
                                        ylab("Cell Count") +
                                        theme_classic() +
                                        theme(axis.text=element_text(size=16),
                                              axis.title=element_text(size=14,face="bold"))
    # Count by Pct
    Human_Count_byPct <- RawCount[grep("^GRCh38_",  rownames(RawCount)),which( df$Mouse/(df$Mouse+df$Human) < pct_threshold)]
    Mouse_Count_byPct <- RawCount[grep("^mm10__",  rownames(RawCount)),which( df$Human/(df$Mouse+df$Human) < pct_threshold)]

    # Count by10X
    Human_Count_by10X <- RawCount[grep("^GRCh38_",  rownames(RawCount)),which(df$Species_By10x == 'Human')]
    Mouse_Count_by10X <- RawCount[grep("^mm10__",  rownames(RawCount)),which(df$Species_By10x == 'Mouse')]
    
    # add rownames
    rownames(Mouse_Count_byPct) <- gsub("mm10___","",rownames(Mouse_Count_byPct))
    rownames(Human_Count_byPct) <- gsub("GRCh38_","",rownames(Human_Count_byPct))
    
    rownames(Mouse_Count_by10X) <- gsub("mm10___","",rownames(Mouse_Count_by10X))
    rownames(Human_Count_by10X) <- gsub("GRCh38_","",rownames(Human_Count_by10X))
    
    # Doublet Rate By Percent
    numMouseCell_ByPct <- length(Mouse_Count_byPct[1,])
    numHumanCell_ByPct <- length(Human_Count_byPct[1,])
    numDoublet_ByPct   <- length(df$Human) - numMouseCell_ByPct - numHumanCell_ByPct
    doublet_rate_ByPct = numDoublet_ByPct / (numMouseCell_ByPct + numHumanCell_ByPct + numDoublet_ByPct)
    
    # Doublet Rate By 10X
    numMouseCell_By10X <- length(Mouse_Count_by10X[1,])
    numHumanCell_By10X <- length(Human_Count_by10X[1,])
    numDoublet_By10X   <- length(df$Human) - numMouseCell_By10X - numHumanCell_By10X
    doublet_rate_By10X = numDoublet_By10X / (numMouseCell_By10X + numHumanCell_By10X + numDoublet_By10X)
    

    returnList <- list("HumanCountBy10X" = Human_Count_by10X, "MouseCountBy10X" = Mouse_Count_by10X,"HumanCountByPct" = Human_Count_byPct, "MouseCountByPct" = Mouse_Count_byPct, "doubletRateBy10X"=doublet_rate_By10X,"doubletRateByPct"=doublet_rate_ByPct,"pltScatterByPct"=plt_scatter_by_Percent ,"pltScatterBy10X"=plt_scatter_by_10X , "plot_hist"=plt_ratio_hist,"NumDoubletByPct"=numDoublet_ByPct,"NumMouseCellByPct"=numMouseCell_ByPct,"NumHumanCellByPct"=numHumanCell_ByPct,"NumDoubletBy10X"=numDoublet_By10X,"NumMouseCellBy10X"=numMouseCell_By10X,"NumHumanCellBy10X"=numHumanCell_By10X)
    
    return(returnList)
}
