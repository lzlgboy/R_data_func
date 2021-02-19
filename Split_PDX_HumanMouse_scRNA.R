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
            scale_color_manual(values=c("grey", "red", "blue")) +
            theme_bw() + 
            xlim(c(0,max(df$Human)+5)) +
            ylim(c(0,max(df$Mouse)+5))
    
    Human_Count <- RawCount[grep("^GRCh38_",  rownames(RawCount)),which( df$Mouse/(df$Mouse+df$Human) < pct_threshold)]
    Mouse_Count <- RawCount[grep("^mm10__",  rownames(RawCount)),which( df$Human/(df$Mouse+df$Human) < pct_threshold)]
    
    rownames(Mouse_Count) <- gsub("mm10___","",rownames(Mouse_Count))
    rownames(Human_Count) <- gsub("GRCh38_","",rownames(Human_Count))
    
    numMouseCell <- length(Mouse_Count[1,])
    numHumanCell <- length(Human_Count[1,])
    numDoublet   <- length(df$Human) - numMouseCell - numHumanCell

    returnList <- list("HumanCount" = Human_Count, "MouseCount" = Mouse_Count, "plot"=plt,"NumDoublet"=numDoublet,"NumMouseCell"=numMouseCell,"NumHumanCell"=numHumanCell)
    
    return(returnList)
}
