extractCluster <- function(res.pheatmap,df = data.frame,outputFile = NULL) {
    res.pheatmap.clustered <- df[res.pheatmap$tree_row$order,]
    res.pheatmap.clustered <- res.pheatmap.clustered[,res.pheatmap$tree_col$order]
    if (! is.null(outputFile)) {
        write.table(res.pheatmap.clustered, quote = F, row.names = T, col.names = T, file = paste("./",outputFile), sep = "\t")
    }else{
    return(res.pheatmap.clustered)
    }
}
