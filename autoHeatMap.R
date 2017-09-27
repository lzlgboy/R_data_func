extractCluster <- function(df = ,do_cluster = F, color = NULL,title="Heatmap",clustered_Matrix = NULL) {
    require(pheatmap)
    pheatmap(df,cluster_cols=F,cluster_rows = F,show_rownames =F)
    if (do_cluster) {

    }
    if (clustered_Matrix) {
        return(df)
    }
}
