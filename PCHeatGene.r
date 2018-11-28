PCHeatGene <- function(df = data.frame,pc_to_plot=1,top_n_gene = 20,flag_return=FALSE,sampleNameVector=NULL,selectNameVector=NULL,colorVector = NULL,title="Top Gene in PC",fontSzie=3) {
    require(ggplot2)
	require(pheatmap)
	
    #data <- t(apply(df,1,scale)) # scale the data, no need to scale if data is scaled (e.g. rlog etc)
    #data <- df

    #result.PCA <- prcomp(t(df),scale. = T)
    result.PCA <- prcomp(t(df))

    #percent.PCA <- round((((result.PCA$sdev)^2 / sum(result.PCA$sdev^2))*100)[1:length(result.PCA$sdev)])
    percent.PCA <- round((((result.PCA$sdev)^2 / sum(result.PCA$sdev^2))*100))
    loading <- result.PCA$rotation
    scores.PCA <- result.PCA$x
	
	df.Top.gene =df[order(-abs(result.PCA$rotation[,pc_to_plot]))[1:top_n_gene],]

	pheatmap(df.Top.gene,cluster_rows=FALSE,scale="row",main = paste("Top influential in PC", pc_to_plot, "\n",  "n = " ,top_n_gene,sep=""))
	
	if (flag_return){
		return(df.Top.gene)
	}
}
