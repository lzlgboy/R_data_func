# https://github.com/satijalab/seurat/issues/235
# First, the usual command to make the feature plots and return the ggplot2 objects
fp <- Seurat::FeaturePlot(object=p, features.plot=GeneList, cols.use = c("gray75", "red"), 
                          min.cutoff="q25",max.cutoff="q75", reduction.use = "tsne", 
                          do.return=TRUE)

# Here, fp is a list of objects, one per feature plot

# Then, re-order the "data" dataframe in increasing order of gene expressions
for (i in 1:length(fp)){
  fp[[i]]$data <- fp[[i]]$data[order(fp[[i]]$data$gene),]
}
