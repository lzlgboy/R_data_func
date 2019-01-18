# Calculate number of cells per cluster from object@ident
cell.num <- table(object@ident)

# Add cell number per cluster to cluster labels
ClusterLabels = paste("Cluster", names(cell.num), paste0("(n = ", cell.num, ")"))

# Order legend labels in plot in the same order as 'ClusterLabels'
ClusterBreaks = names(cell.num)

# Plot tSNE with new legend labels for clusters
TSNEPlot(object = object, do.return = T) +
  scale_colour_discrete(breaks = ClusterBreaks, 
                        labels = ClusterLabels) +
  labs(x = "t-SNE 1",
       y = "t-SNE 2")
