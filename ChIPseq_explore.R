library(preprocessCore)
library(pheatmap)
source("~/GitHub_box/R_data_func/boxplt.R")

chroms=paste("chr",c(1:22),sep="")

#peakFileName='annotated.count.K27ac.Mike.size-given.norm.10e7.txt'
#peakFileName='annotated.merged.peaks.ATAC.PC.naiveB.raw.txt'
peakFileName='annotated.merged.peaks.ATAC.PC.naiveB.txt'
annotationFilename='annotation.txt'

basic=c("ID", "chr", "start", "end", "strand", "Peak.Score","Region.size", "Annotation", "Detailed.Annotation", "Distance.to.TSS", "Nearest.PromoterID", "Entrez.ID", "Nearest.Unigene", "Nearest.Refseq", "Nearest.Ensembl", "Annotation.Divergence", "Gene.Alias", "Gene.Description", "Gene.Type")

df = read.table(peakFileName, row.names=NULL, header = T, sep = "\t", comment.char="", quote="\"")
annotation = read.table(annotationFilename, row.names=NULL, header = T, sep = "\t", comment.char="", quote="\"")

colnames(df)[1:length(c(basic,as.character(annotation$short_name)))]=c(basic,as.character(annotation$short_name))

oriNr=nrow(df)
df=df[which(df$chr%in%chroms),]

