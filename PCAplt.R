PCAplt <- function(df = data.frame,pc=c(1,2),title="",sizeShape=3) {
    require(ggplot2)
    require(RColorBrewer)
    require(ggrepel)
    data <- t(apply(df,1,scale)) # scale the data
}
