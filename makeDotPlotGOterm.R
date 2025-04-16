makeDotPlotGOterm <- function(df.goResult, ntop = 15) {  

require(dplyr)
require(ggplot2)

df_top <- df.goResult %>%
arrange(p.adjust) %>%
head(ntop) %>%
arrange(desc(Count))  %>% 
tidyr::separate(GeneRatio,c("count","total"),sep="/") %>% 
mutate(GeneRatio = as.double(count)/as.double(total))

## Order the GO term in a style similar to dotplot.
orderVec <- NULL
for(i in 1:length(df_top$Description)){
   if(df_top$Description[i] %in% orderVec){
      next
    } else {
      orderVec <- c(orderVec,df_top$Description[i])
    }
}
df_top <- df_top %>% transform(Description = factor(Description, levels = rev(orderVec)))


ggplot(df_top, aes(x=GeneRatio, y=Description)) +
    geom_point(aes(size = Count, color = p.adjust)) +
    scale_x_continuous() +  
    scale_colour_gradient(limits=c(0, max(df_top$p.adjust)), low="red",high="blue") +
    theme_bw() #+
#     theme(axis.title = element_blank())

}
