plotEnrichBar <-  function(df_enrich, n_top= 15, select_term = NULL ,is_up = TRUE, line_wrap = 80,font_size=16,title=NULL) {
    # Load packages
    require(magrittr)
    require(cowplot)
    require(dplyr)
    
    
    color_up_down = c("#a32a31","#3665a6")  # color for up and down
 
    df <- df_enrich[1:n_top,]      # select top n terms for plot
    if (length(select_term) > 0) {  # select_term = c(1,3,5,...)  to select terms for display
        df <- df[select_term,]        
    }

    
    if (is_up) {
        df <- dplyr::mutate(df,change="up")
        color <- color_up_down[1]
    }else{
        df <- dplyr::mutate(df,change="down")
        color <- color_up_down[2]
    }
    
    df <- dplyr::mutate(df,negLogPvalue=-log10(pvalue))
    df$GeneRatio_float <-  sapply(df$GeneRatio, function(x) eval(parse(text=x))) # turn geneRatio from fraction to float number

    df$Description <- factor(df$Description,                                    # Factor levels in decreasing order
                              levels = unique(df$Description[order(df$pvalue, decreasing = TRUE)]))

    if (is_up) {
        plt <- ggplot(df) +
          geom_col(aes(negLogPvalue, Description), fill = color) +
          geom_shadowtext(
            data = df,
            aes(0, y = Description, label = str_wrap(Description, width=line_wrap)),
              
            hjust = 0,
            nudge_x = 0.3,
            colour = "white",
            bg.colour = color,
            bg.r = 0.2,
            size = 7
              ) + 
           ylab("")
    }else{
        plt <- ggplot(df) +
          geom_col(aes(negLogPvalue, Description), fill = color) +
          geom_shadowtext(
            data = df,
            aes(0.45, y = Description, label = str_wrap(Description, width=line_wrap)),
              
            hjust = 1,  # align right
            nudge_x = 0.3,
            colour = "white",
            bg.colour = color,
            bg.r = 0.2,
            size = 7
              ) +
           ylab("GO Term Enriched")
    }

                                  
    if (is_up) {
        plt <- plt
    }else{
        plt <- plt + 
        scale_x_reverse() 
    }
    plt <- plt + 
        theme(
            plot.title=element_text(family='', face='bold', colour='black', size=26),
#             axis.title.y = element_blank(),
            axis.line.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = c(0.2, 0.8),
            text = element_text(size=font_size),
            axis.text.x = element_text(angle=0, hjust=0),
            axis.text.y=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            plot.margin = unit(c(0, 0, 0, 0), "cm")
        ) +
        theme(legend.position="none") +    
        xlab("-log10(Pvalue)") 

    return (plt)                               
}                                  

# ============= Combine Up and Down Plot ==========================
plotEnrichBar_UpDown <- function(GO_up, GO_down, n_top= 15, select_term = NULL, line_wrap = 80,font_size=16,title=NULL) {
    plt_Up <- plotEnrichBar(GO_up,is_up = TRUE,font_size = font_size, line_wrap = line_wrap)
    plt_Down <- plotEnrichBar(GO_down,is_up = FALSE,font_size = font_size, line_wrap = line_wrap)
    
    title <- ggdraw() + draw_label(title, fontface='bold') 

    p_temp <- cowplot::plot_grid(plt_Down,plt_Up) 
    plt_combined <- cowplot::plot_grid(title,p_temp,ncol =1 ,rel_heights=c(0.1,1) ) # rel_heights values control title margins
    return (plt_combined)
}
