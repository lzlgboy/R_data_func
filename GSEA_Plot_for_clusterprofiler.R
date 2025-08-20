get_GSEA_anno <- function(gsea_result_obj,gene_set_name){
    anno <- gsea_result_obj[gene_set_name, c("NES", "pvalue", "p.adjust")]
    lab <- paste0(names(anno), "=",  round(anno, 3), collapse="\n")
    return(lab)
}

plotGSEA_with_anno <- function(gsea_result_obj,gene_set_name,fontsize = 10,anno_x = 0.6, anno_y=0.8,anno_color = 'black',anno_font_size=15) {
    tmp_plt <- gseaplot2(gsea_HallMark,geneSetID = gene_set_name,title = gene_set_name,base_size = 10)
    ggdraw(tmp_plt) + draw_text(lab, x = anno_x, y = anno_y, color = anno_color,size = anno_font_size)
}
