MakeNetwork <- function(db_TF_target = './data/combined_TF_target.txt',
                        db_TF_human  = './data/list_Human_TFs.txt',
                        file_DEG = '',
                        file_important_gene_list = '',
                        flag_TF_only = F,
                        threshold_logFC = 0.2,  
                        threshold_pval = 0.01,  
                        threshold_EdgeNum = 10  
                        ) 
{ 
    # Load packages
    require(dplyr) 
    require(magrittr) 
    require('igraph')

    # Load TF->target relationship database 
    TF_Target_table <- read.table(file = db_TF_target,stringsAsFactors = F)
    colnames(TF_Target_table) <- c("Target","TF")

    # Load TF gene list
    Human_TFs <- read.table(file = db_TF_human,header =T, sep = "\t")
    list_HumanTFs <- as.character(Human_TFs$Symbol)

    # Load Important gene list 
    if (file_important_gene_list != '') {
        important_genes <- read.table(file = file_important_gene_list,stringsAsFactors = F)
        print(file_important_gene_list)
    }

    # Load DEGs for Disease vs. Normal comparison
    if (file_DEG != '') {
        DEG_table <- read.table(file = file_DEG)
    }

    # filter DEGs by FoldChange and Pval
    Disease.vs.Norm_filter_logFC_Pvalue_for_TF_net <-
    DEG_table %>%
    filter((avg_logFC > threshold_logFC | avg_logFC < -threshold_logFC) & p_val < threshold_pval) %>%
    rownames(.)
    
    ### Network construction ###
    #====================================================
    Network_Disease.vs.Norm_for_Cytoscape <- 
    TF_Target_table %>%
    filter( TF  %in% Disease.vs.Norm_filter_logFC_Pvalue_for_TF_net                  # TF or Target is DE Genes
        | Target %in% Disease.vs.Norm_filter_logFC_Pvalue_for_TF_net)  %>%
    filter( TF %in% important_genes$V1 ) %>%
    filter( Target %in% important_genes$V1) %>%
    mutate(Type ='pd') %>%
    dplyr::select(c("TF","Type","Target")) %>%
    arrange(TF,Target)

    ## Count and filter, only TF with > 10 edges kept
    temp_Table_TF_Disease.vs.Norm <- as.data.frame(table(Network_Disease.vs.Norm_for_Cytoscape$TF),stringsAsFactors = F) 
    colnames(temp_Table_TF_Disease.vs.Norm) <- c("TF","Edge")

    Table_TF_filtered_Disease.vs.Norm <- 
    temp_Table_TF_Disease.vs.Norm %>% 
    filter(Edge >= threshold_EdgeNum) %>%
    arrange(desc(Edge))

    # filter Netowrk, keep TFs that has >= 10 edges,

    Network_Disease.vs.Norm_for_Cytoscape_filtered <- 
    Network_Disease.vs.Norm_for_Cytoscape %>%
    filter(TF %in% Table_TF_filtered_Disease.vs.Norm$TF)

    # # Check result netowrk
    # length(unique(Network_Disease.vs.Norm_for_Cytoscape_filtered$TF))
    # unique(Network_Disease.vs.Norm_for_Cytoscape_filtered$TF)

    # length(unique(Network_Disease.vs.Norm_for_Cytoscape_filtered$Target))
    # unique(Network_Disease.vs.Norm_for_Cytoscape_filtered$Target)

    # ================================= #
    # Create Network 
    for_Disease.vs.Norm_edge <- Network_Disease.vs.Norm_for_Cytoscape_filtered[,c('TF','Target')]
    names(for_Disease.vs.Norm_edge) <- c('from','to')

    for_Disease.vs.Norm_node <- as.data.frame(unique(c(Network_Disease.vs.Norm_for_Cytoscape_filtered$TF, Network_Disease.vs.Norm_for_Cytoscape_filtered$Target)),stringsAsFactors = F)
    colnames(for_Disease.vs.Norm_node) <- "id"

    Net_Disease.vs.Norm <- graph_from_data_frame(d = for_Disease.vs.Norm_edge,vertices = for_Disease.vs.Norm_node, directed = T)

    # Net_EC_KLF2_hi.vs.low[[]]

    # Assign color to node 
    temp <- as.data.frame(names(V(Net_Disease.vs.Norm))) 
    colnames(temp) <- 'Node'

    Net_Disease.vs.Norm_table <-
    temp %>%
    left_join(
        DEG_table %>%
        mutate(GeneName=rownames(.)),
        c("Node"="GeneName")) %>%
        mutate(color = "red")

    Net_Disease.vs.Norm_table$color[which(Net_Disease.vs.Norm_table$avg_logFC > 0)] <- "green"


    # Compute node degrees (#links) and use that to set node size:
    deg_EC.Disease.vs.Norm <- degree(Net_Disease.vs.Norm, mode="all")
    # V(Net_EC_TF)$size <- log2(deg + 1)
    V(Net_Disease.vs.Norm)$size <- log(deg_EC.Disease.vs.Norm,base = 1.71828)

    V(Net_Disease.vs.Norm)$color <-Net_Disease.vs.Norm_table$color

    # graph_attr(Net_EC_TF, "layout") <- layout_with_lgl
plot(Net_Disease.vs.Norm,
      layout = layout_with_kk(Net_Disease.vs.Norm,kkconst = 3000*vcount(Net_Disease.vs.Norm)),
     edge.arrow.size=.001, 
    #  edge.arrow.size=.1, 
     edge.curved=.1,
     edge.width = 0.1,
    #  vertex.label.font = 2, # use bold 
     vertex.label.cex = 1.5,  # enlarge the font size
     
#      vertex.size = 3,
     vertex.shape = "circle",
    vertex.label.color="black",
    )
plot_Network <- recordPlot()
plot.new()

### Disease.vs.Norm TF only network

Network_Disease.vs.Norm_for_Cytoscape_filtered_keep_only_TF <-
Network_Disease.vs.Norm_for_Cytoscape_filtered %>% 
filter(Target %in% list_HumanTFs)

Net_Disease.vs.Norm_onlyTF_edge <- Network_Disease.vs.Norm_for_Cytoscape_filtered_keep_only_TF[,c('TF','Target')]
names(Net_Disease.vs.Norm_onlyTF_edge) <- c('from','to')

Net_Disease.vs.Norm_onlyTF_node <- as.data.frame(unique(c(Network_Disease.vs.Norm_for_Cytoscape_filtered_keep_only_TF$TF, Network_Disease.vs.Norm_for_Cytoscape_filtered_keep_only_TF$Target)),stringsAsFactors = F)
colnames(Net_Disease.vs.Norm_onlyTF_node) <- "id"

Net_Disease.vs.Norm__onlyTF <- graph_from_data_frame(d = Net_Disease.vs.Norm_onlyTF_edge,vertices = Net_Disease.vs.Norm_onlyTF_node, directed = T)

# Assign color to node 
temp <- as.data.frame(names(V(Net_Disease.vs.Norm__onlyTF))) 
colnames(temp) <- 'Node'

Net_Disease.vs.Norm_onlyTF_table <-
temp %>%
left_join(
    DEG_table %>%
    mutate(GeneName=rownames(.)),
    c("Node"="GeneName")) %>%
    mutate(color = "red")

Net_Disease.vs.Norm_onlyTF_table$color[which(Net_Disease.vs.Norm_onlyTF_table$avg_logFC > 0)] <- "green"

# Compute node degrees (#links) and use that to set node size:
deg_Disease.vs.Norm_onlyTF <- degree(Net_Disease.vs.Norm__onlyTF, mode="all")
# V(Net_EC_TF)$size <- log2(deg + 1)
V(Net_Disease.vs.Norm__onlyTF)$size <- 2*log(deg_Disease.vs.Norm_onlyTF,base = 1.71828)

V(Net_Disease.vs.Norm__onlyTF)$color <- Net_Disease.vs.Norm_onlyTF_table$color

plot(Net_Disease.vs.Norm__onlyTF,
      layout = layout_with_kk(Net_Disease.vs.Norm__onlyTF,kkconst = vcount(Net_Disease.vs.Norm__onlyTF)),
     edge.arrow.size=0.3, 
     edge.width = 0.5,
     vertex.lable.cex = 1.5,
     vertex.shape = "circle",
    vertex.label.color="black",
    )

plot_Network_TFonly <- recordPlot()
plot.new()

resultList <- list('pltNet'=plot_Network,'pltNet_OnlyTF'=plot_Network_TFonly)

return(resultList)

}
