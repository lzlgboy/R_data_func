VolcanoPlot <- function (toptable, lab, x, y, pCutUse, auto_Title = TRUE,selectLab = NULL, xlim = c(min(toptable[, 
  x], na.rm = TRUE)-0.1, max(toptable[, x], na.rm = TRUE)+0.1), ylim = c(-0.1, 
  max(-log10(toptable[, y]), na.rm = TRUE) + 0.1), xlab = bquote(~Log[2] ~ 
  "fold change"), ylab = bquote(~-Log[10] ~ italic(P)), axisLabSize = 16, 
  pCutoff = 0.05, pLabellingCutoff = 0.05, FCcutoff = 2, 
  title = "", titleLabSize = 16, transcriptPointSize = 0.8, 
  transcriptLabSize = 3, col = c("black", "black", 
    "black", "red2","blue2"), colOverride = NULL, colAlpha = 1/2, 
  legend = c("NS", "FC", "P", "FC_P_UP","FC_P_DOWN"), legendPosition = "top", 
  legendLabSize = 10, legendIconSize = 3, DrawConnectors = FALSE, 
  widthConnectors = 0.5, colConnectors = "black", cutoffLineType = "longdash", 
  cutoffLineCol = "black", cutoffLineWidth = 0.4, gridlines.major = TRUE, 
  gridlines.minor = TRUE, border = "partial", borderWidth = 0.8, 
  borderColour = "black") 
{
  require(ggplot2)
  if (!requireNamespace("ggplot2")) {
    stop("Please install ggplot2 first.", call. = FALSE)
  }
  if (!requireNamespace("ggrepel")) {
    stop("Please install ggrepel first.", call. = FALSE)
  }
  if (!is.numeric(toptable[, x])) {
    stop(paste(x, " is not numeric!", sep = ""))
  }
  if (!is.numeric(toptable[, y])) {
    stop(paste(y, " is not numeric!", sep = ""))
  }
  if (!is.numeric(toptable[, pCutUse])) {
    stop(paste(pCutUse, " is not numeric!", sep = ""))
  }
  i <- xvals <- yvals <- Sig <- NULL
  toptable <- as.data.frame(toptable)
  toptable$Sig <- "NS"
  toptable$Sig[(abs(toptable[, x]) > FCcutoff)] <- "FC"
  toptable$Sig[(toptable[, pCutUse] < pCutoff)] <- "P"
  toptable$Sig[(toptable[, pCutUse] < pCutoff) & (toptable[, x] > FCcutoff)] <- "FC_P_Up"
  toptable$Sig[(toptable[, pCutUse] < pCutoff) & (toptable[, x] < -FCcutoff)] <- "FC_P_Down"
  toptable$Sig <- factor(toptable$Sig, levels = c("NS", "FC", 
    "P", "FC_P_Up","FC_P_Down"))
  
    
  num_NS   <- length(which(toptable$Sig == 'NS'))
  num_FC   <- length(which(toptable$Sig == 'FC'))
  num_P   <- length(which(toptable$Sig == 'P'))  
  num_FC_P_Up   <- length(which(toptable$Sig == 'FC_P_Up'))
  num_FC_P_Down <- length(which(toptable$Sig == 'FC_P_Down')) 
    
  if (min(toptable[, y], na.rm = TRUE) == 0) {
    warning(paste("One or more P values is 0.", "Converting to minimum possible value..."), 
      call. = FALSE)
    toptable[which(toptable[, y] == 0), y] <- .Machine$double.xmin
  }
  toptable$lab <- lab
  toptable$xvals <- toptable[, x]
  # kenian modified to include all data point when zoom In the plot (set small xlim & ylim)
  #========
  toptable$xvals[toptable[, x] > max(xlim)] <- max(xlim)
  toptable$xvals[toptable[, x] < min(xlim)] <- min(xlim)

  toptable$yvals <- toptable[, y]
  toptable$yvals[-log10(toptable[, y]) > max(ylim)] <- 10^(-max(ylim))
  #toptable$yvals[toptable[, y] < min(ylim)] <- min(ylim)
  #========
  if (!is.null(selectLab)) {
    names.new <- rep(NA, length(toptable$lab))
    indices <- which(toptable$lab %in% selectLab)
    names.new[indices] <- as.character(toptable$lab[indices])
    toptable$lab <- names.new
  }
  if (auto_Title) {
      title = paste(title,"    ",'logFC',">",FCcutoff,' ',pCutUse,"<",pCutoff)
  }
  th <- theme_bw(base_size = 24) + theme(legend.background = element_rect(), 
    plot.title = element_text(angle = 0, size = titleLabSize, 
      face = "bold", vjust = 1), axis.text.x = element_text(angle = 0, 
      size = axisLabSize, vjust = 1), axis.text.y = element_text(angle = 0, 
      size = axisLabSize, vjust = 1), axis.title = element_text(size = axisLabSize), 
    legend.position = legendPosition, legend.key = element_blank(), 
    legend.key.size = unit(0.5, "cm"), legend.text = element_text(size = legendLabSize), 
    title = element_text(size = legendLabSize), legend.title = element_blank())
  if (!is.null(colOverride)) {
    plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + 
      th + guides(colour = guide_legend(override.aes = list(size = legendIconSize))) + 
      geom_point(aes(color = factor(names(colOverride))), 
        alpha = colAlpha, size = transcriptPointSize) + 
      scale_color_manual(values = colOverride)
  }
  else {
    plot <- ggplot(toptable, aes(x = xvals, y = -log10(yvals))) + 
      th + guides(colour = guide_legend(override.aes = list(size = legendIconSize))) + 
      geom_point(aes(color = factor(Sig)), alpha = colAlpha, 
        size = transcriptPointSize) + 
      scale_color_manual(values = c(NS = col[1], 
      FC = col[2], P = col[3], FC_P_Up = col[4], FC_P_Down = col[5]), 
      labels = c(NS = paste(legend[1],"\nN=",num_NS), 
                 FC = paste(legend[2], "\nN=",num_FC), 
                 P = paste(legend[3], "\nN=",num_P), 
                 FC_P_Up = paste(legend[4],"\nN=",num_FC_P_Up), 
                 FC_P_Down = paste(legend[5],"\nN=",num_FC_P_Down)))
  }
  plot <- plot + xlab(xlab) + ylab(ylab) + xlim(xlim[1], xlim[2]) + 
    ylim(ylim[1], ylim[2]) + ggtitle(title) + geom_vline(xintercept = c(-FCcutoff, 
    FCcutoff), linetype = cutoffLineType, colour = cutoffLineCol, 
    size = cutoffLineWidth) + geom_hline(yintercept = -log10(pCutoff), 
    linetype = cutoffLineType, colour = cutoffLineCol, size = cutoffLineWidth)
  if (border == "full") {
    plot <- plot + theme(panel.border = element_rect(colour = borderColour, 
      fill = NA, size = borderWidth))
  }
  else if (border == "partial") {
    plot <- plot + theme(axis.line = element_line(size = borderWidth, 
      colour = borderColour), panel.border = element_blank(), 
      panel.background = element_blank())
  }
  else {
    stop("Unrecognised value passed to 'border'. Must be 'full' or 'partial'")
  }
  if (gridlines.major == TRUE) {
    plot <- plot + theme(panel.grid.major = element_line())
  }
  else {
    plot <- plot + theme(panel.grid.major = element_blank())
  }
  if (gridlines.minor == TRUE) {
    plot <- plot + theme(panel.grid.minor = element_line())
  }
  else {
    plot <- plot + theme(panel.grid.minor = element_blank())
  }
  if (DrawConnectors == TRUE && is.null(selectLab)) {
    plot <- plot + geom_text_repel(data = subset(toptable, 
      toptable[, y] < pLabellingCutoff & abs(toptable[, 
        x]) > FCcutoff), aes(label = subset(toptable, 
      toptable[, y] < pLabellingCutoff & abs(toptable[, 
        x]) > FCcutoff)[, "lab"]), size = transcriptLabSize, 
      segment.color = colConnectors, segment.size = widthConnectors, 
      vjust = 1.5)
  }
  else if (DrawConnectors == TRUE && !is.null(selectLab)) {
    plot <- plot + geom_text_repel(data = subset(toptable, 
      !is.na(toptable[, "lab"])), aes(label = subset(toptable, 
      !is.na(toptable[, "lab"]))[, "lab"]), size = transcriptLabSize, 
      segment.color = colConnectors, segment.size = widthConnectors, 
      vjust = 1.5)
  }
  else if (DrawConnectors == FALSE && !is.null(selectLab)) {
    plot <- plot + geom_text(data = subset(toptable, !is.na(toptable[, 
      "lab"])), aes(label = subset(toptable, !is.na(toptable[, 
      "lab"]))[, "lab"]), size = transcriptLabSize, check_overlap = TRUE, 
      vjust = 1.5)
  }
  else if (DrawConnectors == FALSE && is.null(selectLab)) {
    plot <- plot + geom_text(data = subset(toptable, toptable[, 
      y] < pLabellingCutoff & abs(toptable[, x]) > FCcutoff), 
      aes(label = subset(toptable, toptable[, y] < pLabellingCutoff & 
        abs(toptable[, x]) > FCcutoff)[, "lab"]), size = transcriptLabSize, 
      check_overlap = TRUE, vjust = 1.5)
  }
  table_FC_P_Up   <- toptable[which(toptable$Sig == 'FC_P_Up'),]
  table_FC_P_Down <- toptable[which(toptable$Sig == 'FC_P_Down'),] 
  table_FC_P_UpDown <- toptable[which(toptable$Sig == 'FC_P_Up' | toptable$Sig == 'FC_P_Down'),]
    
  newList <- list("plot_volcano" = plot, 
                  "table_FC_P_Up" = table_FC_P_Up,
                  "table_FC_P_Down" = table_FC_P_Down,
                  "table_FC_P_UpDown" = table_FC_P_UpDown
                 )  
  return(newList)
}
