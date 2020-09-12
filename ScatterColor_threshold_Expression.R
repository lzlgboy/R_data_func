fn_exp <- function(dfr, expr_txt) 
{        
  eval(parse(text = expr_txt), dfr) 
}


ScatterColor <- function(df, x='X', y = 'Y', xy_lab= c("X","Y"), exp_threshold = c('X > 1 & Y > 1'), colors_threshold = c('red'),pt_size= 3) {
  
  require(ggplot2)
  
  df.plot <- df
  
  df.plot$Sig <- "N"
  for (i in seq (1,length(exp_threshold))) {
    df.plot$Sig[fn_exp(df.plot, exp_threshold[i])] <- exp_threshold[i]
  }
  
  exp_threshold <- c(exp_threshold,'N')
  colors <- c(colors_threshold,'grey')
  
  ggplot(df.plot,aes(x=df.plot[,x],y=df.plot[,y])) +
    geom_point(aes(color=Sig),size=pt_size) + 
    scale_color_manual(values = colors,breaks = exp_threshold) +

    theme_bw()
}
