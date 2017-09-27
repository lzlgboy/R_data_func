LogQuanTrans <- function(df = data.frame,annotation=data.frame,basic=NULL,logBase = 2,flag.Quant=F) {
    df.basic <- df[1:length(basic)]
    df[1:length(basic)] <- list(NULL)
    sampleName <- colnames(df)
    if (flag.Quant){
        df <- normalize.quantiles(as.matrix(df))
    }
    colnames(df) <- sampleName
    df <-log(df+1,logBase)
    df <- cbind(df.basic,df)
    return(df)
}
