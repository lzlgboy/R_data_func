removeConstantCol <- function (df=data.frame) {
    df.rm.noVar.col <- df[,apply(df,2,var) != 0]
    return(df.rm.noVar.col)
}

