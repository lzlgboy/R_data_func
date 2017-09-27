removeConstantRow <- function (df=data.frame) {
    df.rm.noVar.row <- df[apply(df,1,var) != 0,]
    return(df.rm.noVar.row)
}

