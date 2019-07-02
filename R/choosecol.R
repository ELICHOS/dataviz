choosecol<-function(namecol){
  list(
  "nega.enqconj"=rgb(218, 0, 0, maxColorValue = 255),
"nega.mrie"=rgb(196, 89, 17, maxColorValue = 255), #brewer.pal(n = 9, name = "Reds")[5]
"posi.enqconj"=rgb(142,193,75, maxColorValue = 255),#brewer.pal(n = 9, name = "Greens")[6]
"posi.mrie"=rgb(0,109,44, maxColorValue = 255),#brewer.pal(n = 9, name = "Greens")[6]
"stab.enqconj"=rgb(238, 234, 197, maxColorValue = 255),#brewer.pal(n = 8, name = "Set2")[6]
"nesp.mrie"= rgb(52, 119, 128, maxColorValue = 255),#rgb(173, 158, 157, maxColorValue = 255),#rgb(52, 119, 128, maxColorValue = 255)#brewer.pal(n = 9, name = "Blues")[6]
"nesp.enqconj"=gray(level = 0.2),
"unkn.enqconj"=gray(level = 0.5)
)->listcol
  if(length(namecol)==1&"all"%in%namecol){
    data.frame(t(data.frame(listcol)))->df
    df$expr<-c(
      "rgb(218, 0, 0, maxColorValue = 255)",
     "rgb(196, 89, 17, maxColorValue = 255)", #brewer.pal(n = 9, name = "Reds")[5]
     "rgb(142,193,75, maxColorValue = 255)",#brewer.pal(n = 9, name = "Greens")[6]
     " rgb(0,109,44, maxColorValue = 255)",#brewer.pal(n = 9, name = "Greens")[6]
      "rgb(238, 234, 197, maxColorValue = 255)",#brewer.pal(n = 8, name = "Set2")[6]
      "rgb(52, 119, 128, maxColorValue = 255)",#rgb(52, 119, 128, maxColorValue = 255)#brewer.pal(n = 9, name = "Blues")[6]
      "gray(level = 0.2)",
      "gray(level = 0.5)"
    )
    names(df)<-c("codeHEXA", "EXPR")
    res<-df} else {
  listcol[namecol]->res
      unlist(res, use.names=FALSE)->res
  }
  return(res)
}
