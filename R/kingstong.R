#' @export
kingstong<-function(VARLAB="Q1NB", DFS=list(pan1data, pan2data, pan3data),
                    ORDERING=nombri,
                    LEN.WRAP=70,
                    TITLE.SIZE=12,
                    ENQUETE.SIZE=4,
                    LEGEND.SIZE=2,
                    TEXT.SIZE=4, 
                    question.lab=NULL, 
                    IFLEGEND=FALSE, MARGES.DU.PLOT=c(5, 5, 5, 5),
                    WIDTHCAT=list("big"="3", "small"=c("1", "2")),
                    LEGEND.POSITION="top",
                    add.mean=TRUE, ...){
  ####
  library(ggplot2)
  funkyquest(varlab = VARLAB, dfs=DFS)->trap
  blanklab<-""
  if(is.null(question.lab)){
  question.lab<-sub('.*\\.', '',
                    struct.o.que[grepl(pattern = VARLAB,  gsub(".", "", x=struct.o.que$name, fixed = TRUE), ignore.case = TRUE)==TRUE , "text"]
  )
  }
  ####
  table(trap)->tab.eff
  round(prop.table(tab.eff, margin = 1)*100, 2)->tab.prop
  ####
  p<-fonkytonk3(Funkyquest=trap, QUESTION=question.lab, ordering = ORDERING,
                len.wrap=LEN.WRAP,
                Title.Size=TITLE.SIZE,
                Enquete.size=ENQUETE.SIZE,
                legend.size=LEGEND.SIZE,
                text.size=TEXT.SIZE, iflegend=IFLEGEND, marges.du.plot=MARGES.DU.PLOT,
                widthcat=WIDTHCAT, leg.pos=LEGEND.POSITION, ADD.MEAN=add.mean)
  ####
  list("tab.eff"=tab.eff, "tab.prop"=tab.prop, "p"=p)->res
  return(res)
}
