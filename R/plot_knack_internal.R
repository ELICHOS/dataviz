#' plot.knack.internal
#'
#' @param kn Aknat
#' @param SEQ.CUT
#' @param col.neg col.neg
#' @param col.pos col.pos
#' @param col.na col.na
#' @param TITLE
#' @return a plot
#' @export
plot.knack.internal<-function(kn=k, SEQ.CUT=seq.cut, 
                              col.neg = pickobrew(the.palette = "Reds", the.indexes = 8), 
                              col.pos = pickobrew(the.palette = "Greens", 8),
                              col.na = pickobrew("Greys", 2), 
                              #mod.split="split",#"facet"
                              TITLE="",FACET.VARS = facet.var,
                              ... ){
  p<-ggplot(data=kn, aes(CLASS, VARIABLE))+
  geom_raster(aes(fill = cut(VALUE, breaks = SEQ.CUT), alpha=VALUE))+
  #geom_text(aes(label=round(VALUE, 1)), fontface="italic", colour=gray(0.3))+
  labs(title ="Heat Map", x = "Outlet Identifier", y = "Item Type")+
  scale_fill_manual(values = c(col.neg, col.pos), 
                    na.value=col.na, 
                    labels=wrap.it(c("Valeurs significativement inférieures (risque d'erreur max: 5%)", "Valeurs significativement supérieures (risque d'erreur max: 5%)", "Pas d'association significative"), 
                                   len=40),
                    name=wrap.it("Sens de l'association entre variables et classes        :",
                                 80))+#, guide_legend(ncol=3))+
scale_alpha(range = c(0.5, 1), guide = FALSE)
if(length(FACET.VARS)>=1&sum(FACET.VARS%in%names(kn))==length(FACET.VARS)){#&length(unique(kn$GROUP))>1){
  if(length(FACET.VARS)==1){
  #kn$GROUP<-as.character(kn$GROUP)
  p<-p+
    facet_grid(reformulate(FACET.VARS,  '.') , scales = "free_y")
  } else {
    p<-p+
      facet_grid(reformulate(FACET.VARS[2], FACET.VARS[1]) )#, scales = "free_y")
  }
} else {p<-p }

p<-p+
  theme_light(
    #base_family = "Calibri Light"
    )+
  labs(title = TITLE, x="Classes", y="Variables")+
  #coord_flip()+
  theme(axis.title = element_blank(),
        #axis.text.y = element_text(colour = NEWCOL[c(1, 2, 4, 3, 6, 5, 7)], face = "bold", size = 11),
        legend.position="top")+
  geom_vline(xintercept = c(0, seq(1.5, 6.5, 1)), color=gray(0.95))+
  geom_hline(yintercept = c(0, seq(1.5, 17.5, 1)), color=gray(0.95))

return(p)
}
