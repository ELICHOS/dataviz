#' theme_MRIE_hc
#' 
#' Thème pré-formaté MRIE pour ggplot. Reprend la structure des thèmes ggplot classiques et les arguments qui vont avec. 
#'
#' @param base_size base text size
#' @param base_family font family
#' @param style
#' @param coord_flip les valeurs numériques sont-elles affichées en abscisse (x)? 
#' @param bgcolor
#' @export
theme_MRIE_hc<-function (base_size = 12, base_family = "sans", style = c("default", 
                                                          "darkunica"), coord_flip=FALSE, bgcolor = NULL) 
{
  library(ggthemes)
  
  if (!is.null(bgcolor)) {
    warning("`bgcolor` is deprecated. Use `style` instead.")
    style <- bgcolor
  }
  style <- match.arg(style)
  bgcolor <- switch(style, default = "#FFFFFF", darkunica = "#2a2a2b")
  if(coord_flip==TRUE){
    panelgrid.major.x=element_line(colour = "#D8D8D8", size=0.8)
    panelgrid.major.y=element_line(colour = add.alpha(col = "#D8D8D8", alpha = 0.5))
    panelgrid.minor.x = element_line(colour = add.alpha(col = "#D8D8D8", alpha = 0.2), size=0.2)
    panelgrid.minor.y = element_line(colour = add.alpha(col = "#D8D8D8", alpha = 0.2), size=0.2)
  } else {
    panelgrid.major.y=element_line(colour = "#D8D8D8", size=0.8)
    panelgrid.major.x=element_line(colour = add.alpha(col = "#D8D8D8", alpha = 0.5))
    panelgrid.minor.y = element_line(colour = add.alpha(col = "#D8D8D8", alpha = 0.2), size=0.2)
    panelgrid.minor.x = element_line(colour = add.alpha(col = "#D8D8D8", alpha = 0.2), size=0.2)
  }
  ret <- theme(rect = element_rect(fill = bgcolor, linetype = 0, 
                                   colour = NA), 
               text = element_text(size = base_size, family = base_family), 
               title = element_text(hjust = 0.5, size =0.8*base_size, face = "plain"), 
               axis.title.x = element_text(face = "plain",size =0.5*base_size, hjust = 0.5), 
               axis.title.y = element_text(face = "plain",size =0.5*base_size, hjust = 0.5), 
               axis.text = element_text(face = "plain",size =0.6*base_size),
               legend.text = element_text(face = "plain",size =0.5*base_size), 
               panel.grid.major.y=panelgrid.major.y,
               panel.grid.major.x=panelgrid.major.x,
               panel.grid.minor.y = panelgrid.minor.y, 
               panel.grid.minor.x = panelgrid.minor.x, 
               
               panel.border = element_blank(), 
               panel.background = element_blank(), legend.position = "bottom", 
               legend.key = element_rect(fill = "#FFFFFF00"))
  if (style == "darkunica") {
    ret <- (ret + theme(rect = element_rect(fill = bgcolor), 
                        text = element_text(colour = "#A0A0A3"), title = element_text(colour = "#FFFFFF"), 
                        axis.title.x = element_text(colour = "#A0A0A3"), 
                        axis.title.y = element_text(colour = "#A0A0A3"), 
                        panel.grid.major.y = element_line(colour = "#707073"), 
                        legend.title = element_text(colour = "#A0A0A3")))
  }
  ret
}