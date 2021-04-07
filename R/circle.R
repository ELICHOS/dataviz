#' @export
circle <- function(x,y,r,n=1000){
  # SOURCE : https://github.com/gaospecial/ggVennDiagram
  angles <- seq(0,2*pi,length.out = n)
  xv <- cos(angles) * r + x
  yv <- sin(angles) * r + y
  xv <- round(xv,6)
  yv <- round(yv,6)
  data.frame(x=xv,y=yv)
}