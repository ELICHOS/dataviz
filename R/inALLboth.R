#' Is all y and x elements are commons, ignoring order?
#' 
#' @usage x \%inALLboth\% y
#' @param x a vector
#' @param y a vector
#' @export
#' @rdname inALLboth
"%inALLboth%" <- function(x, table) {
  res1<-match(x, table, nomatch = 0) > 0
  sum(c(sum(res1)==length(table), sum(res1)==length(x)))==2
}