#' Value Matching
#' 
#' Complement of \code{%in%}. Is all y elements in x?
#' @usage x \%inALLy\% y
#' @param x a vector
#' @param y a vector
#' @export
#' @rdname inALLy
"%inALLy%" <- function(x, table) {
  res1<-match(x, table, nomatch = 0) > 0
  sum(res1)==length(table)
}
