#' Value Matching
#' 
#' Complement of \code{%in%}. Is all x elements in y?
#' @usage x \%inALLx\% y
#' @param x a vector
#' @param y a vector
#' @export
#' @rdname inALLx
"%inALLx%" <- function(x, table) {
  res1<-match(x, table, nomatch = 0) > 0
  sum(res1)==length(x)
}


