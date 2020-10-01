"%inALLx%" <- function(x, table) {
  res1<-match(x, table, nomatch = 0) > 0
  sum(res1)==length(x)
}
"%inALLy%" <- function(x, table) {
  res1<-match(x, table, nomatch = 0) > 0
  sum(res1)==length(table)
}
"%inALLboth%" <- function(x, table) {
  res1<-match(x, table, nomatch = 0) > 0
  sum(c(sum(res1)==length(table), sum(res1)==length(x)))==2
}

