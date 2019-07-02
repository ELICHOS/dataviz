#' @export
clean.html <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}