#'  wrap.it permet d'insérer des codes "\ n" (to the line) dans une chaine de caractere
#'
#' @param x chaine de caractere
#' @param len length before wrapping
#'
#' @return
#' @export
wrap.it <- function(x, len)
{
  res<-sapply(x, function(y) paste(strwrap(y, len),
                                   collapse = "\n"),
              USE.NAMES = FALSE)
  return(res)
}


