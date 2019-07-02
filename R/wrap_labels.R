#' wrap.labels permet d'appliquer wrap.it à une liste ou un vecteur
#'
#' @param x chaine de caractere
#' @param len length before wrapping

#' @return
#' @export
wrap.labels <- function(x, len)
{
  if (is.list(x)) # Call this function with a list or vector
  {
    res<-lapply(x, wrap.it, len)
  } else {
    res<-wrap.it(x, len)
  }
  return(res)
}
