#' text.forplot permet de changer des caracteres pour les rendres lisible par les polices 'Hershey'. Unicode anti-slash
#'
#' @param input une chaine de caracteres entrante
#' @return input, une chaine de caracteres
#' @export
text.forplot<-function(input=a){
  list.input<-c("é", "è", "ô", "à")
  list.output<-c("\\\\'e", "\\\\`e","\\\\^o","\\\\'a")
  if(length(list.input)!=length(list.output)){message("fatal error occured")}
  for( i in 1:length(list.input)){
    input<-gsub(input, pattern = list.input[i], replacement = list.output[i])
  }
  return(input)
}
#gsub(input, pattern = "é", replacement = "\\\\'e")
#gsub(input, pattern = "è", replacement = "\\\\`e")
#gsub(input, pattern = "ô", replacement = "\\\\^o")
#gsub(input, pattern = "à", replacement = "\\\\'a")
