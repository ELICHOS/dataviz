#' transform_binaries
#' 
#' transforme une série de 
#' 
#' @param data data.frame où sont stockées la.les variables(s)
#' @param var character. Nom de la variable
#' @param var.pattern character. Pattern qui identifie les variables.Pour variables multiples.
#' @param var.exclude character. Pattern a exclure de la recherche de variables. Pour variables multiples.
#' @param var.order vector of characters. Order des levels à que l'on souhaite spécifier. 
#' @param exclude vector of characters. Levels à supprimer dans le décompte. Exemple : c(NA, "", " ", "non-réponse")
#' @param exclude.recod character. Si !is.null(exclude) permet de recoder les levels exclus. Exemple : "non-réponse". 
#' @param question.lab Label de la question posée. 
#' @param id
#' @param equivalence.data data.frame donnant la structure du questionnaire, associant les noms de variables dans le data.frame original (equivalence.var) et un intitulé de question (equivalence.text). 
#' @param equivalence.var
#' @param equivalence.text
#' @param equivalence.gsub.patt
#' @param var.group faut-il dénombrer par groupes? Variable dans le data.frame original servat à grouper (character: nom de variable | numeric : position de la variable | un vecteur : la variable pour les groupes, de la même taille que la variable originale).
#' @param var.group.order
#' @param Rnd arrondi pour les pourcentages
#' @export

transform_binaries<-function(data=trest, 
                             Check_var="Oui", No_var="",
                             type="external", 
                             external.data=struct_jeune, 
                             external.data.text="text",
                             external.data.names="names"){
  if(type=="external"){
    if(is.null(external.data)){
      stop("Si type=external, il faut spécifier external.data")
    }
    
    if(length(external.data.text)==1&class(external.data.text)=="character"){
      text.var<-external.data[ ,external.data.text]
    } else {
      if(length(external.data.text)>1){
        text.var<-external.data.text
      }
    }
    if(length(external.data.names)==1&class(external.data.names)=="character"){
      names.var<-external.data[ ,external.data.names]
    }else {
      if(length(external.data.names)>1){
        names.var<-external.data.names
      }
    }
    if(length(text.var)!=length(names.var)){stop("length(text.var)!=length(names.var)")}
    
    res<-lapply(names(data), function(nx){
      data[ , nx]->res
      res[!res%in%Check_var]<-No_var
      if(!nx%in%names.var){stop(paste0(nx, " / ", "!names%in%names.var"))}
      res[res%in%Check_var]<-text.var[names.var==nx]%>%as.character()
      return(res)
    })%>%data.frame(stringsAsFactors = F)
    names(res)<-names(data)
    return(res)
  } else {
    if(type=="names"){
      res<-lapply(names(data), function(nx){
        data[ , nx]->res
        res[!res%in%Check_var]<-No_var
        res[res%in%Check_var]<-nx
        return(res)
      })%>%data.frame(stringsAsFactors = F)
      names(res)<-names(data)
    }
  }
  return(res)
}

