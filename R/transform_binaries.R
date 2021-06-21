#' transform_binaries
#' 
#' transforme une série de 
#' 
#' @param data data.frame où sont stockées la.les variables(s)
#' @export

transform_binaries<-function(data=trest, 
                             Check_var="Oui", No_var="",
                             type="external", 
                             external.data=struct_jeune, 
                             external.data.text="text",
                             external.data.names="names"){
  
  data<-as.data.frame(data)
  data[]<-lapply(data, as.character)
  
  if(type=="external"){
    if(is.null(external.data)&is.null(external.data.text)){
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
      res<-factor(x = res, levels = unique(c(unique(res), text.var[names.var==nx]%>%as.character())))
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
        print(class(res))
        print(res)
        res<-factor(x = res, levels = unique(c(unique(res), nx)))
        return(res)
      })%>%data.frame(stringsAsFactors = F)
      names(res)<-names(data)
    }
  }
  return(res)
}

