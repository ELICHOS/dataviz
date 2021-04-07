#' @export
kahtml<-function(data, vars, row_col, atridata, alter.datanames=NULL){#c("QP1.MOUI.", "QP2.MOUI.")){
  if(row_col=="row"){
    row_col<-"print_row"
  }
  if(row_col=="col"){
    row_col<-"print_col"
  }
  if(!is.null(alter.datanames)){
    varsnames<-alter.datanames
    if(sum(!varsnames%in%atridata$names)>0){
      labar1<-varsnames[1]
        labar2<-varsnames[2]
    } else {
      labar1<-atridata$question.text[atridata$names==varsnames[1]]
      labar2<-atridata$question.text[atridata$names==varsnames[2]]
    }
  } else {
    varsnames<-vars
    labar1<-atridata$question.text[atridata$names==varsnames[1]]
    labar2<-atridata$question.text[atridata$names==varsnames[2]]
  }
  
table.con(data = data, var = vars, )[[row_col]]->totab

  pk<-totab%>%  
  kable(format = "html", escape = TRUE,
        caption = paste0('<strong> <div align="right">',  "\u21DB", labar2, "</div> </strong> "))%>%
  kable_styling()%>%
    pack_rows(group_label = paste0("\u290B ", labar1), 
              start_row = 1, end_row = length(unique(data[ , vars[1]])))
  
  # if(row_col=="row"){
  #   pk<-pk%>%
  #   column_spec(column = ncol(totab)+1, bold = TRUE)
  # } else {
  # if(row_col=="col"){
  #   print("COUCOU")
  #   print("_____________________________________________________")
  #   
  #   #pk<-pk%>%
  #   #row_spec(row = 5, bold = TRUE)
  # }}
  # 
  # pk<-pk%>%pack_rows(group_label = paste0("\u290B ", labar1), 
  #               start_row = 1, end_row = length(unique(data[ , vars[1]])))
  return(pk)
}
