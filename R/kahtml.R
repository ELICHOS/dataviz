#' kahtml
#' 
#' calcul ( avec basiques::table.con() ) et exporte au format html via kable() une table de contingence. 
#' 
#' @param data data.frame. Voir table.con
#' @param vars voir table.con
#' @param row_col si "row" affiche la somme par ligne. Si "col", affiche la somme par colonnes. Tous les noms utilisés dans l'output de table.con peuvent être utilisés.
#' @param struct_data éventuellement, data.frame avec les données de structure du questionnaire (comprenant struct_data$question.text et struct_data$names).
#' @param alter.datanames manuel : c("Libellé pour la variable 1", "Libellé pour la variable 2")
#' @export
kahtml<-function(data, vars, row_col, struct_data, alter.datanames=NULL){#c("QP1.MOUI.", "QP2.MOUI.")){
  if(row_col=="row"){
    row_col<-"print_row"
  }
  if(row_col=="col"){
    row_col<-"print_col"
  }
  if(!is.null(alter.datanames)){
    varsnames<-alter.datanames
    if(sum(!varsnames%in%struct_data$names)>0){
      labar1<-varsnames[1]
        labar2<-varsnames[2]
    } else {
      labar1<-struct_data$question.text[struct_data$names==varsnames[1]]
      labar2<-struct_data$question.text[struct_data$names==varsnames[2]]
    }
  } else {
    varsnames<-vars
    labar1<-struct_data$question.text[struct_data$names==varsnames[1]]
    labar2<-struct_data$question.text[struct_data$names==varsnames[2]]
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
