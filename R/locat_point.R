locat.point<-function(point.coord=c(45.778386, 4.840662), df.squares=df.corners){
veci<-sapply(1:nrow(df.squares), function(i){
  if(point.coord[1]>df.squares[i, "X3"]&point.coord[1]<df.squares[i, "X4"]&point.coord[2]>df.squares[i, "X1"]&point.coord[2]<df.squares[i, "X2"]){df.squares[i , ]} 
}
)
do.call("rbind", veci)->veci
veci->res
return(res)
}
