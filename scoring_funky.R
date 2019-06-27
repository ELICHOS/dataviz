scoring.funky<-function(matrice.de.distance=matdedist, feedback=FALSE,
                        OUTSIDE=FALSE, df.OUTSIDE=places14.COR, nb.enfant.alternate=NULL){
  ####
  start_time <- Sys.time()
  ####
  matrice.de.distance->tetdi
  #
  creches.map2$NOM<-paste(creches.map2$Etablissement, creches.map2$Commune, sep="_")
  creches.map2[, c("NOM", "Code_commune_INSEE", "Places")]->place.creches.e #"Enfants.inscrits",
  names(place.creches.e)<-c("NOM", "CODE", "places")
  place.creches.e$id<-place.creches.e$NOM
  if(OUTSIDE==TRUE){
    df.OUTSIDE[, c("NUMCOM", "PLACES.com")]->places.e
    names(places.e)<-c("id", "places")
    #rbind(place.assmat.e, place.creches.e)->places.e
  }
  if(OUTSIDE==FALSE){
  CD.assmat.communes[, c("Commune", "Code_commune_INSEE", "nb.place.moyen.assmat")]->place.assmat.e
  names(place.assmat.e)<-c("NOM", "CODE", "places")
  place.assmat.e$id<-place.assmat.e$CODE
  #
  #crechesPAJE.map[, c("X", "Nb.place.retenue")]->place.paje
  #names(place.paje)<-c("id", "places")
  #
  rbind(place.assmat.e, place.creches.e)->places.e#, place.paje)->places.e
  data.frame(aggregate(places.e$places~places.e$CODE, FUN=sum, na.rm=TRUE))->places.communes
  names(places.communes)<-c("id", "places")
  print(places.communes)
  }
  print(places.e)
  #
  vecsco<-lapply(X = 1:nrow(tetdi), FUN = function(e){
    #print(e)
    #### df.e ####
    tetdi[e , ]->dist.e
    row.names(dist.e)->nom.e
    dist.e["rank", ]<-rank(dist.e)
    data.frame(t(dist.e))->dist.e
    dist.e$ID<-row.names(dist.e)
#
    if(OUTSIDE==TRUE){
      names(dist.e)<-c("dist", "rank1", "id")
      dist.e[, c(1, 3)]->dist.e2
      dist.e2$rank2<-rank(dist.e2$dist)
    }
    if(OUTSIDE==FALSE){
    data.frame(t(tetcre[row.names(tetdi[e , ]) , ]))->tetcre.e
    #data.frame(tetcre.e[1:(nrow(tetcre.e)/2) , ])->tetcre.e
    names(tetcre.e)<-row.names(tetdi[e , ])
    tetcre.e$ID<-row.names(tetcre.e)
    names(tetcre.e)<-c("dist", "id")
    names(dist.e)<-c("dist", "rank1", "id")
    rbind(dist.e[, c(1, 3)], tetcre.e[, c(1, 2)])->dist.e2
    dist.e2$rank2<-rank(dist.e2$dist)
    }
#####
    merge(dist.e2, unique(places.e), by=c("id"))->df.e # df.e avec place et distance pour les etablissements
    df.e<-df.e[order(df.e$rank2) , ]
    df.e$places.cumsum<-cumsum(x =df.e$places )
    ####
    if(OUTSIDE==FALSE){
    merge(dist.e, unique(places.communes), by=c("id"))->df.communes # df.e avec place et distance pour les commmunes
    df.communes<-df.communes[order(df.communes$rank1) , ]
    df.communes$places.cumsum<-cumsum(x =df.communes$places )
    }
    ####

    #### e ####
    if( is.null(nb.enfant.alternate) ){
      datasCOR[datasCOR$numzon==row.names(tetdi)[e], "enfM3"]->nbe
    } else {
      enfnb<-as.data.frame(nbenf.M3)
      subset(enfnb, subset=enfnb$id==row.names(tetdi)[e])->tempdfe
      if(nrow(tempdfe)<1){nbe<-NA} else {
        tempdfe[, "nbenf.M3"]->nbe
      }
    }
    if(is.na(nbe)){res<-c(nom.e, NA, NA, NA)} else {
      table(df.e$places.cumsum<=nbe)->tabe
      data.frame(tabe)->dftabe
      #
      if(OUTSIDE==FALSE){
      table(df.communes$places.cumsum<=nbe)->tabe.communes
      data.frame(tabe.communes)->dftabe.communes
      }
      #
      if(dim(dftabe)[1]==1&df.e[1 , "places.cumsum"]>nbe){
        res<-c(nom.e, 0, 0, 1)
      } else {
        (dftabe[dftabe$Var1==TRUE , "Freq"])+1->nbrowe
        df.e[1:nbrowe , ]->df.e
        df.e$alloc.places<-sapply(X = 1:nrow(df.e), FUN = function(f){
          if(df.e$places.cumsum[f]<=nbe){df.e$places[f]} else {
            nbe-(df.e$places.cumsum[f-1])
          }
        })
        if(feedback==TRUE){
          print(df.e)
          message(paste("jusqu'ici tout va bien", e))
        }
        df.e$facteur<-sapply(X = 1:nrow(df.e), FUN = function(g){
          df.e$alloc.places[g]*as.numeric(df.e$dist)[g]
        })
        #### Communes ####
        if(OUTSIDE==FALSE){
        if(dim(dftabe.communes)[1]==1&df.communes[1 , "places.cumsum"]>nbe){
          df.communes<-df.communes[1 , ]
        } else {
        (dftabe.communes[dftabe.communes$Var1==TRUE , "Freq"])+1->nbrowe.communes
        df.communes[1:nbrowe.communes , ]->df.communes
        df.communes$alloc.places<-sapply(X = 1:nrow(df.communes), FUN = function(f){
          if(df.communes$places.cumsum[f]<=nbe){df.communes$places[f]} else {
            nbe-(df.communes$places.cumsum[f-1])
          }
        })
        if(feedback==TRUE){
          print(df.communes)
          message(paste("jusqu'ici tout va bien.communes", e))
        }
        df.communes$facteur<-sapply(X = 1:nrow(df.communes), FUN = function(g){
          df.communes$alloc.places[g]*as.numeric(df.communes$dist)[g]
        })
        }
        }
        #### scores ####
        maxidi*nbe->maxidi.e
        #weighted.mean(x=df.e$facteur, w=df.e$alloc.places, na.rm=TRUE)
        sum(df.e$facteur)->idi.e
        idi.e/nbe->meanometers
        if(OUTSIDE==FALSE){
          nrow(df.communes)->ncom
        }
        if(OUTSIDE==TRUE){
          nrow(df.e)->ncom
        }

####
        res<-c(nom.e, (meanometers)*max.score/maxidi, round(meanometers/1000, 2), ncom )# idi.e*max.score/maxidi.e)
        #}
        #}
      }
    }
    #print(res)
    res
  })
  message("JITVB")
  data.frame(do.call(rbind, vecsco), stringsAsFactors = FALSE)->resdf
  print(resdf)
  message("JITVB2")
  names(resdf)<-c("id", "score", "mean.km", "nb.comm")
  print(resdf)
  message("JITVB3")
  resdf$id<-as.numeric(resdf$id)
  resdf$score<-as.numeric(resdf$score)
  resdf$mean.km<-as.numeric(resdf$mean.km)
  print(resdf)

  message("JITVB4")
  max.km<-maxidi/1000
  res0<-list("resdf"=resdf, "max.km"=max.km)
  return(res0)
  ####
  end_time <- Sys.time()
  end_time - start_time
  ####
}
