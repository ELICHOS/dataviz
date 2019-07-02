diffi<-c( "Plus difficile aujourd'hui",
          "Aussi difficile aujourd'hui",
          "Ne sait pas",
          "Moins difficile aujourd'hui")
nombri<-c("Aussi nombreuses", "Moins nombreuses","Ne sait pas","Plus nombreuses")[c(4, 1, 3, 2)]
futuri<-c("Ne sait pas",   "S'accroitre" ,  "Se réduire" ,   "Se stabiliser")[c(2, 4, 1, 3)]
nonoui<-c("Non", "Ne sait pas", "Oui")
stabi<-c("Est restée stable", "Ne sait pas",       "S'est améliorée",   "S'est dégradée" )[c(4, 1, 2, 3)]
quali<-c("Ne sait pas",            "Se sont améliorées",
         "Se sont dégradées","Sont restées les mêmes")[c(3, 4, 1, 2)]
####
nega<-rgb(218, 0, 0, maxColorValue = 255)#rgb(196, 89, 17, maxColorValue = 255) #brewer.pal(n = 9, name = "Reds")[5]
posi<-rgb(142,193,75, maxColorValue = 255)#brewer.pal(n = 9, name = "Greens")[6]
#posi<-rgb(0,109,44, maxColorValue = 255)#brewer.pal(n = 9, name = "Greens")[6]
stab<-rgb(238, 234, 197, maxColorValue = 255)#brewer.pal(n = 8, name = "Set2")[6]
#nesp<- rgb(173, 158, 157, maxColorValue = 255)#rgb(52, 119, 128, maxColorValue = 255)#brewer.pal(n = 9, name = "Blues")[6]
nesp<-gray(level = 0.2)
unkn<-gray(level = 0.5)
