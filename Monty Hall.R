# Montti Hall

Nrepit <- 1000

acierto <- 0
fallo <- 0
for(i in 1:Nrepit){
puert <- c("a","b","c")

Pcor <- sample(puert, size=1)

Pinc <- puert[!puert %in% Pcor]

Pelec1 <- sample(puert,size = 1)

Prest <- Pinc[!Pinc %in% Pelec1]

Pdesc <- sample(Prest, size=1)

Prest2 <- puert[!puert %in% Pdesc]

Pelec2 <- Pelec1


if (Pelec2 == Pcor) {

acierto <- acierto + 1}
else{
  fallo <- fallo + 1
}
}


Prob_acierto <- acierto/Nrepit


#caso que cambia de eleccion

Nrepit <- 1000

acierto <- 0
fallo <- 0
for(i in 1:Nrepit){
  puert <- c("a","b","c")
  
  Pcor <- sample(puert, size=1)
  
  Pinc <- puert[!puert %in% Pcor]
  
  Pelec1 <- sample(puert,size = 1)
  
  Prest <- Pinc[!Pinc %in% Pelec1]
  
  Pdesc <- sample(Prest, size=1)
  
  Prest2 <- puert[!puert %in% Pdesc]
  
  Pelec2 <- Prest2[!Prest2 %in% Pelec1]
  
  
  if (Pelec2 == Pcor) {
    
    acierto <- acierto + 1}
  else{
    fallo <- fallo + 1
  }
}


Prob_acierto <- acierto/Nrepit




