library(RODBC)
library(dplyr)
library(data.table)
library(lubridate)
library(openxlsx)
library(stringr)
library(readxl)

# ACA GENERE UN CAMBIO

CAJA <- odbcConnect("caja;UID=d26673516;PWD=manzanas14;DBQ=CAJA;DBA=W;APA=T;EXC=F;FEN=T;QTO=T;FRC=10;FDL=10;LOB=T;RST=T;BTD=F;BNF=F;BAM=IfAllSuccessful;NUM=
                    NLS;DPM=F;MTS=T;MDI=F;CSR=F;FWC=F;FBS=64000;TLO=O;MLD=0;ODA=F", believeNRows=FALSE)

source("//d049nt01/Estudios$/CRUCES BASICOS/Retros/MesAnteriorv4 Simple Beneficio.R")

IndicesDic <-
  sqlQuery(
    CAJA,
    paste(
      "SELECT IND.INDISECU,IND.FECHAAUMEN,IND.INDIFECHAA,IND.INDIVALOR, IND.SECTID
      FROM CAJAJUBITESTUSU.INDICES IND WHERE IND.INDIVIGENT = 'S' AND IND.INDIFECHAA = ' 01/12/2018'"
    ),
    as.is = T
    )

NucDur <- sqlQuery(CAJA,paste("SELECT         Pa.Pagcalsoln,
PA.CALNUCDUR FROM
Cajajubitestusu.Pagocalc Pa                                     WHERE Pa.Pagcalpago = 4779 AND Pa.Pagcalsect <> '190000'
                                     order by PA.PAGCALSOLN asc"),as.is = T)

Retros <- sqlQuery(CAJA,paste("SELECT REC.REBEIMPORT,REC.JBSOLNUMER FROM CAJAJUBITESTUSU.RECBENE3 REC WHERE REC.CPLCOD = 1026 AND REC.RECEMINRO = '201812'"), as.is = T)

TopeVig <- sqlQuery(CAJA,paste("SELECT EXTRACT(month FROM CAITVIGENC) Mes, EXTRACT(year FROM CAITVIGENC) Año , CAITOPEJUB FROM CAJAJUBITESTUSU.CAITOPE cau"), as.is = T)

minimo <- sqlQuery(CAJA,paste("SELECT EXTRACT(MONTH FROM CAIMVIGENC)MONTH, EXTRACT(YEAR FROM CAIMVIGENC) YEAR, CAIMINIMO FROM CAJAJUBITESTUSU.CAIMINIM"), as.is = T)

Rec9002 <- sqlQuery(CAJA,paste("Select rec.jbsolnumer, rec.rebeimport FROM CAJAJUBITESTUSU.RECBENE3 Rec Where rec.cplcod = 9002 and REC.RECEMINRO = 201811"), as.is = T)



BaseParaTrabajar <-
  merge (Rec9002,
         BaseParaTrabajar,
         by.x = "JBSOLNUMER" ,
         by.y = "PAGCALSOLN")


BaseParaTrabajar <-
  merge (UnBeneficioVigente,
         IndicesDic,
         by.x = "PAGCALSECT" ,
         by.y = "SECTID")


BaseParaTrabajar$DiferenciaMeses <-
  difftime(BaseParaTrabajar$INDIFECHAA,
           BaseParaTrabajar$FECHAAUMEN,
           units = "days")

BaseParaTrabajar$DiferenciaMeses <-
  round(BaseParaTrabajar$DiferenciaMeses / 30 , 0)

BaseParaTrabajar <-  BaseParaTrabajar %>% 
  group_by(JBSOLPERNI) %>% 
  mutate(Cantidad  = n())

BaseParaTrabajar$INDIVALOR <-
  as.numeric(sub(",", ".", BaseParaTrabajar$INDIVALOR, fixed = TRUE))


BaseParaTrabajar5 <-  BaseParaTrabajar[BaseParaTrabajar$Cantidad == 5 , ]
rm(BaseParaTrabajar5)
BaseParaTrabajar4 <-  BaseParaTrabajar[BaseParaTrabajar$Cantidad == 4 , ]
rm(BaseParaTrabajar4)

BaseParaTrabajar3 <-  BaseParaTrabajar[BaseParaTrabajar$Cantidad == 3 , ]
rm(BaseParaTrabajar3)

BaseParaTrabajar2 <-  BaseParaTrabajar[BaseParaTrabajar$Cantidad == 2 , ]

BaseParaTrabajar1 <-  BaseParaTrabajar[BaseParaTrabajar$Cantidad == 1 , ]

BaseParaTrabajar0 <-  BaseParaTrabajar[BaseParaTrabajar$Cantidad == 0 , ]
rm(BaseParaTrabajar0)

BaseParaTrabajar2 <-
  arrange(BaseParaTrabajar2, desc(BaseParaTrabajar2$DiferenciaMeses))
BaseParaTrabajar1 <-
  arrange(BaseParaTrabajar1, desc(BaseParaTrabajar1$DiferenciaMeses))


#Cálculo para dos índices
BaseParaTrabajar2$Pasada2 <- 0
BaseParaTrabajar2$Trabajado2 <- 0
BaseParaTrabajar2$PAGCAL9997N <- 0

n <- nrow(BaseParaTrabajar2)
i <- 1
a <- 1
b <- nrow(BaseParaTrabajar2)

while(i < n+1)
{
  a <- 1
  
  if(  BaseParaTrabajar2[i,]$DiferenciaMeses > 1 & BaseParaTrabajar2[i,]$Trabajado2 == 0 & BaseParaTrabajar2[i,]$Pasada2 == 0 ) {
    
    BaseParaTrabajar2[i,]$PAGCAL9997N <- BaseParaTrabajar2[i,]$PAGCAL9997 * (1 + BaseParaTrabajar2[i,]$INDIVALOR)
    BaseParaTrabajar2[i,]$Trabajado2 <- 1
    BaseParaTrabajar2[i,]$Pasada2 <- 1  
    while(a < b+1){
      if(BaseParaTrabajar2[a,]$Trabajado2 != 1 & BaseParaTrabajar2[a,]$JBSOLPERNI == BaseParaTrabajar2[i,]$JBSOLPERNI ){
        BaseParaTrabajar2[a,]$PAGCAL9997 <- BaseParaTrabajar2[i,]$PAGCAL9997N
        BaseParaTrabajar2[a,]$PAGCAL9997R <- BaseParaTrabajar2[a,]$PAGCAL9997 *  BaseParaTrabajar2[a,]$PAGCALPORP *  BaseParaTrabajar2[a,]$PAGCALPORD
        BaseParaTrabajar2[a,]$PAGCAL9997N <- BaseParaTrabajar2[a,]$PAGCAL9997 * (1 + BaseParaTrabajar2[a,]$INDIVALOR)
        BaseParaTrabajar2[a,]$Pasada2 <- 1 
        BaseParaTrabajar2[i,]$Trabajado2 <- 1
        
        break
      }
      a <- a+1
    }
    
    
  }
  
  i <- i +1
  
  
}


BaseParaTrabajar1$PAGCAL9997N <- 0

BaseParaTrabajar1$PAGCAL9997N <- BaseParaTrabajar1$PAGCAL9997 * (1 + BaseParaTrabajar1$INDIVALOR)
    
  

BaseParaTrabajar1 <- BaseParaTrabajar1[,-c(24,21,20,19,14)]   

 BaseParaTrabajar2 <- BaseParaTrabajar2[,-c(14,19,20,21,24,25,26)]
 
 
 BaseParaTrabajarFinal <- rbind(BaseParaTrabajar1,BaseParaTrabajar2)
 BaseParaTrabajarFinal2 <- BaseParaTrabajarFinal
 
 rm(BaseParaTrabajar2,BaseParaTrabajar1,BaseParaTrabajar)
 
BaseParaTrabajarFinal$Pagcal9002N <- BaseParaTrabajarFinal$PAGCAL9997N * BaseParaTrabajarFinal$PAGCALPORP * BaseParaTrabajarFinal$PAGCALPORD

BaseParaTrabajarFinal <- merge(BaseParaTrabajarFinal,NucDur, by = "PAGCALSOLN")
BaseParaTrabajarFinal$CALNUCDUR.y <- as.numeric(sub(",", ".", BaseParaTrabajarFinal$CALNUCDUR.y, fixed = TRUE))

BaseParaTrabajarFinal$NUCDURDIS.Y <- BaseParaTrabajarFinal$CALNUCDUR.y * BaseParaTrabajarFinal$PAGCALPORP * BaseParaTrabajarFinal$PAGCALPORD

BaseSectorExento <- BaseParaTrabajarFinal[BaseParaTrabajarFinal$SECTEXETOP == "S", ]

BaseParaTrabajarFinal <- BaseParaTrabajarFinal[!(BaseParaTrabajarFinal$JBSOLPERNI %in% BaseSectorExento$JBSOLPERNI), ]

BaseParaTrabajarMayorTope <- BaseParaTrabajarFinal[BaseParaTrabajarFinal$Pagcal9002N > BaseParaTrabajarFinal$Tope, ]
 
BaseParaTrabajarFinal <- BaseParaTrabajarFinal[!(BaseParaTrabajarFinal$JBSOLPERNI %in% BaseParaTrabajarMayorTope$JBSOLPERNI), ]

BaseParaTrabajarFinal$Pagcal9002N <- ifelse(BaseParaTrabajarFinal$Pagcal9002N < BaseParaTrabajarFinal$NUCDURDIS.Y ,BaseParaTrabajarFinal$NUCDURDIS.Y, BaseParaTrabajarFinal$Pagcal9002N )

BaseParaTrabajarMayorTope$Pagcal9002N <- BaseParaTrabajarMayorTope$Pagcal9002N * 0.90

BasePagaTope <- BaseParaTrabajarMayorTope[BaseParaTrabajarMayorTope$Pagcal9002N < BaseParaTrabajarMayorTope$Tope, ]

BasePagaTope$Pagcal9002N <- ifelse(BasePagaTope$Pagcal9002N < BasePagaTope$Tope, BasePagaTope$Tope, BasePagaTope$Pagcal9002N )

BasePagaTope$Pagcal9002N <- ifelse(BasePagaTope$Pagcal9002N < BasePagaTope$NUCDURDIS.Y , BasePagaTope$NUCDURDIS.Y, BasePagaTope$Pagcal9002N )

BaseParaTrabajarMayorTope <- BaseParaTrabajarMayorTope[!(BaseParaTrabajarMayorTope$JBSOLPERNI %in% BasePagaTope$JBSOLPERNI), ]

BaseParaTrabajarMayorTope$Pagcal9002N <- ifelse(BaseParaTrabajarMayorTope$Pagcal9002N < BaseParaTrabajarMayorTope$Tope,  BaseParaTrabajarMayorTope$Tope, BaseParaTrabajarMayorTope$Pagcal9002N )

BaseParaTrabajarMayorTope$Pagcal9002N <- ifelse(BaseParaTrabajarMayorTope$Pagcal9002N < BaseParaTrabajarMayorTope$NUCDURDIS.Y ,  BaseParaTrabajarMayorTope$NUCDURDIS.Y, BaseParaTrabajarMayorTope$Pagcal9002N )





 Retro <- rbind(BaseParaTrabajarMayorTope,BasePagaTope,BaseParaTrabajarFinal,BaseSectorExento)

  n <- nrow(Retro)
 i <- 1
 
 Retro$PAGCAL9997RX <- 0
 
 while(i < n+1)
 {
   Retro[i,]$PAGCAL9997RX <- CalcTope(Retro[i,]$PAGCAL9997N,Retro[i,]$DiferenciaMeses,Retro[i,]$PAGCALPORP,Retro[i,]$PAGCALPORD )
   
   i <- i+1
 }
 # 
 # n <- nrow(Retro)
 # i <- 1
 # while(i < n+1)
 # {
 #   Retro[i,]$PAGCAL9997RX <- Minimo(Retro[i,]$PAGCAL9997N,Retro[i,]$DiferenciaMeses,Retro[i,]$PAGCALPORP,Retro[i,]$PAGCALPORD )
 #   
 #   i <- i+1
 # }

 Retro$Pagcal9002N <- ifelse((Retro$Pagcal9002N < 10000) & (Retro$PAGCALPORD  == 1) &(Retro$JBSOLPOJ == "P") , 10000, Retro$Pagcal9002N)
 
 
 
 
 Retro$CalculoRetro <- (Retro$Pagcal9002N - Retro$PAGCAL9997R) * Retro$DiferenciaMeses
 
 
 Retro$CalculoRetro <- round(Retro$CalculoRetro, 2)
 
 

 
 Retro$CalculoRetro2 <- ifelse(Retro$Pagcal9002N != Retro$PAGCAL9997RX, Retro$PAGCAL9997RX - Retro$PAGCAL9997R, 0)
 Retro$CalculoRetro3 <- ifelse(Retro$Pagcal9002N != Retro$PAGCAL9997RX, Retro$Pagcal9002N - Retro$PAGCAL9997RX, 0)
 
 Retro$CalculoRetro2 <- round(Retro$CalculoRetro2, 2)
 Retro$CalculoRetro3 <- round(Retro$CalculoRetro3, 2)
 
 
 Retro <-  Retro %>% 
   group_by(JBSOLPERNI) %>% 
   mutate(Suma  = sum(CalculoRetro))
 
 Retro$CalculoRetro <- ifelse((Retro$Suma - Retro$REBEIMPORT) > 200 | (Retro$Suma - Retro$REBEIMPORT) < - 200, Retro$CalculoRetro - Retro$CalculoRetro3, Retro$CalculoRetro)
 Retro <-  Retro %>% 
   group_by(JBSOLPERNI) %>% 
   mutate(Suma  = sum(CalculoRetro))
 
 
 
 Retro <- merge(Retro,Retros, by.x = "PAGCALSOLN" , by.y = "JBSOLNUMER")
 Retro$REBEIMPORT <- as.numeric(sub(",", ".", Retro$REBEIMPORT, fixed = TRUE))
 
 BaseParaTrabajar3 <- Retro[(Retro$REBEIMPORT - Retro$Suma) < -100 | (Retro$REBEIMPORT - Retro$Suma) > 100  , ]
 
 
 CalcTope  <-function(bas,diferenciaMeses,porPen,porDis){
   if ( diferenciaMeses != 0) {
   mes <- 12 - diferenciaMeses
   año <- 2018 
   bas <- bas * porPen
   Tope <- TopeVig[TopeVig$MES == mes & TopeVig$AÑO == 2018 , 3 ]
  
   Tope <- as.numeric(sub(",", ".", Tope , fixed = TRUE))
     
     if (bas > Tope){
       bas <- bas * 0.90
       if ( bas > Tope ){
         bas997top <- bas
       }
       else{ bas997top <- Tope }
     }
    
    else { bas997top <- bas}
    
    
   
   }
   else {
     bas997top <- bas
   }
   return (bas997top*porDis)
 }
 
Minimo <- function(bas,diferenciaMeses,porPen,porDis){
  mes <- 12 - diferenciaMeses
  año <- 2018
  bas <- bas * porPen
  minimo2 <- minimo[minimo$MONTH == mes & minimo$YEAR == año, minimo$CAIMINIMO]
  
  if ( bas < minimo2){
   bas997min <- minimo2 
   bas997min <- bas996min * porDis
  }
  else{
    bas997min <- bas
    bas997min <- bas997min * porDis
  }
 
    return (bas997min)
    }
   
   
   
   
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 



table(BaseParaTrabajar$PAGCALSECT)

