library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(plotly)
library(stringr)
library(purrr)

qqptest<-read_csv("/home/allan/QQP/panales.csv") %>%
  select(-X1)

names(qqptest)

qqptest <- qqptest %>%
  mutate(fecha= as.Date(paste(year(fechaRegistro),month(fechaRegistro),day(fechaRegistro), sep=","), "%Y,%m,%d" )) %>%
  mutate(fecha= floor_date(fecha , "week")) 

#ceiling_date(as.Date("2017-10-18") , "week")
#floor_date(as.Date("2017-10-18") , "week")

setwd("/home/allan/IndiceDePreciosPromedio")
a<-list.files()

inegi2011<-read_excel(a[1], skip=5)

for (i in 2:length(a)){
  inegi2011<-inegi2011 %>%
    bind_rows(read_excel(a[i], skip=5))
}

rm(a,i)

resumen<-inegi2011 %>%
  group_by(Año, Mes) %>%
  summarise(frec=n())
table(inegi2011$Año )
rm(resumen)

names(inegi2011)[c(1,4,5,10,13,14,16)]<-c("year",
                    "Clave_ciudad",
                    "Nombre_ciudad",
                    "Clave_generico",
                    "Especificacion",
                    "Precio_promedio",
                    "unidad")

last_element <- function(v) {
  n<-length(v)
  v[n]
}

first_element <- function(v) {
  v[1]
}

second_element <- function(v) {
  v[2]
}


paquetes<-data.frame(paquete=unlist(map(str_split(inegi2011$Especificacion, ","), last_element )))
marca<-data.frame(marca=unlist(map(str_split(inegi2011$Especificacion, ","), first_element )))
SIZE<-data.frame(Tamano=unlist(map(str_split(inegi2011$Especificacion, ","), second_element )))
inegi2011<-bind_cols(inegi2011, paquetes, marca, SIZE) %>%
  mutate(Tamano = str_trim(Tamano))

rm(last_element, paquetes, marca, SIZE)


inegi2011.1<-inegi2011 %>%
  mutate( fecha= as.Date(paste(year,Mes,"15", sep=","), "%Y,%m,%d" )) %>% 
  select(-Fecha_Pub_DOF, -División, -Grupo, -Clase, -Subclase, -Generico, -year, -Mes) %>%
  mutate(paquete = str_trim(paquete)) %>%
  mutate(paquete = str_replace_all(paquete, "PZS$", "PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PZA$", "PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PZ$", "PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "P$", "PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PIEZA$", "PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PIEZAS$", "PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "\\.", "_")) %>%
  mutate(paquete = str_replace_all(paquete, " ", "_")) %>%
  mutate(paquete = str_replace_all(paquete, "__", "_")) %>%
  mutate(paquete = str_replace_all(paquete, "BOLSA_C", "PAQ_C")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_DE_", "PAQ_C/")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_CON_", "PAQ_C/")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQUETE_CON_", "PAQ_C/")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/_", "PAQ_C/")) %>%
  mutate(paquete = str_replace_all(paquete, "PAÑALES", "PZAS/")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/40_PZAS_", "PAQ_C/40_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_40_PZAS", "PAQ_C/40_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/40_PZAS/", "PAQ_C/40_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "BOLSA_C/40_PZAS", "PAQ_C/40_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "BOLSA_DE_40_PZAS", "PAQ_C/40_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_38_PIEZAS", "PAQ_C/38_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_38_PZAS", "PAQ_C/38_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/14_PZAS/", "PAQ_C/14_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/14_PZAS_CLASSIC", "PAQ_C/14_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/20_PZAS/", "PAQ_C/20_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/35_PZAS_ETAPA_3", "PAQ_C/35_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/38_PZAS_", "PAQ_C/38_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/38_PZAS/", "PAQ_C/38_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/46_PZAS_5_ETAPA", "PAQ_C/46_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/52_PZAS_4_ETAPA", "PAQ_C/52_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQ_C/56_PZAS_ETAPA_2", "PAQ_C/56_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQUETE_C/31_PZAS", "PAQ_C/31_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "PAQUETE_C/38_PZAS", "PAQ_C/38_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "SUAVELASTIC_DE_9_A_13_KG_DE_38_PZAS", "PAQ_C/38_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "SUAVELASTIC_MAX_PAQ_C/40_PZAS", "PAQ_C/40_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "ULTRACONFORT_NIÑO_PAQ_C/36_PZAS_E3", "PAQ_C/36_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^9_A_13_KG_PAQ_C/14_PZAS$", "PAQ_C/14_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^PAQ_36_PZAS$", "PAQ_C/36_PZAS")) %>% 
  mutate(paquete = str_replace_all(paquete, "^KIDDIES_ANTIFUGAS_PAQ_C/40_PZAS$", "PAQ_C/40_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^PAQ_36_DE_PZAS$", "PAQ_C/36_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^PAQ_C/26$", "PAQ_C/26_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^(_7_-_13_KG_)_PAQ_C/60_PZAS$", "PAQ_C/60_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^PANTS_PAQ_C/24_PZAS$", "PAQ_C/24_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^ETAPA_3_PAQ_C/38_PZAS$", "PAQ_C/38_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^ABSORSEC_PAQ_C/14_PZAS$", "PAQ_C/14_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^DE_9-13_KG_PAQ_C/14_PZAS$", "PAQ_C/14_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^PAQ_60_PZAS$", "PAQ_C/60_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^DE_9_A13_KG_PAQ_C/14_PZAS$", "PAQ_C/14_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^ULTRACONFORT_NIÑO_PAQ_C/38_PZASE3$", "PAQ_C/38_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^ABSORSEC_DE_6_A_10_KG_PAQ_C/14_PZAS$", "PAQ_C/14_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^NIÑO_O_NIÑA_PAQ_C/76_PZAS$", "PAQ_C/76_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^CONFORT_PAQ_C/10_PZAS$", "PAQ_C/10_PZAS")) %>%
  mutate(paquete = str_replace_all(paquete, "^SUAVELASTIC_MAX_PAQ_C/38_PZAS$", "PAQ_C/38_PZAS"))
# "^apple$" PZA
#  mutate(paquete = recode(paquete,
#                          PAQ_DE_40_PZAS="PAQ_C/40_PZAS",
#                          PAQ_DE_38_PZAS="PAQ_C/38_PZAS",
#                          BOLSA_DE_18_PZAS="PAQ_C/18_PZAS") )

inegi2011.1$Precio_promedio<-as.numeric(inegi2011.1$Precio_promedio)
class(inegi2011.1$Precio_promedio)

frec<-inegi2011.1 %>%
  group_by(paquete) %>%
  summarise(frec=n())

frecM<-inegi2011.1 %>%
  group_by(marca, Tamano) %>%
  summarise(frec=n())

rm(frec, frecM, first_element, last_element, second_element)

#########################################################################
##### graficas INEGI

inegi2011.1 %>%
  filter(paquete == "PAQ_C/40_PZAS",
         year(fecha)==2014,
         Tamano %in% c("GRANDE", "GRANDE ")) %>%
  select(fecha, Especificacion, Precio_promedio, unidad, Consecutivo, Nombre_ciudad,paquete) %>%
  group_by(fecha, Precio_promedio) %>%
  summarise(frec=n() ) %>%
  ggplot(aes(x=fecha, y=Precio_promedio, size=frec))+
  geom_point()

#########################################################################
##### graficas QQP

table(qqptest$marca)
names(qqptest)

qqp<-qqptest %>% 
  filter(year(fecha) %in% c(2011, 2012, 2013, 2014)) %>%
  group_by(fecha, presentacion, marca, precio) %>%
  summarise(frec=n()) %>%
  mutate(CVE=paste(marca, presentacion, sep=" ")) %>%
  filter(!(CVE %in% c("BEBIN PAQUETE 38 PIEZAS.",
                    "CHICOLASTIC. KIDDIES PAQUETE 40 PIEZAS. TALLA 4",
                    "HUGGIES. SUPREME PAQUETE 40 PIEZAS. ETAPA 4. NIÑA",
                    "HUGGIES. ULTRA CONFORT PAQUETE 38 PIEZAS. ETAPA 4. NIÑA",
                    "KLEEN BEBE PAQUETE 14 PIEZAS. SUAVELASTIC. MAX. GRANDE"))) 


table(qqp$CVE, year(qqp$fecha))

p<-ggplot()+
  geom_point(data=qqp, aes(x=fecha, y=precio, size=frec, group=CVE, col=CVE, fill=CVE), alpha=.3 , shape=21)+
  scale_radius(range = c(1, 20))
p

ggplotly(p)

#########################################################################

inegi<-inegi2011.1 %>%
  filter(paquete == "PAQ_C/40_PZAS",
         year(fecha) %in% c(2011, 2012, 2013, 2014, 2015),
         Tamano %in% c("GRANDE", "GRANDE ")
         #Nombre_ciudad=="Área Met. de la Cd. de México"
         ) %>%
  select(fecha, Especificacion, Precio_promedio, unidad, Consecutivo, Nombre_ciudad,paquete) %>%
  group_by(fecha, Precio_promedio) %>%
  summarise(frecn=n() )

###----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+###

inegi_consec<-inegi2011.1 %>%
  filter(paquete == "PAQ_C/40_PZAS",
         year(fecha) %in% c(2011, 2012, 2013, 2014, 2015),
         Tamano %in% c("GRANDE", "GRANDE ")
         #Nombre_ciudad %in% c("Área Met. de la Cd. de México", 
         #                     "Monterrey, N.L.", 
         #                     "Guadalajara, Jal.")
         ) %>%
  select(fecha, Especificacion, Precio_promedio, Consecutivo, Nombre_ciudad, paquete, marca) %>%
  mutate(Grupo=paste(Consecutivo,Nombre_ciudad, sep="-")) %>%
  select(Grupo, fecha, Especificacion, Precio_promedio, marca)

table(inegi2011.1$Nombre_ciudad)
table(inegi2011.1$SIZE)

#### INEGI vs QQP
#########################################################################

p<-
ggplot()+
   geom_point(data=inegi, aes(x=fecha, y=Precio_promedio, size=frecn),alpha=.1, col="blue")+
   geom_point(data=qqp, aes(x=fecha, y=precio, size=(frec/100)), alpha=.1)+
   geom_line(data=inegi_consec, aes(x=fecha, y=Precio_promedio, group=Grupo, col=marca))

ggplotly(p)

# isalas@salascano.com
#########################################################################

qqptest %>%
#  filter(year(fechaRegistro)>2013) %>%
  filter(year(fechaRegistro)==2015) %>%
  mutate(YEAR_WEEK=year(fechaRegistro)*100+week(fechaRegistro)) %>%
  filter(producto %in% c("PANALES DESECHABLES", "PAÑALES DESECHABLES")) %>%
  group_by(producto, precio, YEAR_WEEK) %>%
  summarise(Freq=n()) %>%
  ggplot(aes(x=YEAR_WEEK, y=precio, size=Freq, alpha=.0000005))+
  geom_point()




categoria<-qqptest %>%
  group_by(producto, categoria) %>%
  summarise(freq=n())

names(qqptest)

#str(qqptest)


