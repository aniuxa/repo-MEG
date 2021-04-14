#==============================================================================
# Fecha: 2021-04-07 
# Ejemplo de construcción de un índice
# ==============================================================================


# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere

pacman::p_load(tidyverse, 
               readxl,writexl,googlesheets4, # importar hojas de cálculo
               haven, foreign, # importacón de dta y sav
               sjlabelled, GGally, RColorBrewer, # etiquetas
               janitor, skimr, # Limpieza
               esquisse, stringr, pracma) 



# Datos                   -----------------------------------------------

BD_MUJERES_ENDIREH2016_sample <- read_dta("https://github.com/aniuxa/repo-MEG/raw/main/datos/BD_MUJERES_ENDIREH2016_sample.dta")

## Etiquetas de la práctica anterior -----------------------------------

sit_conyugal.lab<-c("casada-unida",  "Unida+pareja ausente", "Separada o Divorciada", 
                    "viuda", "Soltera + novio", "Soltera con ex", "Soltera nunca novio")


BD_MUJERES_ENDIREH2016_sample$sit_conyugal<-set_labels(BD_MUJERES_ENDIREH2016_sample$sit_conyugal, 
                                                       labels=sit_conyugal.lab) # establece las etiquetas



niv.shortlab <- c("Ninguna", "Preescolar", "Primaria", "Secundaria", "Prepa", "Técnico -pri",
                  "Técnico -sec", "Técnico -prepa", "Normal", "Normal-lic", 
                  "Licenciatura", "Posgrado") #Vector de etiquetas cortas.

BD_MUJERES_ENDIREH2016_sample$niv<-set_labels(BD_MUJERES_ENDIREH2016_sample$niv,
                                              labels=niv.shortlab) # establece las etiquetas


freq.shortlab <- c("muchas veces", "pocas veces", "una vez", 
                   "no ocurrió", "no_especificado") #Vector de etiquetas cortas.


BD_MUJERES_ENDIREH2016_sample <- BD_MUJERES_ENDIREH2016_sample %>% 
  dplyr::mutate_at(vars(p13_1_1:p13_1_22), ~ set_labels(.x, labels=freq.shortlab))


si_no<-c("Sí", "No", "No especificado")

# IMPORTANTE: REVISAR SI LAS VARIABLES SON NUMÉRICAS, NO SE PUEDE ETIQUETAR UNA VARIABLE NO NUMÉRICA

BD_MUJERES_ENDIREH2016_sample <- BD_MUJERES_ENDIREH2016_sample %>%  
  dplyr::mutate_at(vars(p12_1_1_1:p12_1_1_29), ~ as.numeric(.x) ) %>%  # Vuelve numérico
  dplyr::mutate_at(vars(p12_1_1_1:p12_1_1_29), ~ set_labels(.x, labels=si_no)) 

class(BD_MUJERES_ENDIREH2016_sample$p12_1_1_30)
table(BD_MUJERES_ENDIREH2016_sample$p12_1_1_30)



BD_MUJERES_ENDIREH2016_sample <- BD_MUJERES_ENDIREH2016_sample %>% 
  dplyr::mutate_at(vars(starts_with("p13_1_")), ~ set_labels(.x, labels=freq.shortlab))

## Vamos a construir indices con los valores los primeros 22 items de la sección 13 ---


index <-  BD_MUJERES_ENDIREH2016_sample %>% 
  filter(sit_conyugal<7) %>% # Ojo!!!!!!! Siempre checar a quién le toca la sección
  select(id_muj, p13_1_1:p13_1_22) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(p13_1_1:p13_1_22), ~na_if(.x, 9)) %>%  # manda los 9 a missing
  dplyr::mutate_at(vars(p13_1_1:p13_1_22), ~ (4 - .x))  # cambia la lógica de la numeración a 4 se le resta el valor. Cambia sentido

# Hoy hacemos el índice
index<- index %>% 
  mutate(index= rowMeans(across(p13_1_1:p13_1_22), na.rm = T)) %>% 
  mutate(index=index/3) # nos quedan una metrica del 0 al 3 

hist(index$index)

index<-index %>% 
  select(id_muj,index)

# Plan con maña

BD_MUJERES_ENDIREH2016_sample<-BD_MUJERES_ENDIREH2016_sample %>% 
  merge(index, by="id_muj", all.x = T)


# Un segundo índice -----

# Para el ámbito laboral

# Ojo está sección tiene un filtro inicial: sólo mujeres que han trabajado

index2 <-  BD_MUJERES_ENDIREH2016_sample %>% 
  filter(p7_1==1) %>% # Ojo!!!!!!! Siempre checar a quién le toca la sección
  select(id_muj, p7_9_1:p7_9_18) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(p7_9_1:p7_9_18), ~na_if(.x, 9)) %>%  # manda los 9 a missing
  dplyr::mutate_at(vars(p7_9_1:p7_9_18), ~ (2 - .x))  # cambia la lógica de la numeración a 2 se le resta el valor. Cambia sentido



# Hoy hacemos el índice
index2<- index2 %>% 
  mutate(index2= rowMeans(across(p7_9_1:p7_9_18), na.rm = T)) # Ojo aquí no dividimos porque sólo mide si se dio la situación


hist(index2$index2)

index2<- index2 %>% 
  select(id_muj, index2)



BD_MUJERES_ENDIREH2016_sample<-BD_MUJERES_ENDIREH2016_sample %>% 
  merge(index2, by="id_muj", all.x = T)

summary(BD_MUJERES_ENDIREH2016_sample$index)
summary(BD_MUJERES_ENDIREH2016_sample$index2)


# Un tercer índice.... con operaciones de matrices para los pesos. 

# Supongamos que el anterior 


index3 <-  BD_MUJERES_ENDIREH2016_sample %>% 
  filter(p7_1==1) %>% # Ojo!!!!!!! Siempre checar a quién le toca la sección
  select(id_muj, p7_9_1:p7_9_18) %>% # Vamos a hacer una base chiquita con el id y los items relevantes
  dplyr::mutate_at(vars(p7_9_1:p7_9_18), ~na_if(.x, 9)) %>%  # manda los 9 a missing
  dplyr::mutate_at(vars(p7_9_1:p7_9_18), ~ (2 - .x))  # cambia la lógica de la numeración 


# pesos del índice ---- 

# Esto es un poquito más complicado. 
# Con un poquito de álgebra matricial sale en pocos pasos
# Necesitamos un vector de pesos. ¿A qué tipos de violencia podríamos asignarles más peso?

weights<-rep(1, times=18)# Son 18 items
weights[12:14]<-2 # le damos más peso a los items 12 al 14
weights

dummy<-(weights*diag(length(weights))) # vector por identidad
dummy

index_weight<-rowSums(as.matrix(index3[,-1])%*%dummy) # Suma todos los valores
#multiplicados por el peso

index3<-cbind(index3, index_weight) #vuelve a pegar.

index3<-index3%>%
  select(id_muj, index_weight) %>% # nos quedamos con estos
  mutate(index3=index_weight/sum(weights)) # se divide entre el total de pesos


BD_MUJERES_ENDIREH2016_sample<-BD_MUJERES_ENDIREH2016_sample %>% 
  merge(index3, by="id_muj", all.x = T)


summary(BD_MUJERES_ENDIREH2016_sample$index2)
summary(BD_MUJERES_ENDIREH2016_sample$index3)


