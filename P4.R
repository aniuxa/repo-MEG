#==============================================================================
# Fecha: 2021-04-07 
# Práctica 4
# ==============================================================================


# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere

pacman::p_load(tidyverse, 
               readxl,writexl,googlesheets4, # importar hojas de cálculo
               haven, foreign, # importacón de dta y sav
               sjlabelled, GGally, RColorBrewer, # etiquetas
               janitor, skimr, # Limpieza
               esquisse, stringr) 



# Datos                   -----------------------------------------------

#rm(ej_google,subset1,subset2, subset3, subset4, tab1, tab2, tab3)

#BD_MUJERES_ENDIREH2016_sample <- read_dta("./datos/BD_MUJERES_ENDIREH2016_sample.dta")

BD_MUJERES_ENDIREH2016_sample <- read_dta("https://github.com/aniuxa/repo-MEG/raw/main/datos/BD_MUJERES_ENDIREH2016_sample.dta")
View(BD_MUJERES_ENDIREH2016_sample)

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



# Gráficos de barra                   ------------------------------------------


## Base                               ------------------------------------------


barplot(table(BD_MUJERES_ENDIREH2016_sample$sit_conyugal))

barplot(table(as_label(BD_MUJERES_ENDIREH2016_sample$sit_conyugal)))


## GGPLOT                               ----------------------------------------

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=sit_conyugal))%>% 
  + geom_bar() # simple


BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=sit_conyugal, weight=fac_muj)) %>%  # el factor de expansión
  + geom_bar()

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=as_label(sit_conyugal), weight=fac_muj)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + # proporción
  scale_y_continuous(labels=scales::percent) # porcentajes

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=as_label(sit_conyugal), weight=fac_muj)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) + coord_flip() # da la vuelta



# Juguemos con esquisse ----------------------------------------

mini<- BD_MUJERES_ENDIREH2016_sample %>%
  select(fac_muj, niv, sit_conyugal, starts_with("p13_1"), income_xx, income_xy) %>%
  mutate(sit_conyugal=as_label(sit_conyugal)) %>%
  mutate(niv=as_label(niv))

esquisse:::esquisser()

# hacer un scatter plot

# Un gráfico de medias de ingresos según situación conyugal

# Para hacer un gráfico de barra es mejor hacer un tibble

tab<-mini %>% 
  count(as_label(p13_1_1), niv, wt=fac_muj)

tab


library(dplyr)
library(ggplot2)

tab %>%
  filter(!is.na(`as_label(p13_1_1)`)) %>%
  ggplot() +
  aes(x = `as_label(p13_1_1)`, fill = niv, weight = n) +
  geom_bar() +
  scale_fill_viridis_d(option = "viridis") +
  labs(x = "¿Desde que inició la relación ...la ha empujado o le ha jalado el cabello", y = "Frecuencias", title = "Jalar el pelo", fill = "Escolaridad") +
  theme_bw() +
  theme(legend.position = "bottom")

## GGPLOT de barras con dos variables -----


BD_MUJERES_ENDIREH2016_sample %>% 
  filter(sit_conyugal<7) %>%  #filtra a las mujeres que no han tenido novio
  ggplot(aes(x=as_label(sit_conyugal), weight=fac_muj, fill=as_label(p13_1_1))) + # segunda variable va en fill
  geom_bar(aes(y = (..count..)/sum(..count..)), position="fill") + 
  labs(title="Jalar el pelo", x="Situación conyugal", y= "Porcentaje", fill="Frecuencia" )+
  scale_y_continuous(labels=scales::percent) + coord_flip() # da la vuelta


BD_MUJERES_ENDIREH2016_sample %>% 
  filter(sit_conyugal<7) %>%  #filtra a las mujeres que no han tenido novio
  ggplot(aes(x=as_label(sit_conyugal), weight=fac_muj, fill=as_label(p13_1_2))) + # segunda variable va en fill
  geom_bar(aes(y = (..count..)/sum(..count..)), position="fill") + 
  labs(title="Cachetada", x="Situación conyugal", y= "Porcentaje", fill="Frecuencia" )+
  scale_y_continuous(labels=scales::percent) + coord_flip() # da la vuelta

