  #==============================================================================
  # Fecha: 2021-04-07 
  # Práctica 4
  # ==============================================================================


# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere

pacman::p_load(tidyverse, 
               readxl,writexl,googlesheets4, # importar hojas de cÃ¡lculo
               haven, foreign, # importacón de dta y sav
               sjlabelled, GGally, RColorBrewer, # etiquetas
               janitor, skimr, # Limpieza
               esquisse, stringr) 



# Datos                   -----------------------------------------------

setwd("C:/Users/anaes/Dropbox/2021/MEG/repo-MEG")

BD_MUJERES_ENDIREH2016_sample <- read_dta("./datos/BD_MUJERES_ENDIREH2016_sample.dta")

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
    dplyr::mutate_at(vars(p13_1_1:p13_1_22), ~set_labels(.x,labels=freq.shortlab))

# Gráficos de barra                   ------------------------------------------


## Base                               ------------------------------------------


barplot(table(BD_MUJERES_ENDIREH2016_sample$sit_conyugal))

barplot(table(as_label(BD_MUJERES_ENDIREH2016_sample$sit_conyugal)))


## GGPLOT                               ----------------------------------------

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=sit_conyugal)) %>% 
           + geom_bar()


BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=sit_conyugal, weight=fac_muj)) %>% 
  + geom_bar()

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=as_label(sit_conyugal), weight=fac_muj)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent)

BD_MUJERES_ENDIREH2016_sample %>% 
  ggplot(aes(x=as_label(sit_conyugal), weight=fac_muj)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) + coord_flip()



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
  count(as_label(p13_1_1),niv, wt=fac_muj)

tab 
