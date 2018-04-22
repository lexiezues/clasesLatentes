library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(miscset)
library(poLCA)
library(LCAvarsel)
library(snow)

#setup---------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
datos<-read.csv('datos/MMSI_2016.csv')

# str(datos)
# summary(datos)

#transformaciones----------------------------------------------------------------------

datos<-datos %>%
  mutate_at(
    vars(P12_3,P12_4),
    .funs=funs(ifelse(.>3,NA,.))
  ) %>%
  mutate_at(
    vars(
      P12_1,P12_2,P12_3,P12_4,
      P10_1,P10_2,P10_3
    ),
    .funs=funs(as.factor)
  ) %>%
  filter(!is.na(P12_3)&!is.na(P12_4))



#graficas--------------------------------------------------

#seccion 12
datos %>%
  dplyr::select(P12_1,P12_2,P12_3,P12_4) %>%
  summary(10)

ggplotGrid(ncol = 2,
           lapply(c("P12_1", "P12_2", "P12_3", "P12_4"),
                  function(col) {
                    ggplot(datos, aes_string(col)) + geom_bar() + coord_flip()
                  }))

#seccion 10
datos %>%
  dplyr::select(contains('P10')) %>%
  summary(15)

ggplotGrid(ncol = 2,
           lapply(c("P10_1", "P10_2", "P10_3"),
                  function(col) {
                    ggplot(datos, aes_string(col)) + geom_bar() + coord_flip()
                  }))


#seleccion de variables---------------------------------------

datosModelo<-datos %>%
  dplyr::select(contains('P10'),contains('P12'))

fitVariables<-LCAvarsel(
  Y=datosModelo,
  G=3,
  search='backward'
)

fitVariables

fitVariables$variables



#CORRELACIONES---------------------

ggcorrplot(
  correlaciones,
  type = "lower",
  outline.color = 'white',
  ggtheme=ggplot2::theme_gray
)

