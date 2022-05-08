library(tidyverse)
library(worldmet)
library(lubridate)
library(ggplot2)
if(!require(devtools)) {install.packages("devtools"); require(devtools)}
# instalcja pakietu gios_import
#devtools::install_github("mrzeszut/giosimport")
library(giosimport)
## sprawdźmy które stacje są najbliżej wybranej stacji jakości powietrza

noaa_isd <- getMeta(end.year="current", lon=18.625355, lat=54.359474, returnMap=T)

gdansk_met <- importNOAA(code="121420-99999", year=2010:2020)

summary(gdansk_met)
getwd()
kat_dost <- "F:/Studia/Semestr IV/przetwarzanie_danych/Rzeszut/proj 1"
setwd(kat_dost)

meta <- gios_metadane(type = "stacje", 
                      download = T,    # zmień na T, jeśli uruchamiasz piewszy raz
                      path = kat_dost, 
                      mode = "wb")


stacje <- gios_metadane(type = "stacje", 
                        download = T,   
                        path = kat_dost, 
                        mode = "wb")

#View(stacje)

# tylko aktywne stacje
gios_vis(data = stacje %>% filter(is.na(data.zamkniecia)))

#wczytanie stanowisk
stanowiska <- gios_metadane(type = "stanowiska", 
                            download = T,  
                            path = kat_dost, 
                            mode = "wb")


statystyki <- gios_metadane(type = "statystyki", ### nie dzila
                            download = T, 
                            path = kat_dost, 
                            mode = "wb")

pliki_all <- map2(.x = as.list(zrodlo[,1]), 
                  .y = as.list(zrodlo[,2]), 
                  .f = gios_download, 
                  path = kat_dost,
                  mode = "wb")
names(pliki_all) <- paste0("R",zrodlo[,2])
names(pliki_all)
pliki_all
pliki_all$R2020<-list.files(path = "D:/Studia/Semestr IV/przetwarzanie_danych/Rzeszut/proj 1/2020", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

inp_so2 <- map(.x = pliki_all, 
                  .f = ~ .[str_detect(., pattern = "SO2_1g")]) %>% 
  flatten_chr()

inp_so2<-inp_so2[-c(1:10)] #wybór tylko 10lat wstecz 2011-2020

so2_10_lat_all <- map_df(.x = inp_so2, 
               .f = gios_read, 
               czas_mu = "1g", 
               path = kat_dost)

summary(so2_10_lat_all)

ind =str_detect(so2_10_lat_all$kod, "PmGdaPoWie01") | str_detect(so2_10_lat_all$kod, "Pm.a01a") #stary i nowy kod
so2_gd<-filter(so2_10_lat_all,ind) #wybrana stacja gdansk 	PL0045A, 	Gdańsk, ul. Powstańców Warszawskich

so2_gdn <- gios_kody(data = so2_gd, meta = meta) #ujednoslicenie nazw kodow
so2_gd<-so2_gdn
rm(so2_gdn)
### laczenie danych wmisyjnych i meteo
### dane o jak. pow - UTC+01, dane meteo- UTC+00
str(gdansk_met)
gdansk_met$date <- gdansk_met$date + 3600


dt <- inner_join(gdansk_met, so2_gd, by = "date")

view(dt)

#ostatecznie


