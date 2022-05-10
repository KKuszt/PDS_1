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
#kat_dost <- "F:/Studia/Semestr IV/przetwarzanie_danych/Rzeszut/proj 1"
#kat_dost <- "C:/Users/48506/Documents/Studia/Studia II rok/Semestr4/PDS/projekt_zaliczenie/PDS_1"
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


statystyki <- gios_metadane(typ e = "statystyki", ### nie dzila
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

#@@@@@@@@@@@@@@@@@@@@

#POMYSLY ZIEMIANA:
#-wyczyscic dt, bo jest duzo kolumn, ktorych nie potrzebujemy, np:
#kod, stacja, lat, lon
#no i chyba te wszystkie z chmurami zwiazane ale nie jesyem pewny
#i chyba substancje tez mozemy bo wiemy co to jest obs 

#@@@ UŚREDNIENIE DANYCH DO MIESIĄCA @@@
#usrednienia dokonujemy funkcją timeAverage z pakietu openair
library(openair)
dt_miesiac <- dt %>% openair::timeAverage(avg.time="month")
#wartości NA zniknęły więc nic nie trzeba uzupełniać
#w jednej kolumnie jest kilka wartośći NaN - nie wiem ocb

#konwersja na obiekt tsibble
library(tsibble)
dt_ts <- dt_miesiac %>% 
  mutate(date = as.Date(date)) %>% 
  as_tsibble(index = date)

#wyrzucam te kolumny ktore nas nie interesują
#tu mozemy zmienic co faktycznie chcemy miec w ttch danych, wzialem te kolumny ktore uwazalem ze moga
#sie przydac ale nie wiem w sumie co to ceil_hgt
dt_ts <- dt_ts %>%
  select(date, obs, ws, wd, air_temp, atmos_pres, visibility, dew_point, RH, ceil_hgt)

library(feasts)
#ogolnie nie da sie np. zrobic wykresu bo on chce dane CODZIENNE
#wiec usrednienie do miesiecy tutaj pierdoli sprawe
#fill_gaps jak odpalisz to ci pokaze, że w kazde inne dni niz 1 dzien miesiaca
#wszystko sie wypelnia na XD

#zeby zrobic wykresy musi byc obiekt tsibble, ale to nie dziala
dt_ts %>% gg_season(obs)

#zeby zrobic modele musi byc konwersja na time series
dt_ts %>% as.ts() -> dt_ts

install.packages("forecast")
library(forecast)

# Opracujemy model regresji liniowej
fit <- tslm(obs ~ air_temp, data=dt_ts)  # to samo
fit

#nie wiem czy ten model jest dobry XD

library(GGally)
dt_ts %>%
  as.data.frame() %>%
  ggally_smooth_lm(aes(x=air_temp, y=obs), 
                   se = F, 
                   color = "blue") 
#Warning messages:
#1: Removed 3503 rows containing non-finite values (stat_smooth). 
#2: Removed 3503 rows containing missing values (geom_point). 

checkresiduals(fit)
#Error in `check_aesthetics()`:
#! Aesthetics must be either length 1 or the same as the data (1): xend, yend and x

dt_ts %>%
  as.data.frame() %>%
  GGally::ggpairs()
# Dwie zmienne mają ograniczone zastosowanie
# Wykonamy model regresji wielorakiej 

fit <- tslm(obs ~ air_temp + ws + atmos_pres + RH + visibility,
            data=dt_ts)

summary(fit)

cor(x = dt_ts[,'obs'] %>% as.vector(), y = fitted(fit) %>% as.vector())
summary(fit)$sigma
summary(fit)$r.squared
summary(fit)$adj.r.squared

#wiecej nie zdazylem ide na ang