install.packages("dplyr", dependencies=TRUE)
install.packages("plyr", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("maps", dependencies=TRUE)
install.packages("usmap", dependencies=TRUE)
install.packages("mapproj", dependencies=TRUE)
install.package("devtools", dependencies=TRUE)

.libs <- c("dplyr","plyr","ggplot2","maps", "mapproj","usmap","devtools")
sapply(.libs,require, character.only=TRUE)



restaurantes <-read.csv(file.choose(),	 header=T,	 sep=',')


poblacion <-read.csv(file.choose(),	 header=T,	 sep=',')




#### FILTRAR POR ESTADO 
# Alabama (AL)

AlabamaRest <- filter(restaurants,restaurants$province == "Alabama")

# Alaska (AK)

AlaskaRest <- filter(restaurants,restaurants$province == "Alaska")

# Arizona (AZ)

ArizonaRest <- filter(restaurants,restaurants$province == "Arizona")

# Arkansas (AR)

ArkansasRest <- filter(restaurants,restaurants$province == "Arkansas")

# California (CA)

CaliforniaRest <- filter(restaurants,restaurants$province == "California")

# Colorado (CO)

ColoradoaRest <- filter(restaurants,restaurants$province == "Colorado")

# Connecticut (CT)

ConnecticutRest <- filter(restaurants,restaurants$province == "Connecticut")

# Delaware (DE)

DelawareRest <- filter(restaurants,restaurants$province == "Delaware")

# Dist of Columbia (DC)

DistColumbiaRest <- filter(restaurants,restaurants$province == "District of Columbia")

# Florida (FL)

FloridaRest <- filter(restaurants,restaurants$province == "Florida")

# Georgia (GA)

GeorgiaRest <- filter(restaurants,restaurants$province == "Georgia")

# Hawaii (HI)

HawaiiRest <- filter(restaurants,restaurants$province == "Hawaii")

# Idaho (ID)

IdahoRest <- filter(restaurants,restaurants$province == "Idaho")

# Illinois (IL)

IllinoisRest <- filter(restaurants,restaurants$province == "Illinois")

# Indiana (IN)

IndianaRest <- filter(restaurants,restaurants$province == "Indiana")

# Iowa (IA)

IowaRest <- filter(restaurants,restaurants$province == "Iowa")

# Kansas (KS)

KansasRest <- filter(restaurants,restaurants$province == "Kansas")

# Kentucky (KY)

KentuckyRest <- filter(restaurants,restaurants$province == "Kentucky")

# Louisiana (LA)

LouisianaRest <- filter(restaurants,restaurants$province == "Louisiana")

# Maine (ME)

MaineRest <- filter(restaurants,restaurants$province == "Maine")

# Maryland (MD)

MarylandRest <- filter(restaurants,restaurants$province == "Maryland")

# Massachusetts (MA)

MassachusettsRest <- filter(restaurants,restaurants$province == "Massachusetts")

# Michigan (MI)

MichiganRest <- filter(restaurants,restaurants$province == "Michigan")

# Minnesota (MN)

MinnesotaRest <- filter(restaurants,restaurants$province == "Minnesota")

# Mississippi (MS)

MississippiRest <- filter(restaurants,restaurants$province == "Mississippi")

# Missouri (MO)

MissouriRest <- filter(restaurants,restaurants$province == "Missouri")

# Montana (MT)

MontanaRest <- filter(restaurants,restaurants$province == "Montana")

# Nebraska (NE)

NebraskaRest <- filter(restaurants,restaurants$province == "Nebraska")

# Nevada (NV)

NevadaRest <- filter(restaurants,restaurants$province == "Nevada")

# New Hampshire (NH)

NewHampshireRest <- filter(restaurants,restaurants$province == "New Hampshire")

# New Jersey (NJ)

NewJerseyRest <- filter(restaurants,restaurants$province == "New Jersey")

# New Mexico (NM)

NewMexicoRest <- filter(restaurants,restaurants$province == "New Mexico")

# New York (NY)

NewYorkRest <- filter(restaurants,restaurants$province == "New York")

# North Carolina (NC)

NorthCarolinaRest <- filter(restaurants,restaurants$province == "North Carolina")

# North Dakota (ND)

NorthDakotaRest <- filter(restaurants,restaurants$province == "North Dakota")

# Ohio (OH)

OhioRest <- filter(restaurants,restaurants$province == "Ohio")

# Oklahoma (OK)

OklahomaRest <- filter(restaurants,restaurants$province == "Oklahoma")

# Oregon (OR)

OregonRest <- filter(restaurants,restaurants$province == "Oregon")

# Pennsylvania (PA)

PennsylvaniaRest <- filter(restaurants,restaurants$province == "Pennsylvania")

# Rhode Island (RI)

RhodeIslandRest <- filter(restaurants,restaurants$province == "Rhode Island")

# South Carolina (SC)

SouthCarolinaRest <- filter(restaurants,restaurants$province == "South Carolina")

# South Dakota (SD)

SouthDakotaRest <- filter(restaurants,restaurants$province == "South Dakota")

# Tennessee (TN)

TennesseeRest <- filter(restaurants,restaurants$province == "Tennessee")

# Texas (TX)

TexasRest <- filter(restaurants,restaurants$province == "Texas")

# Utah (UT)

UtahRest <- filter(restaurants,restaurants$province == "Utah")

# Vermont (VT)

VermontRest <- filter(restaurants,restaurants$province == "Vermont")

# Virginia (VA)

VirginiaRest <- filter(restaurants,restaurants$province == "Virginia")

# Washington (WA)

WashingtonRest <- filter(restaurants,restaurants$province == "Washington")

# West Virginia (WV)

WestVirginiaRest <- filter(restaurants,restaurants$province == "West Virginia")

# Wisconsin (WI)

WisconsinRest <- filter(restaurants,restaurants$province == "Wisconsin")

# Wyoming (WY)

WyomingRest <- filter(restaurants,restaurants$province == "Wyoming")



###### MAPA DE TODO USA ############################

mapa_usa<-map_data('world', region = c("Usa", "Alaska", "Hawaii"), xlim =c(-200,-50))

mapa_usa %>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group),
               fill = "lightgray",
               color = "white",
               size = 0.01) +
  geom_point(data= restaurantes, 
             aes(x=longitude, y = latitude), 
             stroke = F, 
             alpha =0.2, 
             color = "blue")+ 
  ggtitle( "Restaurantes de comida rápida en EEUU")


################## MAPA POR ESTADO #######################
vignette(package = "usmap")
vignette("introduction", package = "usmap")
vignette("mapping", package = "usmap")
vignette("advanced-mapping", package = "usmap")

##state_map <- us_map(regions = "states")


### cambiar orden columnas POPULATION para transformar
colnames(poblacion)
poblacion <- poblacion[ , c(4,3,1,2,5,6,7,8,9,10,11)]
head(poblacion)

population_usa<- usmap_transform(poblacion)

#### grafica mapa punto cantidad restaurante
plot_usmap() +
  geom_point(data = population_usa,
             aes(x = long.1 , y = lat.1, size = nRestaurants),
             color = "purple", alpha = 0.5) + theme(legend.position = "right")


plot_usmap() +
  geom_point(data = population_usa,
             aes(x = long.1 , y = lat.1, size = nRestaurants),
             color = "purple", alpha = 0.5) + theme(legend.position = "right")
scale_fill_continuous(
  low = "white", high = "red", name = "Nº de restaurantes") 


