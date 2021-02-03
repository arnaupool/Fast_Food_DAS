install.packages("dplyr", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("maps", dependencies=TRUE)
install.packages("usmap", dependencies=TRUE)
install.packages("mapproj", dependencies=TRUE)
install.package("devtools", dependencies=TRUE)

.libs <- c("dplyr","ggplot2","maps", "mapproj","usmap","devtools")
sapply(.libs,require, character.only=TRUE)


#### ARXIU Datafiniti_Fast_Food_Restaurants_May19

restaurants <-read.csv(file.choose(),	 header=T,	 sep=',')
head(restaurants)
colnames(restaurants)

### quitando datos innecesarios
## id, dateAdded, dateUpdated, keys, sourceURLs, websites, country
### 
restaurants <- subset(restaurants,select = - c(1,2,3,8,9,15,16))
head(restaurants)
colnames(restaurants)
# "address"           "categories"        "primaryCategories" "city"              "latitude"         
# "longitude"         "name"              "postalCode"        "province"


### CONVERTIR A FACTOR  PROVINCE, CATEGORIES 

sapply(restaurants, class)

restaurants$city <- factor(restaurants$city)
restaurants$province <- factor(restaurants$province)
restaurants$categories <- factor(restaurants$categories)
restaurants$primaryCategories <- factor(restaurants$primaryCategories)

### NO HI HAN DUPLICATS
prueba <- restaurants[!duplicated(restaurants),]
head(prueba)

### NO HI HA NA



###### MAPA DE TODO USA ############################

mapa_usa<-c(map_data('usa'), map_data(''), mapd_data(''))

mapa_usa %>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group),
               fill = "blue",
               color = "white") +
geom_point(data= restaurants, 
           aes(x=longitude, y = latitude), 
           stroke = F) +   ##stroke to modify the  width of the border
  scale_size_continuous(name = "Kg") +
  ggtitle( "Restaurantes")


theme_map()



################## MAPA POR ESTADO #######################
vignette("mapping", package = "usmap")

state_map <- us_map(regions = "states")
plot_usmap("states")  +
  geom_point(data= restaurants, 
             aes(x=longitude, y = latitude), 
             stroke = F) +   ##stroke to modify the  width of the border
  scale_size_continuous(name = "Kg") +
  ggtitle( "Restaurantes")

### BUSCAR CORRELACIÓN ENTRE POBLACIÓN Y CANTIDAD DE RESTAURANTES

restaurants$province[restaurants$province == "AL"] <- "Alabama"
restaurants$province[restaurants$province == "AK"] <- "Alaska"
restaurants$province[restaurants$province == "AB"] <- "Alberta "
restaurants$province[restaurants$province == "AZ"] <- "Arizona "
restaurants$province[restaurants$province == "AR"] <- "Arkansas "
restaurants$province[restaurants$province == "BC"] <- "British Columbia"


#### FILTRAR POR ESTADO 
# Alabama (AL)

AlabamaRest <- filter(restaurants,restaurants$province == "Alabama")

# Alaska (AK)

AlaskaRest <- filter(restaurants,restaurants$province == "Alaska")

# Alberta (AB)

AlbertaRest <- filter(restaurants,restaurants$province == "Alberta")

# Arizona (AZ)

ArizonaRest <- filter(restaurants,restaurants$province == "Arizona")

# Arkansas (AR)

ArkansasRest <- filter(restaurants,restaurants$province == "Arkansas")

# British Columbia (BC)

BritishColumbiaRest <- filter(restaurants,restaurants$province == "British Columbia")

# California (CA)

CaliforniaRest <- filter(restaurants,restaurants$province == "California")

# Colorado (CO)

ColoradoaRest <- filter(restaurants,restaurants$province == "Colorado")

# Connecticut (CT)

ConnecticutRest <- filter(restaurants,restaurants$province == "Connecticut")

# Delaware (DE)

DelawareRest <- filter(restaurants,restaurants$province == "Delaware")

# Dist of Columbia (DC)

DistColumbiaRest <- filter(restaurants,restaurants$province == "Dist of Columbia")

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

# Manitoba (MB)

ManitobaRest <- filter(restaurants,restaurants$province == "Manitoba")

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

# New Brunswick (NB)

NewBrunswickRest <- filter(restaurants,restaurants$province == "New Brunswick")

# New Hampshire (NH)

NewHampshireRest <- filter(restaurants,restaurants$province == "New Hampshire")

# New Jersey (NJ)

NewJerseyRest <- filter(restaurants,restaurants$province == "New Jersey")

# New Mexico (NM)

NewMexicoRest <- filter(restaurants,restaurants$province == "New Mexico")

# New York (NY)

NewYorkRest <- filter(restaurants,restaurants$province == "New York")

# NewFoundland/Labrador (NL)

NewFoundlandRest <- filter(restaurants,restaurants$province == "NewFoundland/Labrador")

# North Carolina (NC)

NorthCarolinaRest <- filter(restaurants,restaurants$province == "North Carolina")

# North Dakota (ND)

NorthDakotaRest <- filter(restaurants,restaurants$province == "North Dakota")

# Northwest Territories (NT)

NorthwestTerritoriesRest <- filter(restaurants,restaurants$province == "Northwest Territories")

# Nova Scotia (NS)

NovaScotiaRest <- filter(restaurants,restaurants$province == "Nova Scotia")

# Ohio (OH)

OhioRest <- filter(restaurants,restaurants$province == "Ohio")

# Oklahoma (OK)

OklahomaRest <- filter(restaurants,restaurants$province == "Oklahoma")

# Ontario (ON)

OntarioRest <- filter(restaurants,restaurants$province == "Ontario")

# Oregon (OR)

OregonRest <- filter(restaurants,restaurants$province == "Oregon")

# Pennsylvania (PA)

PennsylvaniaRest <- filter(restaurants,restaurants$province == "Pennsylvania")

# Price Edward Island (PE)

PriceEdwardIslandRest <- filter(restaurants,restaurants$province == "Price Edward Island")

# Quebec (QC)

QuebecRest <- filter(restaurants,restaurants$province == "Quebec")

# Rhode Island (RI)

RhodeIslandRest <- filter(restaurants,restaurants$province == "Rhode Island")

# Saskatchewan (SK)

SaskatchewanRest <- filter(restaurants,restaurants$province == "Saskatchewan")

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

# Yukon (YT)

YukonRest <- filter(restaurants,restaurants$province == "Yukon")

restaurante_states<-c(nrow(AlabamaRest),nrow(AlaskaRest),nrow(AlbertaRest), nrow(ArizonaRest),nrow(ArkansasRest),nrow(BritishColumbiaRest), nrow(CaliforniaRest), nrow(ColoradoaRest),nrow(ConnecticutRest),nrow(DelawareRest),nrow(DistColumbiaRest),nrow(FloridaRest),nrow(GeorgiaRest),nrow(HawaiiRest),nrow(IdahoRest),nrow(IllinoisRest),nrow(IndianaRest),nrow(IowaRest),nrow(KansasRest), 
nrow(KentuckyRest),nrow(LouisianaRest),nrow(MaineRest),nrow(ManitobaRest),nrow(MarylandRest),nrow(MassachusettsRest),nrow(MichiganRest),nrow(MinnesotaRest),
nrow(MississippiRest),nrow(MissouriRest),nrow(MontanaRest),nrow(NebraskaRest),nrow(NevadaRest),nrow(NewBrunswickRest),nrow(NewHampshireRest),
nrow(NewJerseyRest),nrow(NewMexicoRest),nrow(NewYorkRest),nrow(NewFoundlandRest),nrow(NorthCarolinaRest),nrow(NorthDakotaRest),
nrow(NorthwestTerritoriesRest),nrow(NovaScotiaRest),nrow(OhioRest),nrow(OklahomaRest),nrow(OntarioRest), 
nrow(OregonRest),nrow(PennsylvaniaRest),nrow(PriceEdwardIslandRest),nrow(QuebecRest),nrow(RhodeIslandRest),
nrow(SaskatchewanRest),nrow(SouthCarolinaRest),nrow(SouthDakotaRest),nrow(TennesseeRest),nrow(TexasRest),nrow(UtahRest), 
nrow(VermontRest),nrow(VirginiaRest),nrow(WashingtonRest),nrow(WestVirginiaRest),nrow(WisconsinRest),nrow(WyomingRest),nrow(YukonRest))

 

#### FILTRAR POR TIPO DE RESTAURANTES

