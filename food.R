install.packages("dplyr", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("maps", dependencies=TRUE)
install.packages("usmap", dependencies=TRUE)
install.packages("mapproj", dependencies=TRUE)
install.package("devtools", dependencies=TRUE)

.libs <- c("dplyr","ggplot2","maps", "mapproj","usmap","devtools")
sapply(.libs,require, character.only=TRUE)


#### ARCHIVO Datafiniti_Fast_Food_Restaurants_May19

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


#### REFACTORIZACIÓN DE DATOS para juntar con los datos de población

restaurants$province[restaurants$province == "AL"] <- "Alabama"
restaurants$province[restaurants$province == "AK"] <- "Alaska"
restaurants$province[restaurants$province == "AZ"] <- "Arizona"
restaurants$province[restaurants$province == "AR"] <- "Arkansas"
restaurants$province[restaurants$province == "CA"] <- "California"
restaurants$province[restaurants$province == "CO"] <- "Colorado"
restaurants$province[restaurants$province == "CT"] <- "Connecticut"
restaurants$province[restaurants$province == "DE"] <- "Delaware"
restaurants$province[restaurants$province == "DC"] <- "District of Columbia"
restaurants$province[restaurants$province == "FL"] <- "Florida"
restaurants$province[restaurants$province == "GA"] <- "Georgia"
restaurants$province[restaurants$province == "HI"] <- "Hawaii"
restaurants$province[restaurants$province == "ID"] <- "Idaho"
restaurants$province[restaurants$province == "IL"] <- "Illinois"
restaurants$province[restaurants$province == "IN"] <- "Indiana"
restaurants$province[restaurants$province == "IA"] <- "Iowa"
restaurants$province[restaurants$province == "KS"] <- "Kansas"
restaurants$province[restaurants$province == "KY"] <- "Kentucky"
restaurants$province[restaurants$province == "LA"] <- "Louisiana"
restaurants$province[restaurants$province == "ME"] <- "Maine"
restaurants$province[restaurants$province == "MD"] <- "Maryland"
restaurants$province[restaurants$province == "MA"] <- "Massachusetts"
restaurants$province[restaurants$province == "MI"] <- "Michigan"
restaurants$province[restaurants$province == "MN"] <- "Minnesota"
restaurants$province[restaurants$province == "MS"] <- "Mississippi"
restaurants$province[restaurants$province == "MO"] <- "Missouri"
restaurants$province[restaurants$province == "MT"] <- "Montana"
restaurants$province[restaurants$province == "NE"] <- "Nebraska"
restaurants$province[restaurants$province == "NV"] <- "Nevada"
restaurants$province[restaurants$province == "NH"] <- "New Hampshire"
restaurants$province[restaurants$province == "NJ"] <- "New Jersey"
restaurants$province[restaurants$province == "NM"] <- "New Mexico"
restaurants$province[restaurants$province == "NY"] <- "New York"
restaurants$province[restaurants$province == "NC"] <- "North Carolina"
restaurants$province[restaurants$province == "ND"] <- "North Dakota"
restaurants$province[restaurants$province == "OH"] <- "Ohio"
restaurants$province[restaurants$province == "OK"] <- "Oklahoma"
restaurants$province[restaurants$province == "OR"] <- "Oregon"
restaurants$province[restaurants$province == "PA"] <- "Pennsylvania"
restaurants$province[restaurants$province == "RI"] <- "Rhode Island"
restaurants$province[restaurants$province == "SC"] <- "South Carolina"
restaurants$province[restaurants$province == "SD"] <- "South Dakota"
restaurants$province[restaurants$province == "TN"] <- "Tennessee"
restaurants$province[restaurants$province == "TX"] <- "Texas"
restaurants$province[restaurants$province == "UT"] <- "Utah"
restaurants$province[restaurants$province == "VT"] <- "Vermont"
restaurants$province[restaurants$province == "VA"] <- "Virginia"
restaurants$province[restaurants$province == "WA"] <- "Washington"
restaurants$province[restaurants$province == "WV"] <- "West Virginia"
restaurants$province[restaurants$province == "WI"] <- "Wisconsin"
restaurants$province[restaurants$province == "WY"] <- "Wyoming"

head(restaurants,20)



### NO HI HAN DUPLICATS
prueba <- restaurants[!duplicated(restaurants),]
head(prueba)

### NO HI HA NA


###################################
###################################
###################################
###################################
###################################
###################################
###################################
###################################
###################################


## TRANSFORMACIÓN NOMBRES

nombresRest<-data.frame(table(restaurants$name))

BurguerShack<-c("Al's Burger Shack","Al's Burger Shack-Southern Village")
restaurants$name[restaurants$name %in% BurguerShack] <- "Al's Burger Shack"

Albee<-c("Albee's Ny Gyros","Albee's NY Gyros")
restaurants$name[restaurants$name %in% Albee] <- "Albee's NY Gyros"

Alfonso<-c("Alfonso's Pizza Italian Restaurant","Alfonsos Pizza")
restaurants$name[restaurants$name %in% Alfonso] <- "Alfonso's Pizza"

#BORRAR Arby's cerrados o por abrir
restaurants<-restaurants[!grepl("Arby's - Closed", restaurants$name),]
restaurants<-restaurants[!grepl("Arby's - Coming Soon", restaurants$name),]
restaurants<-restaurants[!grepl("Arbys CLOSED", restaurants$name),]

Arby<-c("Arby's","Arbys")
restaurants$name[restaurants$name %in% Arby] <- "Arby's"

Auntie<-c("Auntie Anne's","Auntie Anne's Soft Pretzels")
restaurants$name[restaurants$name %in% Auntie] <- "Auntie Anne's Soft Pretzels"

AW<-c("AW","AW All-American Food","AW Restaurant","Aw Restaurants","AW Restaurants","AWRestaurants","A W Restaurant")
restaurants$name[restaurants$name %in% AW] <- "A&W All-American Food"

BackYard<-c("Back Yard Burgers","Backyard Burgers")
restaurants$name[restaurants$name %in% BackYard] <- "Back Yard Burgers"

BadDaddy<-c("Bad Daddy's Burger Bar","Bad Daddys Burger Bar")
restaurants$name[restaurants$name %in% BadDaddy] <- "Bad Daddy's Burger Bar"

BR<-c("Baskin-Robbins","Baskin Robbins","BaskinRobbins")
restaurants$name[restaurants$name %in% BR] <- "Baskin Robbins"

BigBilly<-c("Big Billy's Burger Joint","Big Billys Burger Joint")
restaurants$name[restaurants$name %in% BigBilly] <- "Big Billy's"

BigBoy<-c("Big Boy","Big Boy's Burgers and Shakes","Big Boy's Construction, Inc.","Big Boy Burger/Express Market","Big Boys","Big Boys Construction")
restaurants$name[restaurants$name %in% BigBoy] <- "Big Boy"

BlakeL<-c("Blake's Lotaburger","Blakes Lotaburger")
restaurants$name[restaurants$name %in% BlakeL] <- "Blake's Lotaburger"

Blimpie<-c("Blimpie","BLIMPIE","Blimpie Subs & Salads")
restaurants$name[restaurants$name %in% Blimpie] <- "Blimpie"

BobBurger<-c("Bob's Burger Brew","Bob's Burgers Brew")
restaurants$name[restaurants$name %in% BobBurger] <- "Bob's Burgers Brew"

BobEvans<-c("Bob Evans","Bob Evans Restaurant")
restaurants$name[restaurants$name %in% BobEvans] <- "Bob Evans"

Bojangles<-c("Bojangles","Bojangles' Famous Chicken 'n Biscuits","Bojangles Famous Chicken n Biscuits")
restaurants$name[restaurants$name %in% Bojangles] <- "Bojangles"

Braum<-c("Braum's Ice Cream and Dairy Store","Braum's Ice Cream Burger Restaurant")
restaurants$name[restaurants$name %in% Braum] <- "Braum's"

##BORRAR Burguer Kings cerrados
restaurants<-restaurants[!grepl("Burger King - Closed", restaurants$name),]
restaurants<-restaurants[!grepl("Burger King - Temporarily Closed", restaurants$name),]

BurgerKing<-c("Burger King","Burger KingÂ®","Burger KingPopeyes")
restaurants$name[restaurants$name %in% BurgerKing] <- "Burger King"

BurgerRanch<-c("Burger Ranch","Burger Ranch Of Selah")
restaurants$name[restaurants$name %in% BurgerRanch] <- "Burger Ranch"

BurgerFi<-c("Burgerfi","BurgerFi")
restaurants$name[restaurants$name %in% BurgerFi] <- "BurgerFi"

Burrito<-c("Burrito Amigos","Burrtio Amigos")
restaurants$name[restaurants$name %in% Burrito] <- "Burrito Amigos"

CJ<-c("C & J Drive In","C J Drive In")
restaurants$name[restaurants$name %in% CJ] <- "C & J Drive In"

CafeY<-c("Cafe Yumm","Cafe Yumm - River Bend")
restaurants$name[restaurants$name %in% CafeY] <- "Cafe Yumm"

Captain<-c("Captain D'S","Captain D's Seafood","Captain D's Seafood Kitchen","Captain Ds")
restaurants$name[restaurants$name %in% Captain] <- "Captain D'S"

Carl<-c("Carl's Jr","Carl's Jr.","Carls Jr","Carls Jr Green Burrito")
restaurants$name[restaurants$name %in% Carl] <- "Carl's Jr"

Chanellos<-c("Chanelloâ€™s Pizza","Chanellos Pizza")
restaurants$name[restaurants$name %in% Chanellos] <- "Chanellos Pizza"

Char<-c("Char-Grill","Char Grill 1")
restaurants$name[restaurants$name %in% Char] <- "Char Grill"

Charleys<-c("Charley's Grill & Spirits","Charley's Grill Spirits")
restaurants$name[restaurants$name %in% Charleys] <- "Charley's Grill & Spirits"

Charley<-c("Charley's Grilled Subs","Charleys Grilled Subs")
restaurants$name[restaurants$name %in% Charley] <- "Charley's Grilled Subs"

Checker<-c("Checker's Drive In Restaurant","Checkers","Checkers Drive-In Rstrnt Inc")
restaurants$name[restaurants$name %in% Checker] <- "Checker's"

Chick<-c("Chick-fil-A","Chick-Fil-A","Chick-Fil-A Of Randolph Mall","ChickfilA")
restaurants$name[restaurants$name %in% Chick] <- "Chick-Fil-A"

ChickenE<-c("Chicken Express","Chicken Express of Conway")
restaurants$name[restaurants$name %in% ChickenE] <- "Chicken Express"

ChinaJ<-c("China Jade","China Jade Chinese Take Out")
restaurants$name[restaurants$name %in% ChinaJ] <- "China Jade"

ChinaStar<-c("China Star","China Star Chinese Restaurant")
restaurants$name[restaurants$name %in% ChinaStar] <- "China Star"

ChinaWok<-c("China Wok","China Wok Chinese Takeout","China Wok Express")
restaurants$name[restaurants$name %in% ChinaWok] <- "China Wok"

Chipotle<-c("Chipotle","Chipotle Mexican Grill")
restaurants$name[restaurants$name %in% Chipotle] <- "Chipotle"

Church<-c("Church's Chicken","Church's Chicken Of Birmingham","Churchs Chicken")
restaurants$name[restaurants$name %in% Church] <- "Church's Chicken"

Cisco<-c("Cisco's Taqueria","Ciscos Taqueria")
restaurants$name[restaurants$name %in% Cisco] <- "Cisco's Taqueria"

CookO<-c("Cook-Out","Cook Out","CookOut")
restaurants$name[restaurants$name %in% CookO] <- "Cook-Out"

Culver<-c("Culver's","CULVER'S","Culvers")
restaurants$name[restaurants$name %in% Culver] <- "Culver's"

##BORRAR Dairy Queen cerrados
restaurants<-restaurants[!grepl("Dairy Queen - Closed", restaurants$name),]

Dairy<-c("Dairy queen","Dairy Queen","Dairy Queen-Russo","Dairy Queen (Treat)","Dairy Queen (Treat) - Seasonally Closed","Dairy Queen Grill & Chill",
"Dairy Queen Grill and Chill","Dairy Queen Grill Chill","Dairy Queen Grill Chill - Temporarily Closed","Dairy Queen Orange Julius")
restaurants$name[restaurants$name %in% Dairy] <- "Dairy Queen"

Dominic<-c("Dominic's of New York","Dominics of New York")
restaurants$name[restaurants$name %in% Dominic] <- "Dominic's of New York"

Domino<-c("Domino's Pizza","Dominos Pizza")
restaurants$name[restaurants$name %in% Domino] <- "Domino's Pizza"

Dq<-c("DQ Grill Chill","DQ Grill Chill Orange Julius")
restaurants$name[restaurants$name %in% Dq] <- "DQ Grill Chill"

Dunkin<-c("Dunkin'","Dunkin' Donuts","Dunkin Donuts","Dunkin Donuts/baskinrobbins")
restaurants$name[restaurants$name %in% Dunkin] <- "Dunkin' Donuts"

##BORRAR Einstein Bros cerrados
restaurants<-restaurants[!grepl("Einstein Bros. Bagels - Closed", restaurants$name),]

Einstein<-c("Einstein Bros Bagels","Einstein Bros. Bagels")
restaurants$name[restaurants$name %in% Einstein] <- "Einstein Bros Bagels"

Tapatio<-c("El Tapatio","El Tapatio Restaurante")
restaurants$name[restaurants$name %in% Tapatio] <- "El Tapatio"

Emidio<-c("Emidio & Sons Italian Restaurant","Emidio Sons Italian Restaurant")
restaurants$name[restaurants$name %in% Emidio] <- "Emidio & Sons Italian Restaurant"

Dave<-c("Famous Dave's","Famous Daves")
restaurants$name[restaurants$name %in% Dave] <- "Famous Dave's"

Farlow<-c("Farlow's On The Water","Farlows on the Water")
restaurants$name[restaurants$name %in% Farlow] <- "Farlow's On The Water"

Fazoli<-c("Fazoli's","Fazolis") 
restaurants$name[restaurants$name %in% Fazoli] <- "Fazoli's"

Fireplace<-c("Fireplace Restaurant & Lounge","Fireplace Restaurant Lounge")
restaurants$name[restaurants$name %in% Fireplace] <- "Fireplace Restaurant & Lounge"

FiveGuys<-c("Five Guys","Five Guys Burgers & Fries","Five Guys Burgers Fries")
restaurants$name[restaurants$name %in% FiveGuys] <- "Five Guys"

Fox<-c("Fox's Pizza Den","Foxs Pizza Den")
restaurants$name[restaurants$name %in% Fox] <- "Fox's Pizza Den"

Freddy<-c("Freddy's Frozen Custard & Steakburgers","Freddy's Frozen Custard Steakburgers","Freddys Frozen Custard Steakburgers") 
restaurants$name[restaurants$name %in% Freddy] <- "Freddy's Frozen Custard & Steakburgers"

Frisch<-c("Frisch's Big Boy","Frisch's Big Boy Restaurant","90 Frisch's Big Boy Restaurant","1 Frisch's Big Boy Restaurant","40 Frisch's Big Boy Restaurant") 
restaurants$name[restaurants$name %in% Frisch] <- "Frisch's Big Boy"

Full<-c("Full Moon Bar-B-Que","Full Moon Bar B Que")
restaurants$name[restaurants$name %in% Full] <- "Full Moon Bar-B-Que"

George<-c("George's Gyros Spot","George's Gyros Spot 2")
restaurants$name[restaurants$name %in% George] <- "George's Gyros Spot"

Gold<-c("Gold Star Chili","Gold Star Hamburgers")
restaurants$name[restaurants$name %in% Gold] <- "Gold Star"

Golden<-c("Golden Palace","Golden Palace Restaurant")
restaurants$name[restaurants$name %in% Golden] <- "Golden Palace"

McAlister<- c("Mcalister's Deli","McAlister's Deli","McAlisters Deli") 
restaurants$name[restaurants$name %in% McAlister] <- "McAlister's Deli"

##BORRAR Mcdonalds cerrados
restaurants<-restaurants[!grepl("McDonald's - CLOSED", restaurants$name),]

McDonald <- c("McDonald's", "Mcdonald's", "Mc Donald's","McDonalds","Mcdonalds", "McDonalds Family Restaurant")
restaurants$name[restaurants$name %in% McDonald] <- "McDonald's"



################################### en procesooooooooooooooooooooooo
###################################
###################################
###################################
###################################
###################################
###################################
###################################


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



nRestaurants<-c(nrow(AlabamaRest),nrow(AlaskaRest), nrow(ArizonaRest),nrow(ArkansasRest),nrow(CaliforniaRest), 
                      nrow(ColoradoaRest),nrow(ConnecticutRest),nrow(DelawareRest),nrow(DistColumbiaRest),nrow(FloridaRest),nrow(GeorgiaRest),
                      nrow(HawaiiRest),nrow(IdahoRest),nrow(IllinoisRest),nrow(IndianaRest),nrow(IowaRest),nrow(KansasRest), 
                      nrow(KentuckyRest),nrow(LouisianaRest),nrow(MaineRest),nrow(MarylandRest),nrow(MassachusettsRest),
                      nrow(MichiganRest),nrow(MinnesotaRest),nrow(MississippiRest),nrow(MissouriRest),nrow(MontanaRest),
                      nrow(NebraskaRest),nrow(NevadaRest),nrow(NewHampshireRest),nrow(NewJerseyRest),nrow(NewMexicoRest),nrow(NewYorkRest),nrow(NorthCarolinaRest),nrow(NorthDakotaRest),
                      nrow(OhioRest),nrow(OklahomaRest),nrow(OregonRest),nrow(PennsylvaniaRest),nrow(RhodeIslandRest),
                      nrow(SouthCarolinaRest),nrow(SouthDakotaRest),nrow(TennesseeRest),nrow(TexasRest),nrow(UtahRest), 
                      nrow(VermontRest),nrow(VirginiaRest),nrow(WashingtonRest),nrow(WestVirginiaRest),nrow(WisconsinRest),nrow(WyomingRest))



### CONVERTIR A FACTOR  PROVINCE, CATEGORIES 

sapply(restaurants, class)

restaurants$city <- factor(restaurants$city)
restaurants$province <- factor(restaurants$province)
restaurants$categories <- factor(restaurants$categories)
restaurants$primaryCategories <- factor(restaurants$primaryCategories)


################## DATOS POBLACION ######################################  

population <-read.csv(file.choose(),	 header=T,	 sep=',')
head(population)

#### COMBINAR DATOS 
population <- cbind(population,nRestaurants)

sapply(population, class)
head(population)
# STATE POPESTIMATE2019      lat       long nRestaurants
# 1    Alabama         4903185 32.37772  -86.30057          635
# 2     Alaska          731545 58.30160 -134.42021           64
# 3    Arizona         7278717 33.44814 -112.09696          186
# 4   Arkansas         3017804 34.74661  -92.28899          124
# 5 California        39512223 38.57667 -121.49363          727
# 6   Colorado         5758736 39.73923 -104.98486          154

population$nRestaurants <- as.numeric(population$nRestaurants)

## CORELACIÓN ENTRE CANTIDAD DE POBLACIÓN Y Nº RESTAURANTES
cor(population$POPESTIMATE2019, population$nRestaurants)
# 0.648239

###### MAPA DE TODO USA ############################

mapa_usa<-map_data('world', region = c("Usa", "Alaska", "Hawaii"), xlim =c(-200,-50))

mapa_usa %>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group),
               fill = "lightgray",
               color = "white",
               size = 0.01) +
  geom_point(data= restaurants, 
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
colnames(population)
population <- population[ , c(4,3,1,2,5)]
head(population)

population_usa<- usmap_transform(population)

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


### BUSCAR CORRELACIÓN ENTRE POBLACIÓN Y CANTIDAD DE RESTAURANTES
