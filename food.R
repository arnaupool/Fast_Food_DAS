install.packages("dplyr", dependencies=TRUE)
install.packages("plyr", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("maps", dependencies=TRUE)
install.packages("usmap", dependencies=TRUE)
install.packages("mapproj", dependencies=TRUE)
install.package("devtools", dependencies=TRUE)

.libs <- c("dplyr","plyr","ggplot2","maps", "mapproj","usmap","devtools")
sapply(.libs,require, character.only=TRUE)


#### ARCHIVO Datafiniti_Fast_Food_Restaurants_May19

restaurants <-read.csv(file.choose(),	 header=T,	 sep=',')
head(restaurants)
colnames(restaurants)
# "id"                "dateAdded"         "dateUpdated"       "address"          
# "categories"        "primaryCategories" "city"              "country" 
# "keys"              "latitude"          "longitude"         "name"             
# "postalCode"        "province"          "sourceURLs"        "websites"    

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



## TRANSFORMACIÓN NOMBRES

nombresRest<-data.frame(table(restaurants$name))

Abby<-c("Abby's Legendary Pizza","Abbys Restaurant Bar")
restaurants$name[restaurants$name %in% Abby] <- "Abby's"

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

Hardee<-c("Hardee's","Hardee's - Loris:","Hardee's Restaurant","Hardee's Restaurants","Hardee's/red Burrito",
"Hardee's/Red Burrito","Hardees","Hardees Red Burrito")
restaurants$name[restaurants$name %in% Hardee] <- "Hardee's"

Hometown<-c("Hometown Buffet","HomeTown Buffet")
restaurants$name[restaurants$name %in% Hometown] <- "HomeTown Buffet"

HotDog<-c("Hot Dog on a Stick","Hot Dog On A Stick")
restaurants$name[restaurants$name %in% HotDog] <- "Hot Dog On A Stick"

InOut<-c("In-N-Out Burger","IN-N-OUT WINGS","InNOut Burger")
restaurants$name[restaurants$name %in% InOut] <- "In-N-Out Burger"

Jack<-c("Jack's","Jack's Family Restaurants","Jack's Hamburgers","Jacks")
restaurants$name[restaurants$name %in% Jack] <- "Jack's"

Jason<-c("Jason's Deli","Jasons Deli")
restaurants$name[restaurants$name %in% Jason] <- "Jason's Deli"

Jerry<-c("Jerry's Subs & Pizza","Jerry's Subs and Pizza")
restaurants$name[restaurants$name %in% Jerry] <- "Jerry's Subs & Pizza"

Jersey<-c("Jersey Mike's Subs","Jersey Mikes Subs")
restaurants$name[restaurants$name %in% Jersey] <- "Jersey Mike's Subs"

Jimmy<-c("Jimmy John's","Jimmy John's Boyton Beach","Jimmy Johns")
restaurants$name[restaurants$name %in% Jimmy] <- "Jimmy John's"

##BORRAR KFC cerrados
restaurants<-restaurants[!grepl("KFC - Closed", restaurants$name),]

KFC<-c("Kentucky Fried Chicken","Kfc","KFC","KFC - Kentucky Fried Chicken",
       "KFC AW","KFC Kentucky Fried Chicken")
restaurants$name[restaurants$name %in% KFC] <- "KFC"

Killer<-c("Killer Burger","Killer Burgers")
restaurants$name[restaurants$name %in% Killer] <- "Killer Burger"

##BORRAR Krystal cerrados
restaurants<-restaurants[!grepl("Krystal - Closed", restaurants$name),]

Krystal<-c("Krystal","Krystal Burgers")
restaurants$name[restaurants$name %in% Krystal] <- "Krystal"

LL<-c("L & L Hawaiian Barbecue","L L Hawaiian Barbecue","L L Hawaiian Barbeque","LL Hawaiian Barbecue")
restaurants$name[restaurants$name %in% LL] <- "L & L Hawaiian Barbecue"

Long<-c("Long John Silver's","Long John Silvers")
restaurants$name[restaurants$name %in% Long] <- "Long John Silver's"

Mai<-c("Mai-Tai Restaurant","Mai Tai Restaurant")
restaurants$name[restaurants$name %in% Mai] <- "Mai-Tai Restaurant"

Mambo<-c("Mambo Grill and Tapas","Mambo Grill Tapas")
restaurants$name[restaurants$name %in% Mambo] <- "Mambo Grill & Tapas"

Mary<-c("Mary's Pizza Shack","Marys Pizza Shack")
restaurants$name[restaurants$name %in% Mary] <- "Mary's Pizza Shack"

McAlister<- c("Mcalister's Deli","McAlister's Deli","McAlisters Deli") 
restaurants$name[restaurants$name %in% McAlister] <- "McAlister's Deli"


### 80 ###
##BORRAR Mcdonalds cerrados
restaurants<-restaurants[!grepl("McDonald's - CLOSED", restaurants$name),]

McDonald <- c("McDonald's", "Mcdonald's", "Mc Donald's","McDonalds",
              "Mcdonalds","McDonalds Family Restaurant")
restaurants$name[restaurants$name %in% McDonald] <- "McDonald's"

Melt<-c("Melt Bar & Grilled","Melt Bar and Grilled")
restaurants$name[restaurants$name %in% Melt] <- "Melt Bar & Grilled"

Moe<-c("Moe's Southwest Grill","Moes Southwest Grill")
restaurants$name[restaurants$name %in% Moe] <- "Moe's Southwest Grill"

Mr<-c("Mr Gyro's Wallingford","Mr Gyros","Mr Gyros Burgers")
restaurants$name[restaurants$name %in% Mr] <- "Mr Gyro's"

Nathan<-c("Nathan's Famous Hot Dogs","Nathans Famous")
restaurants$name[restaurants$name %in% Nathan] <- "Nathan's Famous"

Nichola<-c("Nicholas' Restaurant","Nicholas Restaurant")
restaurants$name[restaurants$name %in% Nichola] <- "Nicholas' Restaurant"

Panda<-c("Panda Express","Panda Express - Inside Vons + Inside Vons +")
restaurants$name[restaurants$name %in% Panda] <- "Panda Express"

Papa<-c("Papa John's Pizza","Papa Johns Pizza")
restaurants$name[restaurants$name %in% Papa] <- "Papa John's"

PDQ<-c("PDQ","PDQ Restaurant")
restaurants$name[restaurants$name %in% PDQ] <- "PDQ"

Pei<-c("Pei Wei","Pei Wei Asian Diner") 
restaurants$name[restaurants$name %in% Pei] <- "Pei Wei"

Peking<-c("Peking Chinese Restaurant","Peking Chinese Restaurants")
restaurants$name[restaurants$name %in% Peking] <- "Peking Chinese Restaurant"

Pietro<-c("Pietro's Pizza & Gallery of Games","Pietro's Pizza Gallery of Games","Pietro's Pizza Pirate Adventure","Pietros Pizza")
restaurants$name[restaurants$name %in% Pietro] <- "Pietro's Pizza"

Pizza<-c("Pizza Hut","Pizza Hut Express")
restaurants$name[restaurants$name %in% Pizza] <- "Pizza Hut"

Popeye<-c("Popeye's Chicken & Biscuits","Popeye's Louisiana Kitchen","Popeyes","Popeyes Chicken and Biscuits","Popeyes Chicken Biscuits","Popeyes Louisiana Kitchen")
restaurants$name[restaurants$name %in% Popeye] <- "Popeye's"

Pot<-c("Pot Belly Sandwich Works","Potbelly Sandwich Works")
restaurants$name[restaurants$name %in% Pot] <- "Pot Belly Sandwich"

Puerto<-c("Puerto Vallarta","Puerto Vallarta Restaurant")
restaurants$name[restaurants$name %in% Puerto] <- "Puerto Vallarta"

Qdoba<-c("Qdoba Mexican Eats","QDOBA Mexican Eats","Qdoba Mexican Grill")
restaurants$name[restaurants$name %in% Qdoba] <- "Qdoba Mexican"

##BORRAR Quiznos cerrados
restaurants<-restaurants[!grepl("Quiznos Closed", restaurants$name),]

Quiznos<-c("Quiznos","Quiznos Sub")
restaurants$name[restaurants$name %in% Quiznos] <- "Quiznos"

Raising<-c("Raising Cane's Chicken Fingers","Raising Canes Chicken Fingers")
restaurants$name[restaurants$name %in% Raising] <- "Raising Cane's Chicken Fingers"

Rallys<-c("Rally's","Rally's Hamburgers","Rally Pizza","Rallys","Rallys Hamburgers")
restaurants$name[restaurants$name %in% Rallys] <- "Rally's"

RedRobin<-c("Red Robin Gourmet Burgers","Red Robin Gourmet Burgers and Brews")
restaurants$name[restaurants$name %in% RedRobin] <- "Red Robin Gourmet Burgers"

Ricks<-c("Rick's on the River","Ricks on the River")
restaurants$name[restaurants$name %in% Ricks] <- "Rick's on the River"

Ritters<-c("Ritter's Frozen Custard","Ritters Frozen Custard")
restaurants$name[restaurants$name %in% Ritters] <- "Ritter's Frozen Custard"

RoyRogers<-c("Roy Rogers","Roy Rogers Restaurant")
restaurants$name[restaurants$name %in% RoyRogers] <- "Roy Rogers"

Runza<-c("Runza","RUNZA")
restaurants$name[restaurants$name %in% Runza] <- "Runza"

Ryans<-c("Ryan's","Ryans")
restaurants$name[restaurants$name %in% Ryans] <- "Ryan's"

Sam<-c("Sam's Hotdog's","Sam's Hotdog Stand")
restaurants$name[restaurants$name %in% Sam] <- "Sam's Hotdog"

Sara<-c("Sara's Too","Saras Too")
restaurants$name[restaurants$name %in% Sara] <- "Sara's Too"

Simple<-c("Simple Simon's Pizza","Simple Simons Pizza")
restaurants$name[restaurants$name %in% Simple] <- "Simple Simon's Pizza"

Slice<-c("Slice of Life","Slice Of Life")
restaurants$name[restaurants$name %in% Slice] <- "Slice Of Life"

##BORRAR Sonic Drive-In cerrados
restaurants<-restaurants[!grepl("Sonic Drive-In - Closed", restaurants$name),]

Sonic<-c("Sonic","Sonic Drive-in","Sonic Drive-In","SONIC Drive-In",
"Sonic Drive-In - Temporarily Closed","SONIC Drive In","Sonic DriveIn")
restaurants$name[restaurants$name %in% Sonic] <- "Sonic Drive-In"

Sorrento<-c("Sorrento's Restaurant","Sorrentos")
restaurants$name[restaurants$name %in% Sorrento] <- "Sorrento's"

Stanfields<-c("Stanfield's Steakhouse","Stanfields Steak House")
restaurants$name[restaurants$name %in% Stanfields] <- "Stanfield's Steak House"

Steak<-c("Steak 'n Shake","Steak 'N Shake","STEAK 'N SHAKE","Steak n Shake","Steak N Shake")
restaurants$name[restaurants$name %in% Steak] <- "Steak 'N Shake"

Subway<-c("Subway","SUBWAY","Subway 16 Mile","Subway Meadows Park SC","SUBWAY Restaurant",
"Subway Sandwiches","Subway Sandwiches Salads")
restaurants$name[restaurants$name %in% Subway] <- "Subway"

Sumo<-c("Sumo Japanese Steak House","Sumo Japanese Steakhouse Sushi")
restaurants$name[restaurants$name %in% Sumo] <- "Sumo Japanese Steak House"

TL<-c("T & L Hot Dogs","T & L Hotdogs")
restaurants$name[restaurants$name %in% TL] <- "T & L Hot Dogs"

Taco<-c("Taco John's","Taco John's - Temporarily Closed","Taco Johns")
restaurants$name[restaurants$name %in% Taco] <- "Taco John's"

Taste<-c("Taste of Buffalo Pizzeria","Taste Of Buffalo Pizzeria")
restaurants$name[restaurants$name %in% Taste] <- "Taste Of Buffalo Pizzeria"

Oasis<-c("The Oasis Restaurant and Delivery","The Oasis Restaurant Delivery")
restaurants$name[restaurants$name %in% Oasis] <- "The Oasis"

##BORRAR Three Degrees cerrados
restaurants<-restaurants[!grepl("Three Degrees Waterfront Bar Grill - CLOSED", restaurants$name),]

Tom<-c("Tom's Drive-In","Tom Drive-in")
restaurants$name[restaurants$name %in% Tom] <- "Tom's Drive-In"

Topper<-c("Topper's Pizza","Toppers Pizza")
restaurants$name[restaurants$name %in% Topper] <- "Topper's Pizza"

Uncle<-c("Uncle Maddio's Pizza","Uncle Maddio's Pizza Joint")
restaurants$name[restaurants$name %in% Uncle] <- "Uncle Maddio's Pizza"

Wendy<-c("Wendy's","Wendy's - Temporarily Closed","Wendys")
restaurants$name[restaurants$name %in% Wendy] <- "Wendy's"

WG<-c("Wg Grinders","WG Grinders")
restaurants$name[restaurants$name %in% WG] <- "WG Grinders"

##BORRAR Whataburger cerrados
restaurants<-restaurants[!grepl("Whataburger - Closed", restaurants$name),]

Whataburger<-c("Whataburger","Whataburger of East Texas","Whataburger Of East Texas Corporate")
restaurants$name[restaurants$name %in% Whataburger] <- "Whataburger"

Wingstop<-c("Wingstop","Wingstop Fish Chicken","Wingstop Restaurant")
restaurants$name[restaurants$name %in% Wingstop] <- "Wingstop"


##BORRAR Starbucks -- solo 2 existencias para una cadena conocida de cafes(no fast food)
restaurants<-restaurants[!grepl("Starbucks", restaurants$name),]



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



# ### CONVERTIR A FACTOR  PROVINCE, CATEGORIES 
# 
# sapply(restaurants, class)
# 
# restaurants$city <- factor(restaurants$city)
# restaurants$province <- factor(restaurants$province)
# restaurants$categories <- factor(restaurants$categories)
# restaurants$primaryCategories <- factor(restaurants$primaryCategories)
# 

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



################## DATOS PROBLEMAS DE SALUD ###################################### 

### fichero 2019 annual
salud<-read.csv(file.choose(),	 header=T,	 sep=',')

colnames(obesidad)
# [1] "Edition"      "Report.Type"  "Measure.Name" "State.Name"   "Rank"         "Value"       
# [7] "Score"        "Lower.CI"     "Upper.CI"     "Source"       "Source.Year" 

# probando<-data.frame(table(obesidad$Measure.Name))

Obesidad<-salud[salud$Measure.Name == "Obesity",]

Colesterol<-salud[salud$Measure.Name == "High Cholesterol",]

Problemas_Cardiovasculares <-salud[salud$Measure.Name == "Cardiovascular Diseases",]

Diabetes <-salud[salud$Measure.Name == "Diabetes",]

Presion_Alta <-salud[salud$Measure.Name == "High Blood Pressure",]

Cancer<-salud[salud$Measure.Name == "Cancer",]



## Borrar fila de datos generales USA

Obesidad <- Obesidad[!grepl("United States", Obesidad$State.Name),]

Colesterol <- Colesterol[!grepl("United States", Colesterol$State.Name),]

Problemas_Cardiovasculares <- Problemas_Cardiovasculares[!grepl("United States", Problemas_Cardiovasculares$State.Name),]

Diabetes <- Diabetes[!grepl("United States", Diabetes$State.Name),]

Presion_Alta <- Presion_Alta[!grepl("United States", Presion_Alta$State.Name),]

Cancer <- Cancer[!grepl("United States", Cancer$State.Name),]


### Ordenar alfabeticamente

Obesidad <-arrange(Obesidad,State.Name)
  
Colesterol <-arrange(Colesterol,State.Name)

Problemas_Cardiovasculares <-arrange(Problemas_Cardiovasculares,State.Name)

Presion_Alta <- arrange(Presion_Alta,State.Name)

Diabetes<-arrange(Diabetes,State.Name)

Cancer<-arrange(Cancer,State.Name)



## Borrar columnas innecesarias// quedarse solo con la de valor y cambio de nombre

Obesidad <- subset(Obesidad,select = 6)
colnames(Obesidad) <- "Valor_Obesidad"

Colesterol <- subset(Colesterol,select = 6) 
colnames(Colesterol) <- "Valor_Colesterol"

Problemas_Cardiovasculares <- subset(Problemas_Cardiovasculares,select = 6)
colnames(Problemas_Cardiovasculares) <- "Valor_Problemas_Cardiovasculares"

Presion_Alta <- subset(Presion_Alta,select = 6)
colnames(Presion_Alta) <- "Valor_Presion_Alta"

Diabetes <- subset(Diabetes,select = 6)
colnames(Diabetes) <- "Valor_Diabetes"

Cancer <- subset(Cancer,select = 6)
colnames(Cancer) <- "Valor_Cancer"



population <- cbind(population,Obesidad,Colesterol,Problemas_Cardiovasculares,
                    Presion_Alta,Diabetes,Cancer)


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
