library(leaflet)
my_map <- leaflet() %>% addTiles()
my_map

my_map <- leaflet()
my_map <- addTiles(my_map)
my_map <- addMarkers(my_map,lat = 39.2980803,lng = -76.5898801,
                     popup = 'Jeff Leeks Office' )
my_map

my_app <- leaflet() %>% 
    addTiles()      %>%
    addMarkers(my_map,lat = 39.2980803,lng = -76.5898801,
               popup = 'Jeff Leeks Office' )

my_map

df <- data.frame(lat=runif(20,min = 39.2,max = 39.3),
                 lng=runif(20,min = -76.6, max = -76.5))

df %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers()

df <- data.frame(lat=-36.7367003,lng=174.6938181)

MasseyIcon <- makeIcon(
    iconUrl = "http://www.massey.ac.nz/~strewick/Images/MasseyLogo_Transparent.gif",
    iconWidth = 131,
    iconHeight = 51)
df %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(icon=MasseyIcon,
               popup='<a href=http://www.massey.ac.nz/massey/student-life/about-our-campuses/albany-campus/albany-campus_home.cfm?gclid=Cj0KEQiA-MPCBRCZ0q23tPGm6_8BEiQAgw_bAhEVI8_Ku53ifu2TayOeseBdrH1ZhgkmAEO6vEQBGaQaAgNG8P8HAQ>Link</a>')
