#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# 1. Set Up------------------------------------------------------------------------ 



## 1.2 Load packages----------------------------------------------------------------
library(shiny)
library(fpp3)
library(scales)
library(tidyr)
library(shinycssloaders)
library(shinyWidgets)
library(shinythemes)
library(stringr)
library(shinyjs)
library(ggplot2)
library(plotly)
library(rsconnect)
#library(rgdal)
library(plyr)
library(tigris)
library(dplyr)
library(leaflet)
library(leaflegend)
library(tidycensus)
library(tidyverse)
library(viridis)
library(readxl)
library(sf) 
options(scipen=999)
library(htmlwidgets)
library(fontawesome) 
library(bslib)
library(tidyUSDA)
library(kableExtra)

#options(shiny.maxRequestSize = 80*1024^2)

## 1. 1 CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY -------------
# JavaScript code
jscode <- 'var x = document.getElementsByClassName("navbar-brand");
    var dspgLink = "https://kohl.aaec.vt.edu/";
    var githubLink = "https://github.com/Kealyn8/soybeans";
    var dspgLogoHTML = \'<a href="\' + dspgLink + \'"><img src="VTlogo2.jpg" alt="VT DSPG" style="height:38px;"></a>\';
    var githubLogoHTML = \'<a href="\' + githubLink + \'"><img src="DSPG_black-01.png" alt="GitHub" style="max-height: 30px; max-width: 100%;"></a>\';
    var logosHTML = dspgLogoHTML + githubLogoHTML;
    x[0].innerHTML = x[0].innerHTML + " " + logosHTML;
  '

## 1.3 Load the data----------------------------------------------------------------
### 1.3.1 Load the original data----------------------------------------------------------------

## Read shapefile
count <- st_read("/Users/Kealyn/Desktop/VSA/app/Data/va_shp/va_shp.shp")

## Read soybeans data
soycounties <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/soycounties.csv")

#production and exports
domvex <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/domvex.csv")

#fuel
fuelprice <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/fuelprice.csv")

#BU production
PROD <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/produc.csv")

#corn and soy
combined <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/combined.csv")

#us and va yield
UScomb <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/UScomb.csv")

#states yield
STATESYIELD <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/STATESYIELD.csv")

#kable data
growth_data <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/growth_data.csv")

#export data
exports <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/exports.csv") %>% filter(Year >= 2000)

#pace of exports data
pace <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/pace.csv") %>% filter(count <= 53) %>% select(!conversion)

lossratio <- read.csv("/Users/Kealyn/Desktop/VSA/app/Data/lossratio.csv") %>% filter(year >= 2000)

## new column for 
soycounties$Productivity <- soycounties$Bushels / soycounties$Acres

## Merge data

counts2 <- count
count <- as.data.frame(NULL)

for(y in unique(soycounties$year)) {
  
  soyco <- soycounties %>% filter(year == y)
  counts3 <- counts2 %>% left_join(soyco, by = "NAME")
  counts3$year <- rep(y, nrow(counts3))
  counts3$Acres  <- ifelse(is.na(counts3$Acres), 0, counts3$Acres)
  counts3$Bushels  <- ifelse(is.na(counts3$Bushels), 0, counts3$Bushels)
  counts3$Productivity  <- ifelse(is.na(counts3$Productivity), 0, counts3$Productivity)
  count <- rbind(count, counts3)
  
}

count <- count %>% pivot_longer(!c(geometry, year, NAME), names_to = 'Variable', values_to = 'Value')

#these are the locations for the crushing mills, grain elevators, and lego

locations <- as.data.frame(cbind(c('Perdue Grain and Oilseed', 'Perdue AgriBusiness - Bainbridge Grain Elevator and Oilseed Crush', 'Perdue Grain and Oilseed, LLC. (NC)', 'Cargill, Inc.', 'Perdue Grain and Oilseed, LLC.(MD)',
                                   'Perdue Agribusiness Kinsale', 'Perdue Agribusiness Kilmarnock','Perdue Agribusiness Tappahannock', 'Mennel Milling West Point',
                                   'Scoular Port of Richmond', 'Scoular Weyers Cave', 'Scoular Windsor', 'Smithfield Grain Petersburg', 'Wakefield Farm Service', 'Meherrin Grain Franklin',
                                   'Meherrin Grain Capron', 'Meherrin Grain Charlotte Courthouse', 'Meherrin Grain Newsomes', 'Keystone Grain', 'Augusta Co-op Farm Bureau, Inc. Staunton',
                                   'Rockingham Co-op Dayton', 'Culpeper Farmers Co-op', 'Lego Facility', 'DeLong Facility'),
                                 c('Crush Plant', 'Crush Plant', 'Crush Plant', 'Crush Plant', 'Crush Plant', 'Grain Elevator','Grain Elevator','Grain Elevator','Grain Elevator','Grain Elevator','Grain Elevator',
                                   'Grain Elevator','Grain Elevator', 'Grain Elevator', 'Grain Elevator','Grain Elevator','Grain Elevator','Grain Elevator','Grain Elevator','Grain Elevator','Grain Elevator','Grain Elevator',
                                   'Additional Facility', 'Additional Facility'),
                                 c(36.8036684, 40.0729735, 36.36438272, 35.0865324, 38.38468454, 38.0303092, 37.7005569,37.9177046,37.5506807,37.4565925,38.2808153, 36.8099991, 37.1980703, 36.9732506, 36.660653, 36.706100, 37.133065, 36.6302446, 36.6862312, 38.1347458, 38.423984, 38.4826347, 37.31111830508404, 36.8354),
                                 c(-76.28722583, -76.6365597, -76.89787336, -78.84710742, -75.52773315, -76.5756774,-76.3522586,-76.854209, -76.8157385, -77.4225141, -78.9286916, -76.7470809, -77.4352446,  -76.9905024, -76.975516, -77.200142, -78.720856, -77.1096605, -78.9032562, -79.0425081, -78.9228, -77.9716346, -77.38178760354225, -76.2983),
                                 c(27, 17.5, 13, 'N/A', 22, 'N/A','N/A','N/A','N/A','N/A','N/A','N/A','N/A','N/A','N/A','N/A','N/A','N/A','N/A','N/A','N/A','N/A', 'N/A','N/A')))


colnames(locations) <- c('Place', 'Type', 'lat', 'lng', 'Capacity')

#capacities <- as.data.frame(cbind(c('Perdue Grain and Oilseed', 'Perdue AgriBusiness - Bainbridge Grain Elevator and Oilseed Crush', 'Perdue Grain and Oilseed, LLC. (NC)', 'Cargill, Inc.', 'Perdue Grain and Oilseed, LLC.(MD)'),
#                                  c(27, 17.5, 13, 'N/A', 22)))
#colnames(capacities) <- c('Plant', 'Cap')

# Convert locations into a spatial data frame so that it can be plotted

locations <- st_as_sf(locations, coords = c('lng', 'lat'), crs = st_crs(count))

ll <- as.data.frame(st_coordinates(locations$geometry))

locations$lat <- ll$Y
locations$lng <- ll$X

#transforming sf to WGS84

locations <- st_transform(locations, crs = 4326)
count$geometry <- st_transform(count$geometry, crs = 4326)

# names for the hover features

good_names <- c("Acres Harvested", "Bushels Harvested", "Bushels/Acre")

good_names2 <- c("Soybean Acres Harvested in Virginia", "Soybean Bushels Harvested in Virginia", "Soybean Productivity in Virginia")


#making icons for maps
crush_plant <- makeAwesomeIcon(icon = NA, library = "fa", iconColor = 'ivory', markerColor = "darkgreen")
grain_elevator <- makeAwesomeIcon(icon = NA, library = "fa", iconColor = 'ivory', markerColor = 'darkblue')
add_facility <- makeAwesomeIcon(icon = NA, library = "fa", iconColor = 'ivory', markerColor = "purple")


# ## 1.4 Define your functions -------------------------------------------------------
# # Function for health outcomes
mapping2 <- function(variable, year) {
  
  # Filter data for selected year and variable
  temp <- count[count$year == year & count$Variable == variable, ]
  
  # Identify the index of the selected variable
  idx <- which(unique(count$Variable) == variable)
  
  # Create a color palette function based on the "Value" column
  pal <- colorNumeric(palette = c('white', 'orange', 'red4'), domain = temp$Value)
  
  # Create labels for counties
  county_labels <- sprintf(
    "<strong>%s</strong><br/>%s: %g", 
    temp$NAME, 
    good_names[idx], 
    temp$Value
  ) %>% lapply(htmltools::HTML)
  
  # Create labels for locations
  agent_labels <- sprintf(
    "<strong>%s</strong>",
    locations$Place
  ) %>% lapply(htmltools::HTML)

  # Wrap legend title if too long
  spaces <- gregexpr("\\s", good_names[idx])[[1]]
  middle_space <- spaces[length(spaces) %/% 2 + 1]
  legend_title <- paste0(substring(good_names[idx], 1, middle_space-1), "</br>", substring(good_names[idx], middle_space+1))
  
  # Create title for the map
  map_title = paste(good_names2[idx], year, sep= " - ")
  
  #making icon set for legend
  icons <- awesomeIconList(
    `Additional Facility` = add_facility,
    `Crush Plant` = crush_plant,
    `Grain Elevator` = grain_elevator
    )
  
  # group names for legend
  groups <- c("Crush Plant" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-darkgreen awesome-marker'></div>Crush Plant",
              "Crush Plant" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-darkgreen awesome-marker'></div>Crush Plant",
              "Crush Plant" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-darkgreen awesome-marker'></div>Crush Plant",
              "Crush Plant" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-darkgreen awesome-marker'></div>Crush Plant",
              "Crush Plant" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-darkgreen awesome-marker'></div>Crush Plant",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Grain Elevator" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-darkblue awesome-marker'></i></div>Grain Elevator",
              "Additional Facility" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-purple awesome-marker'></div>Additional Facility",
              "Additional Facility" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-purple awesome-marker'></div>Additional Facility")
  
  # Create leaflet map
  map1 <- leaflet(data = temp) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(fillColor = ~pal(Value),
                color = "#BDBDC3",
                weight = 1,
                smoothFactor = 0.2,
                opacity = 1.0,
                fillOpacity = 1.0,
                highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
                label = county_labels,
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px", direction = "auto")) %>%
    #map title
    addControl(htmltools::HTML(paste0("<h1 style='margin:3px'>", map_title, "</h1>")), position = "topright", data = NULL) %>% 
    #legend for continious scale
    addLegend(pal = pal, values = ~Value, title = legend_title, position = "topright") %>%
    #legend + markers
    addAwesomeMarkers(data= locations, lat= locations$lat, lng= locations$lng, icon = ~icons[locations$Type], group= ~groups, label= agent_labels) %>% 
  addLayersControl(
      overlayGroups = groups,
      options = layersControlOptions(collapsed = FALSE),
      position= "bottomright",
      data= locations) %>% 
    htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:left\"><strong>Processing Facilities</strong></label>');
        }
    ") %>% 
    #addControl(htmltools::HTML( '<div style="background:grey; width: 10px; height: 10px;"></div><div>Missing values</div>'), position = "topright") %>% 
    #setting default map zoom
    setView(lng = -78.6568942, lat = 38.2315734, zoom = 6.8)
}


#Soybean oil graph
fuels <- {
  
fuelplot <- ggplot(fuelprice, aes(x = Year)) +
    geom_bar(stat = "identity", aes(y = disappearance, fill = Fueltype), width = 0.5) +
    geom_line(aes(y = Supply, color = "Supply"), linewidth = 0.8) +
    geom_line(aes(y = `Soybean.oil.price`*350, color = "Price"), linewidth = 0.8) +
    scale_y_continuous(expand = c(0, 0),
                       name = "Disappearance (Billions of Pounds)", 
                       limits = c(0,30000),
                       labels = scales::label_number(scale = 1e-3, suffix = "B"),  # Primary y-axis label
                       sec.axis = sec_axis(~./350, name = "Cents per Pound", 
                                           labels = scales::label_number(suffix = ""))) +
    scale_color_manual(name = "", values = c("Supply" = "dodgerblue", "Price" = "brown3"), 
                       labels = c("Price", "Supply")) +
    scale_fill_manual(name = "", values = c("Biofuel" = "gold", "Otheruse" = "chartreuse3"),
                      labels = c("Biofuel", "Otheruse")) +
    labs(title = "Soybean Oil Supply and Domestic Disappearance", subtitle = "2010 - 2024", caption = "Source: USDA", x = NULL) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = NULL) 

fuelplot
  
}

#production and exports graph
domestic <- { 
  
domest <- ggplot(domvex, aes(x = Year)) +
  geom_line(aes(y = Exports, colour = "Exports")) +
  geom_line(aes(y = Domestic, colour = "Domestic")) + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "B")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Bushels (Billions)", x = "Year", title = "Soybean Production and Exports in the United States",
       subtitle = "2000 - 2024", caption = "Source: USDA") +
  scale_color_manual(name = "", values = c("red", "darkmagenta"), 
                     labels = c("Domestic", "Exports")) +
  annotate("text", x = 2014.5, y = 1400, label = "Exports", size = 3) +
  annotate("text", x = 2010, y = 2000, label = "Domestic Use", size = 3) +
  theme(legend.box.background = element_rect(color = "black", fill = "lightgray")) 
  
  domest
}

#bu production
prodx <- {
  
bupro <- ggplot(PROD, aes(x = year)) +
  geom_line(data = subset(PROD, state_name == "US TOTAL"), aes(y = Value/100, color = "US Production")) +
  geom_line(data = subset(PROD, state_name == "VIRGINIA"), aes(y = Value, color = "VA Production")) +
  scale_y_continuous(
    name = "Virginia Production (Millions)",
    labels = scales::label_number(scale = 1e-6, suffix = "M"),  # Primary y-axis label
    sec.axis = sec_axis(~.*100, name = "United States Production (Billions)",
                        labels = scales::label_number(scale = 1e-9, suffix = "B"))) + #secondary axis label
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(name = NULL, 
                     values = c("US Production" = "cornflowerblue", "VA Production" = "darkred")) +
  labs(title = "Soybean Bushel Production in Virginia and United States", subtitle = "(2000 - 2024)", x = NULL, caption = "Source: USDA") +
  theme(legend.box.background = element_rect(color = "black", fill = "lightgray"))

bupro

}

#corn and soy production plotly
cands <- {

soycorx <- combined %>% filter(Commodity %in% c("soy.per","corn.per")) %>%
  mutate(Commodity = recode(Commodity, corn.per = "Corn", soy.per = "Soy")) %>% 
  ggplot(aes(x = Year, y = Acreage.., fill = Commodity)) +
  geom_bar(stat = "identity") +
  xlim(2000,2024)+
  labs(x = "Year", y = "Acreage Percentage", 
       title = "Soy and Corn Acreage in Virginia", subtitle = "2000 - 2024", caption = "Source: USDA") +
  scale_fill_manual("", values = c(Soy = "darkgreen", Corn = "darkgoldenrod1")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(legend.box.background = element_rect(color = "black", fill = "lightgray"))


}

#yield for us and va
yieldx <- {
  
yield <- ggplot(UScomb, aes(x = Year)) +
    geom_line(aes(y = VA.Yield.bu.acre, color = "VA Yield")) +
    geom_line(aes(y = US.Yield.bu.acre, color = "US Yield")) +
    theme_bw() + 
    scale_color_manual(name = "", values = c("cornflowerblue","darkred"), 
                       labels = c("US Yield", "VA Yield")) +
    theme(legend.position = "bottom") +
    theme(legend.box.background = element_rect(color = "black", fill = "lightgray")) +
    labs(y = "Yield in bu/acre", x = "Year", 
         title = "Soybean Yield in bu/acre in Virginia and the United States", subtitle = "2000 - 2024",
         caption = "Source: USDA")
yield
}

#yield for va and surrounding states
statey <- {
  
statesx <- ggplot(STATESYIELD, aes(x = Year, y = Value, color = State, group = State)) +
  geom_line(data = data1, aes(size = 1.5)) +  # Virginia's thicker line
  geom_line(data = data2, aes(size = 1)) +   # Other states' default line thickness
  scale_size_identity() + 
  xlim(2010, 2024) +
  theme_bw() +
  labs(title = "Soybean Yield in Surrounding States", subtitle = "2010 - 2024", caption = "Source: USDA", 
       y = "Yield in bu/acre", x = NULL) +
  theme(legend.position = "bottom", legend.key.height = unit(0.5, "cm"),  # Reduce the height of each legend key
        legend.key.width = unit(1.5, "cm")) +
  theme(legend.box.background = element_rect(color = "black", fill = "lightgray")) +
  guides(color = guide_legend(label.theme = element_text(size = 8), ncol = 3))

statesx
}

#data for state yield kable
kab <- {
  ykab <- growth_data %>%
    kable(format = "html", 
          caption = "Soybean Yield Growth Comparison by State (2010-2024)", 
          col.names = c("State", "2010 Yield", "2024 Yield", "Growth (%)")) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
    column_spec(1, width = "10em", color = "black") %>%
    column_spec(2:3, width = "10em", color = "black") %>%
    row_spec(0, bold = T, color = "white", background = "darkgreen") 
  
ykab
}

#US exports graph
exportsx <- {
  exportsy <- ggplot(exports, aes(y = Exports, x = Year)) +
    geom_line(color = "darkgreen") +
    theme_bw() +
    labs(title = "Soybean Exports in the US", subtitle = "2000 - 2024", caption = "Source: WASDE", 
         y = "Millions of Bushels", x = NULL)
  
}

#Pace of exports graph
pacey <- {
  pacex <- ggplot(pace, aes(x = count)) +
    geom_line(aes(y = percent22*100, color = "MY 2022/23")) +
    geom_line(aes(y = percent23*100, color = "MY 2023/24")) +
    geom_line(aes(y = (percent24*100), color = "MY 2024/25")) +
    scale_color_manual(values = c("MY 2022/23" = "red3", "MY 2023/24" = "orchid", "MY 2024/25" = "darkgreen"),
                       name = NULL) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(title = "Pace of U.S. Soybean Exports", x = "Week", y = "Soybeans, Metric Tons (% of Total)", subtitle = "Marketing Year (Week 1 = 1st week of September)",
         caption = "Source: WASDE, FAS Export Sales Query System")
}

lossy <- {
  # Custom transformation: bring loss ratio up to appear on the secondary axis
  max_acres <- max(c(lossratio$harvested, lossratio$planted), na.rm = TRUE)
  max_loss_percent <- 125  
  scaling_factor <- max_acres / max_loss_percent
  
lossx <- ggplot(lossratio, aes(x = year)) +
    geom_bar(aes(y = lossratio * scaling_factor, fill = "Difference"), 
             stat = "identity", position = "dodge", alpha = 0.6, width = 0.35) +
    geom_line(aes(y = harvested, color = "Acres Harvested"), size = 1.2) +
    geom_line(aes(y = planted, color = "Acres Planted"), size = 1.2) +
    coord_cartesian(ylim = c(40000000, 100000000)) + 
    scale_y_continuous(
      name = "Acres (Planted & Harvested)",
      sec.axis = sec_axis(~ . / scaling_factor, name = "Difference (%)", breaks = seq(0, 200, 20))
    ) +
    scale_fill_manual(name = "", values = c("Difference" = "darkgreen")) +
    scale_color_manual(name = "", values = c("Acres Harvested" = "orange2", "Acres Planted" = "navy")) +
    labs(x = NULL, title = "Soybean Acres Harvested vs. Acres Planted", subtitle = "2000 - 2024", caption = "Source: USDA") +
    theme_bw() +
    theme(
      axis.title.y = element_text(color = "black"),
      axis.title.y.right = element_text(color = "black"),
      legend.position = "bottom"
    )
}



# 2. Define UI for application ------------------------------------------------------------
ui <- navbarPage(#title = "DSPG 2023",
  selected = "overview",
  theme = shinytheme("flatly"),
  #theme = bs_theme(bootswatch = "flatly"),
  # theme = bs_theme(fg = "#000000",
  #          bg = "#FFEFEF",
  #          primary = "#660000",
  #          secondary = "#FF6600"),
  #tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
  tags$head(tags$style('DSPG 2023')), 
  useShinyjs(),
  
  ## 2.1 Tab Overview--------------------------------------------
  tabPanel("Overview", value = "overview",
           fluidRow(style = "margin: 12px;",
                    align = "center",
                    h1(strong("Soybeans of the 21st Century"), style = "font-size: 65px;"),
                    h4("Presented by the Kohl Centre at Virginia Tech", style = "font-size: 50px;"),
                    h4("In collaboration with the Virginia Soybean Association", style = "font-size: 50px;"),
                    img(src = "VSAlogo.jpg", style = "display: inline; margin-right: 5px;", width = "500px;", align = "center"),
                    br()
           ),
           fluidRow(style = "margin: 12px;",
                    align = "justify",
                    column(6,
                           h2(strong("Project Background"), align = "center"),
                           p(strong("Challenge:"),
                             p("Soybeans are among the most valuable crops in the United States, both in terms of economic output and land use. Yet Virginia, despite its access to export markets through the Port of Virginia and a strong history of agriculture, has not reached its full potential in soybean production."),
                             p("By addressing the disparities in soybean production between Virginia and top producing states, there is a significant opportunity to expand Virginia's role in national and global soybean production. Expanding production capacity, improving infrastructure, and keeping up with rising demand in food, industry, and renewable energy sectors can help ensure Virginia's future as a competitive soybean producer.")
                           ),
                           p(strong("Why Soybeans Matter:"),
                             p("Soybeans are a strategic crop with applications across multiple industries: protein and oil for human consumption, high-protein meal for animal feed, biodegradable plastics, lubricants, adhesives, and biodiesel. As global demand grows and supply chains shift, soybeans offer a dynamic opportunity for agricultural growth and crop diversification."),
                             p("This project seeks to elevate Virginia’s role by exploring ways to enhance production efficiency, expand applications, and support policies that create long-term resilience and growth in the soybean industry.")
                           ),
                           p(strong("The Role of Virginia Agriculture:"),
                             p("Agriculture is an extremely large industry in Virginia, however, soybeans remain an underutilized portion of that economy. By identifying key gaps in productivity, infrastructure, and innovation, this project aims to bridge the divide and provide a roadmap for strategic development.")
                           )
                    ),
                    column(6,
                           h2(strong("Our Work"), align = "center"),
                           p(strong("Purpose:")),
                           p("Our team, composed of undergraduate and graduate researchers from the Virginia Tech Department of Economics and supported by the Kohl Centre, is developing a comprehensive assessment of soybean production in Virginia. Our work is built around two sections: historical analysis and current performance. By evaluating each stage of the soybean value chain, from planting to consumption, we aim to provide insights that can guide decision-making for farmers, industry leaders, and policymakers."),
                           h2(strong("Areas of Analysis"), align = "center"),
                           p(strong("Historical Trends:"),
                             p("We begin by examining decades of soybean production data, including yield variability, production, and global trade impacts. Understanding how Virginia’s soybean industry has evolved allows us to identify long-term trends, policy impacts, and structural weaknesses."),
                             p("This retrospective view helps contextualize Virginia’s performance against other states and spot consistent opportunities or vulnerabilities across regions.")
                           ),
                           p(strong("Current Landscape:"),
                             p("We assess present-day production volumes, acreage use, and export data in Virginia’s soybean sector."),
                             p("We also evaluate infrastructure capacity, such as crushing facilities, processing plants, and proximity to export terminals, to determine logistical advantages and constraints throughout Virginia.")
                           )
                    )
           ),
           fluidRow(style = "margin: 12px;",
                    align = "center",
                    column(12,
                           h2(strong("Acknowledgments"), align = "center"),
                           p("We gratefully acknowledge the ", strong("Kohl Centre at Virginia Tech"), 
                             " for their generous sponsorship, mentorship, and support of undergraduate research. Their commitment to applied economic problem-solving and real-world policy engagement made this project possible."),
                           p("We also thank the ", strong("Virginia Soybean Association"), 
                             " and all regional farmers, industry experts, and university faculty who provided critical insight, data access, and practical knowledge to guide this research."),
                           p("This project represents a collaborative effort to better understand and shape the future of Virginia agriculture through innovation, economic analysis, and stakeholder engagement.")
                    )
           ),
           fluidRow(align = "center",
                    p(tags$small(em('Last updated: May 2025')))
           )
  ),
  
  
  

  ## 2.2 Historical Data ------------------
  
  tabPanel("History of Soybeans",
           h1(strong("The History of Soybeans"), align = "center"), 
              
           tabsetPanel(
             tabPanel("Why Soybeans?",
                      
           h2("Why Soybeans?", align = "left"),
           p("Over the past decade, the United States has experienced a significant increase in the use and demand for biofuels derived from soybeans. 
             The share of soybean oil used for biofuel production has quadrupled, reaching 43% of all soybean oil used in the United States in the most recent marketing year (USDA ERS). 
             This surge in demand has been driven by supportive state and federal policies, such as the Renewable Fuel Standard (RFS) program, which mandates the use of specified levels of renewable fuels. 
             The growth in biofuel production has had a substantial impact on the soybean industry, with almost 40% of U.S. soybean oil production projected to go into biofuels by 2025, up from nearly none two decades earlier (USDA ERS).
             ", align = "left"),
           
       fluidRow(column(6, 
            h3("Demand Increases", align = "left"),
           p("In Virginia, the biofuel industry has also seen notable developments. Perdue AgriBusiness, a major player in the state's agriculture sector, announced in 2022 an investment of $59.1 million to expand its soybean-crush operation in Chesapeake. 
           This expansion aims to increase production of high protein soybean meal, soybean oil, and hulls, positioning the company to meet growing demand in the biofuel industry. Virginia Biodiesel Refinery, the state's oldest and largest biodiesel production facility, has been operating since 2003, 
           contributing to the local production of sustainable diesel fuel (Virginia Biodiesel Refinery). These developments highlight Virginia's role in the growing biofuel market, aligning with the national trend of increased soybean use for renewable energy production.
             ", align = "left"), 
           p("During the same period, the United States has experienced significant changes in the use and demand for soybeans in both food and feed applications. Soybean meal, a crucial component in livestock and poultry diets, has seen robust demand due to steady numbers of cattle, hogs, pigs, and chick placements (CoBank). 
           In the 2023/24 marketing year, U.S. soybean meal exports reached a record high of 14.4 million metric tons, valued at $6.7 billion, marking a 10% increase from the previous year and 17% above the 5-year average (Feed and Grain). This surge in meal exports addresses concerns about potential oversupply resulting from increased domestic crush.
             ", align = "left"), 
           p("Looking ahead to the 2024/25 marketing year, the U.S. Department of Agriculture projects global soybean demand to increase by 4.3% year-over-year to 346.2 million metric tons (Feed and Grain). 
           Domestic soybean meal usage is expected to increase by 4% to 36.5 million metric tons in 2024/25, as the livestock sector is projected to expand due to lower prices for feed grains and soybean meal (USDA ERS). The Philippines emerged as the top buyer of U.S. soybean meal in 2023/24, purchasing 2.6 million metric tons, followed by Mexico and Canada (Feed and Grain).
             ", align = "left"),
           p("The United States has experienced a shift in the balance between soybean exports and domestic use. While exports have historically been a significant driver of U.S. soybean demand, domestic consumption, particularly for crushing, has been steadily increasing. 
           In the 2023/24 marketing year, U.S. soybean crush reached a record high of 2,300 million bushels, driven by strong margins and high domestic soybean oil prices (USDA ERS). This trend is expected to continue, with projections indicating that domestic soybean crush capacity could rise by 32% to nearly 2.9 billion bushels by 2026 (CoBank).
             ", align = "left")
           ),
           column(5,
                  tags$div(style = "margin-top: 70px;",
                  img(src = "Soybeans.jpg", style = "display: inline; margin-right: 5px;", width = "680px;", align = "center"),
                  p("Figure 1: Soybeans growing", align = "center", style = "font-style: italic; font-size: 12px; margin-top: 5px;")
           ),
           column(1)
           )),


        h3("Virginia Production", align = "left"),
           p("In Virginia, the soybean industry has shown remarkable resilience and growth, mirroring national trends but with some distinct characteristics. The state's soybean exports have fluctuated significantly in recent years, particularly in relation to China. In 2017, Virginia's soybean exports to China reached $360 million, but dropped to $58 million in 2018 due to trade tensions (Virginia Department of Agriculture and Consumer Services).
           Despite these challenges, Virginia's agricultural exports have remained strong, with soybeans leading the state's top agricultural and forestry exports. In 2023, soybean exports from Virginia were valued at over $1.4 billion (Virginia Department of Agriculture and Consumer Services). This suggests that while Virginia's soybean industry has faced export volatility,
           it has managed to maintain a strong export presence while also potentially benefiting from increased domestic demand for crushing and value-added products.
           ", align = "left"),
           p("Virginia's soybean production has shown notable fluctuations in recent years, with varying yields and acreage. In 2024, Virginia's soybean production was estimated at 26.4 million bushels, a significant 22% increase from the previous year. The state's farmers harvested 600,000 acres, up 30,000 acres from 2023, with yields averaging 44 bushels per acre, a 6-bushel improvement (Morning Ag Clips).
           This increase came despite challenging weather conditions that affected other crops in the state. In comparison, Maryland's 2023 soybean yield averaged 47 bushels per acre, with a total production of 21.6 million bushels harvested from 460,000 acres (Maryland Soybean Board). Pennsylvania's 2024 forecast shows 600,000 harvested acres with an expected yield of 46 bushels per acre,
           leading to an anticipated total production of 27.60 million bushels (Pennsylvania Soybean Board).
           ", align = "left"),

        h3("Virginia and Neighboring States", align = "left"),
          p("When comparing Virginia's soybean production to neighboring states, there are notable differences. North Carolina, while specific yield data wasn't available, had an estimated 931,000 acres of soybean production in 2024, accounting for 28.8% of the state's cropland (USDA NASS). This suggests a significantly larger soybean industry compared to Virginia.
          Kentucky's soybean production in 2024 was forecast at 112 million bushels, a 12% increase from 2023, with yields estimated at 55.0 bushels per acre (Morning Ag Clips), indicating a more productive soybean sector than Virginia's. These variations in production and yield across states likely reflect differences in climate, soil conditions, farming practices,
          and the relative importance of soybeans in each state's agricultural economy. The data also suggests a general trend of increasing yields over time in the region, influenced by factors such as weather conditions, planting dates, and row spacing techniques (Pennsylvania Soybean Board).
          ", align ="left"),

       h3(("Local Industry"), align = "left"),
       p("Virginia's soybean industry plays a significant role in the state's agricultural economy, with soybeans being utilized through various channels. One of the major players in Virginia's soybean market is Perdue AgriBusiness, which purchases 80% of Virginia's soybeans and exports 72 million tons of soybeans per year through The Port of Virginia (Youngkin).
         The company's Chesapeake facility supplies crude degummed soybean oil to Perdue's Salisbury, Maryland oil refinery for further processing and sales to the food industry, as well as supplying the biodiesel industry globally (Youngkin). Perdue AgriBusiness also serves as a significant exporter, with its deep-water terminal in Norfolk,
         VA being one of only two crush plants in the US with a deep water and grain export terminal combination (Perdue AgriBusiness).
         ", align = "left"),
       p("Another notable player in Virginia's soybean industry is Montague Farms, based in Center Cross. This fourth-generation farm supplies specialty soybeans to Asian markets, including Japan and Taiwan, for products such as natto, tofu, soy milk, soy sauce, and oil (Farm Flavor). Other avenues for VA soybeans include Perdue Agribusiness’s crush facilities in Bainbridge, PA, Perdue Agribusiness’s crush facility in Salisbury, MD, and Cargil’s crush facility in Fayetteville, NC.
        ", align = "left"),
       p("Virginia's soybean industry is poised for significant growth with the upcoming construction of a new agricultural export facility in Portsmouth. The DeLong Co., Inc. has unveiled plans for a $26 million containerized agricultural export facility, set to break ground in February 2025 and become operational by late 2025 (World Grain). This state-of-the-art facility will be the only one of its kind on the US East Coast, capable of receiving unit trains and transloading products into containers for export (World Grain).
        ", align = "left"),
       p("The new DeLong facility in Portsmouth will feature 550,000 bushels of storage capacity and buildings for truck and rail receiving and loadout. It is expected to handle 15,000 to 20,000 containers annually, significantly enhancing agricultural exports through Portsmouth (Expansion Solutions Magazine). The facility will source whole grains and feedstuffs produced and processed locally and throughout the Midwest, including soybeans, corn, and wheat (Commonwealth Transportation Board). This development represents a major step in strengthening the economic, agricultural, and logistical landscape of Virginia and the greater East Coast to Midwest regions, opening new markets for agricultural producers and contributing to the area's long-term growth and success (World Grain).
        ", align = "left")

           ),

     tabPanel("Production",
              fluidRow(
                column(6,
                       h3("Domestic vs Exports", align = "left"),
                       p("In the early 2000s, soybean exports from Virginia were relatively modest. However, starting in the mid-2000s, exports began to rise steadily, suggesting a growing demand for Virginia's soybeans in international markets. This growth was especially pronounced during the period from 2006 to 2012, with Virginia farmers producing more than half a billion bushels of grain and soybeans between 2006 and 2012 (Virginia Department of Agriculture and Consumer Services). 
              ", align = "left"),
                      p("After 2013, exports experienced a temporary decline, which may have been influenced by market dynamics such as tariff policies, international trade disruptions, and changes in agricultural priorities. The 2018 Trade War between the United States and China likely contributed to this decline, as it had a significant impact on soybean exports nationwide (U.S. Department of Agriculture). 
",  align = "left"),
                      p("From 2016 onward, soybean exports began to rebound and saw a significant surge after 2020. This rapid increase could be attributed to improved trade relations, higher global demand for soybeans, and investments in export infrastructure. The surge was more dramatic than initially estimated, with soybeans becoming Virginia's top agricultural export in 2023, valued at over $1.4 billion (Virginia Department of Agriculture and Consumer Services). 
", align = "left"),
                      p("Several factors have contributed to this growth in Virginia's soybean exports. International demand has played a crucial role, with China becoming a major importer of Virginia's agricultural products, purchasing more than $912 million worth in 2023 (Virginia Department of Agriculture and Consumer Services). Additionally, Virginia's total soybean production has increased significantly, reaching 25,010,000 bushels on 610,000 harvested acres in 2022, up from 23,520,000 bushels on 560,000 harvested acres in 2020 (Virginia Soybean Association). 
", align = "left"),
                      p("The state has also diversified its export markets, with the top five export destinations in 2023 being China, Canada, United Kingdom, Taiwan, and Belgium (Virginia Department of Agriculture and Consumer Services). This diversification has likely contributed to the overall growth and stability of Virginia's soybean export industry. 
", align = "left")),
                column(5,
                       tags$div(style = "margin-top: 70px;",
                       plotOutput("domest")),
                column(1)       #extra space in the margins
                
              )),
              fluidRow(column(5,
                              h3("Bushel Production in Virginia and United States", align = "left"),
                              plotOutput("bupro")),
                       column(6,
                              tags$div(style = "margin-top: 90px;",
                              p("Virginia’s soybean production has shown significant growth and fluctuations since the early 2000s. From 2006 to 2012, Virginia farmers produced more than half a billion bushels of grain and soybeans, indicating a substantial increase in production (Virginia Department of Agriculture and Consumer Services). This growth trend continued, with total soybean production reaching 25,010,000 bushels on 610,000 harvested acres in 2022, up from 23,520,000 bushels on 560,000 harvested acres in 2020 (Virginia Soybean Association).
", align = "left"),
                              p("However, production can vary annually due to various factors. For instance, in 2023, soybean production in Virginia was forecast at 22.2 million bushels, down 11% from 2022, with yield estimated at 39 bushels per acre (USDA National Agricultural Statistics Service). This decline reflects the impact of external factors such as weather conditions and market fluctuations.
", align = "left"),
                              p("The increase in production over the years can be attributed to several factors, including improved genetics and production management, which have led to yield increases of about half a bushel per year (Country Folks). Farmers have also adapted to variable weather conditions by selecting earlier varieties and spreading out maturity groups (Country Folks).
", align = "left"),
                              p("In comparison to national trends, Virginia's soybean industry has shown significant growth, with soybeans becoming the state's most valuable crop and top agricultural export, valued at over $1.4 billion in 2023 (Virginia Department of Agriculture and Consumer Services). This aligns with the broader national trend of increased soybean farming, albeit on a smaller scale compared to major soybean-producing states.
", align = "left")))
                       
     ),
            fluidRow(column(6,
                            h3("Soybean and Corn in Virginia", align = "Left"),
                            tags$div(style = "margin-top: 50px;",
                            p("Soybeans planted in Virginia were estimated at 630,000 acres, up 50,000 acres from 2023. Acres harvested for grain, at
                              620,000 acres, was 50,000 acres above acres a year ago. U.S. soybean planted area for 2024 was estimated at 86.1 million
                              acres, up 3% from last year. Area for harvest, at 85.3 million acres, is up 4% from 2023.
", align = "left"),
                            p("Acreage planted to corn in Virginia was estimated at 500,000 acres, up 5,000 acres from 2023. Acres harvested for grain
                              was estimated at 380,000 acres, up 5,000 acres from last year. The U.S. corn planted for all purposes in 2024 was
                              estimated at 91.5 million acres, down 3% from last year. Growers expect to harvest 83.4 million acres for grain, down 4%
                                from last year.
", align = "left"),
                            p("Both have experienced steady shares in acreage throughout the years, however soybeans have always had slightly more acreage dedicated to them throughout Virginia. This likely means there are operations with the proper means for soybean harvesting, which incentivizes expanding business since many large expensive machinery are already available. 
", align = "left"))),
                     column(5,
                            tags$div(style = "margin-top: 50px;",
                            plotOutput("soycorx")
                            ),
                     column(1) #extra margin space
                     )),
          fluidRow(
                       column(5,
                              h3("Soybean Oil Supply", align = "left"),
                              plotOutput("fuelplot")),

                       column(6,
                              tags$div(style = "margin-top: 80px;",
                              p("Over the past 15 years, soybeans have seen large changes in soybean oil prices, most noticeably the large increase starting in 2019 and peaking in 2021. Increasing domestic demand for biofuels, shown in yellow on the graph, combined with supply chain disruptions and a variety of policy shifts from COVID-19, led to a sharp tightening of the soybean oil market and a significant rise in prices. Domestic soybean oil disappearance grew roughly 9 billion pounds from the 2009/10 to 2021/22 marketing years, with biofuels accounting for 96% percent of the growth (USDA).
                           ", align = "left"),
                              p("Future growth of soybean oil production is closely tied to the expansion of domestic soybean crushing capacity. As popularity for renewable diesel and other biofuels rise, capabilities to process enough for the increasing demand becomes vital. This could quickly turn into a bottleneck situation if not addressed properly. For Virginia and other areas, increasing the capacity and investing toward better technology could reduce overall costs and boost production and profits.
                                "))),
                       column(1)       #extra space in the margins)
            
            
            
            )),
     
     tabPanel("Yield",
              fluidRow(
                column(6,
                       h3("Soybean Yield", align = "left"),
                       p("Virginia's soybean yield has shown a remarkable trajectory of growth and adaptation since the 1980s, reflecting significant advancements in agricultural practices. In 2022, the state's total soybean production reached 25,010,000 bushels on 610,000 harvested acres, demonstrating substantial agricultural productivity (Virginia Department of Agriculture and Consumer Services). However, production is not without its challenges, as evidenced by the 2023 forecast of 22.2 million bushels, which represented an 11% decline from the previous year, with yields estimated at 39 bushels per acre (USDA National Agricultural Statistics Service). 
", align = "left"),
                       p("The variability in Virginia's soybean yields can be attributed to multiple interconnected factors, including complex weather conditions, particularly drought stress in regions with low water-holding capacity soils, and the state's diverse agricultural practices (Country Folks). Double-cropping systems, for instance, can reduce yields by 4 to 8 bushels per acre compared to full-season soybean cultivation, highlighting the nuanced challenges faced by Virginia's farmers (Virginia Cooperative Extension). Despite these obstacles, the agricultural community has demonstrated remarkable resilience and innovation, with improved genetics and production management contributing to annual yield increases of approximately half a bushel (Country Folks). 
", align = "left"),
                       p("The state's soybean industry has transformed into a critical economic sector, becoming Virginia's most valuable crop and top agricultural export, with a remarkable valuation of over $1.4 billion in 2023 (Virginia Department of Agriculture and Consumer Services). This economic significance reflects not just agricultural productivity, but also the industry's ability to adapt to changing market demands and environmental conditions. While Virginia's yields may not always match the national average, the state's soybean sector continues to play a crucial role in both regional and national agricultural landscapes. 
", align = "left"),
                       p("The continued growth and adaptation of soybean production in Virginia mirror broader national trends, showcasing the agricultural sector's capacity for innovation and resilience. As farmers continue to implement advanced cultivation techniques, select more robust crop varieties, and respond to environmental challenges, the future of soybean production in Virginia appears both promising and dynamic. 
", align = "left")),
                column(5,
                       tags$div(style = "margin-top: 70px;",
                       plotOutput("yield")),
                column(1)       #extra space in the margins
                
              )),
              fluidRow(
                column(5,
                       h3("Soybeans In Surrounding States", align = "left"),
                       plotOutput("statesx")),
                column(6,
                       img(src = "kabless.png", style = "display: block; margin: 90px auto 20px auto; border: 1px solid #C0C0C0;", width = "500px"),
                       tags$div(style = "margin-top: 50px;",
                       p("Above shows the percentage yield differences for the states displayed in the graph above. All states experienced growth in soybean yield, however Virginia experienced the most growth with about a 83% increase from 2010 to 2024. This is likely due to economic factors within Virginia, such as biofuel, renewable energy mandates, and increased exports, that have pushed producers toward investing more in this crop throughout Virginia. 
", align = "left"))
                       
                    
              )
              
     ),
             fluidRow(
               column(5,
                      h3("Soybean Output"),
                      tags$div(style = "margin-top: 30px;",
                               plotOutput("lossx"))),
               column(6,
                      tags$div(style = "margin-top: 140px;",
                      p("In the United States, the difference between planted soybeans and harvested soybeans is little to none, which a few minor acceptions. The gap between planted and harvested is narrow, meaning the majority of farmers in the United States sucessfully raise their crops to harvest. However, there are always additional exogenous ecents that can cause this gap to be larger, such as unfavorable weather, pests, or market conditions.
                        "), 
                        p("As reflected in the graph, the U.S, soybean industry is highly efficient, with advanced techniques and technology that keep planted and harvested ratios at an acceptional level. This consistent ratio speaks of the effectiveness of soybean farmers, and reflects the United States status as one of the largest poybean producers in the world.
                          ")))
             )   
     
     ),
     tabPanel("Exports",
              fluidRow(
                column(6,
                       h3("Soybean Exports", align = "left"),
                       p("Soybeans have considerably grown in the export market since the early 2000s, with about an 83.2% increase from 2000 to 2024. Soybeans have substantially grown in popularity, specifially the products derived from soybeans. Soybean meal and oil are used in a variety of goods, including bioplastics, cooking oils, livestock feed, and biofuels. The United States has been experiencing consistent growth in exports, save for the year 2019, which was likely due to market disruptions from COVID-19. About 55% of the United States' soybean exports go to China, however this steady market may decrease over the years as China makes moves to increase its deomestic production of soybeans to reduce reliance on imports, as stated in their Five-Year Agricultural Plan.
                         ", align = "left"),
                       p("Soybean export amounts give insight to the overall health of global trade, including competitiveness and relationships. By understanding the United States' position in the international soybean market through export amounts and where they are ultimately being shipped, agricultural producers and firms can makes trategic deciscions to maximize opportunities. As mentioned with China before, strong exports can signal economic strength and geopolitical ties. Virginia producers can use this information to potentially grow their roll in soybean production, and ultimately increase their standing as a soybean exporter. One of Virginia's key assets is the Port of Virginia, which greatly increases Virginia's ability to participate in global markets. For producers and policymakers in Virginia, this gateway is essential for expanding global infrastructure.
                         ", align = "left")),
                column(5,
                       tags$div(style = "margin-top: 30px;",
                                plotOutput("exportsy"))
              )),
              fluidRow(  
                column(5,
                       tags$div(style = "margin-top: 30px;",
                                plotOutput("pacex"))),
                column(6,
                       h3("Pace of Soybean Exports", align = "left"),
                       p("The pace of exports is a solid signal of how the market of a commodity is fairing. U.S. soybean exports' demand can shift according to the demand of other countries, especially China (U.S. number one soybean importer). The graph to the left shows how fast the exports reach to 100% of the exports sold and shipped for the given market year. The 2024/25 market year total amount is assuming the published WASDE estimates hold true. Other factors that can affect the pace of exports include trade policies, exchange rates, and supply chain disruptions. 
                         ", align = "left"),
                       p("Based on the previous years' values (2022 and 2023), the 2024/25 marketing year is off to a strong start, with significantly higher values of exports throughout the second quarter of weeks (week 13.25-26.5) and eventually lining back up with the percentage of total exports in 2022 where the data cuts off. The slow down on exports could be due to increased tariffs on US soybeans imposed by China (10% tariff in the beginning of March), making it difficult on farmers. There have been pressures of tariffs for many other industries, and data supports this by showing exports slowing slightly during thie more recent weeks. Overall, the US had a strong start in soybean exports, but with the new tariffs on agricultural products on the US's biggest soybean buyer and increasingly competitive prices from Brazil, exports may begin to decrease later in the year. 
", align = "left"))

                
              )
     ),
  )),
           
  
  ## 2.3 Interactive Map------
  tabPanel("Interactive Map",
           h1(strong("Soybeans in Virginia"), align = "center"),
           
           h1(strong("Crushing Mills & Grain Elevators"), align = "left", style = "font-size: 18px;"),
           p("The inclusion of both crushing mills and grain elevators on the map provides valuable context for understanding how soybean infrastructure aligns with regional production patterns. By overlaying these key facilities with county-level data, users can quickly identify imbalances between supply and processing capacity. These disparities offer insights into potential inefficiencies and transportation costs, which are critical considerations for producers and distributors alike. Ultimately, this analysis can help inform strategic decisions, such as where new crushing facilities or storage infrastructure could be developed, to support high-production areas and strengthen Virginia’s overall soybean supply chain efficiency.", 
             align = "left"),
           
           h1(strong("Additional Facilities"), align = "left", style = "font-size: 18px;"),
           p("The new carbon-neutral Lego factory will be in Richmond, Virginia, and is set to open in 2025 (Lego). Lego has made clear outlines in their plans to use more sustainable materials in their resin they make the lego bricks out of. This resin is made from a mix of certified renewable or recycled raw materials, such as used cooking or plant oils, and virgin fossil (Lego). Soybean oil is a byproduct of soybeans, which makes this such a great opportunity for soybean producers.

DeLong is building an agricultural export facility in Portsmouth, Virginia, and has hopes of operations beginning by late 2025. The facility will feature a storage capacity of 550,000 bushels in addition to trucking and rail receiving and loadouts (WorldGrain). This is possible with the collaboration of CSX and the Port of Virginia.  This facility will be a hotspot for both production locally and throughout the midwest. Expanding agricultural exports in Virginia is a large incentive for local producers, as it cuts back hefty transportation costs. ", align = "left"),
           
           fluidRow(
             column(3, 
                    h1(strong("Features"), align = "left", style = "font-size: 18px;"),
                    p("This interactive map allows users to explore trends in soybean production across Virginia counties over the past five years (2019–2023). It provides a visual comparison of three key agricultural metrics: Acres Harvested, Total Bushels Harvested, and Productivity (Bushels per Acre).", align = "left"),
                    p("At the top of the map, radio buttons allow users to select a specific year, helping them observe how production metrics have evolved annually. The dropdown menu on the left lets users switch between variables, offering flexibility to analyze both total output and land-use efficiency.", align = "left"),
                    p("The goal of the map is to highlight regional strengths and disparities in soybean production, enabling researchers, farmers, and policymakers to identify high-performing areas, track year-over-year changes, and support data-driven agricultural planning.", align = "left")
             ),
             column(9,
                    tags$div(style = "margin-top: 40px;",
                    selectInput("demographics", "Select Variable:", width = "50%", choices = c(
                      "Acres Harvested" = "Acres",
                      "Bushels Harvested" = "Bushels",
                      "Productivity" = "Productivity"
                    )
                    )),
                    radioButtons(inputId = "yearSelect_demo", label = "Select Year: ", 
                                 choices = c("2019","2020", "2021", "2022", "2023"), 
                                 selected = "2023", inline = TRUE),
                    withSpinner(leafletOutput("demographicsvar", height = "800px")),
             )
             
           )

           
  ),
  
  
  # tabPanel("Future Prospects",
  #          fluidRow(h1(strong("Forecasting Projects"), align = "center"),
  #                   p("information about projects here... maybe stocks to use assignment 5.2"),
  #                   )),
  
           
             ## 2.6 Tab Data Sources --------------------------------------------
  tabPanel("Data Sources", 
           fluidRow(style = "margin: 6px;",
                    h1(strong("Data Sources"), align = "center"),
                    p("", style = "padding-top:10px;"),
                    fluidRow(style = "margin: 6px;", align = "left",
                             column(3,
                                    img(src = "usda_quickstats_logo.jpg", style = "display: inline; float: left; width: 100%;", height = "270px"),
                                    p(strong("USDA Quick Stats"), 
                                      "The USDA Quick Stats database is the primary source for county, state, and national level data on soybean acreage, yield, and production, ultimately helping to visualize Virginia's soybean production and its comparison to national trends."
                                    )
                             ),
                             column(3,
                                    img(src = "wasde_logo.jpg", style = "display: inline; float: left; width: 100%;", height = "270px"),
                                    p(strong("World Agricultural Supply and Demand Estimates (WASDE)"), 
                                      "The WASDE reports provide monthly projections of U.S. and international agricultural supply and demand, including soybean production, exports, and prices. We used WASDE data to understand national and global market dynamics and the role the United States plays in this broader context."
                                    )
                             ),
                             column(3,
                                    img(src = "usda_fas_logo.jpg", style = "display: inline; float: left; width: 100%;", height = "270px"),
                                    p(strong("USDA Foreign Agricultural Service (FAS)"), 
                                      "The USDA Foreign Agricultural Service tracks U.S. agricultural exports, including soybeans. This data allowed us to assess The United States’ soybean export performance and analyze key markets, as well as identify export trends to major soybean consumers."
                                    )
                             ),
                             column(3,
                                    img(src = "farmdocdaily_logo.jpg", style = "display: inline; float: left; width: 100%;", height = "270px"),
                                    p(strong("FarmDoc Daily - The United States, Brazil, and China Soybean Triangle"), 
                                      "FarmDoc Daily provided a critical analysis of the shifting relationships between the U.S., Brazil, and China in the global soybean market. This article enriched our understanding of global trade patterns and the competitive dynamics that shape soybean exports."
                                    ),
                                    p(a(href = "https://farmdocdaily.illinois.edu/2024/02/the-united-states-brazil-and-china-soybean-triangle-a-20-year-analysis.html#:~:text=A%20bumper%20soybean%20crop%20and%20lower%20prices,reaching%20a%20record%20of%202%2C737%20million%20bushels.&text=In%20the%20last%20few%20years%2C%20American%20farmers,their%20reliance%20on%20China%20compared%20to%20Brazil.", 
                                        "Read the full article here.", target = "_blank"))
                             )
                    )
           )
  ),
  
  
  
  
  ## 2.5 Tab Team --------------------------------------------
  tabPanel("Meet the Team", 
           fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                    align = "center",
                    h1(strong("Meet the Team")),
                    br(),
                    h4(strong("Virginia Tech's Kohl Centre Junior Fellows")),
                    p("The ", a(href = 'https://aaec.vt.edu/centers/index.html', 'Kohl Centre', target = "_blank"),
                      " is a program within Virginia Tech's Department of Agricultural and Applied Economics, dedicated to providing students with immersive, real-world experiences in policy analysis, agribusiness, and community development. The Centre’s mission is to encourage hands-on learning by empowering students to engage directly with the complex economic challenges facing agriculture and rural communities."),
                    p("This project is part of the Kohl Centre’s Junior Fellows program, which supports undergraduate and graduate level research. The investigation of Virginia’s soybean production and infrastructure was made possible thanks to the Centre’s generous financial sponsorship and academic guidance."),
                    p("We are deeply grateful to the Kohl Centre for believing in this research and for giving us the tools, space, and support to explore the future of Virginia’s soybean economy.")
                    ,
           fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                    column(6, align = "center",
                           h4(strong("Junior Fellows")),
                           img(src = "kealyn.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "500px"),
                           img(src = "billy.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "500px"),
                           br(),
                           p(a(href = 'https://www.linkedin.com/in/kealyngreenwell/', 'Kealyn Greenwell', target = '_blank'), "(Virginia Tech, Graduate Student in Agricultural & Applied Economics);",
                             br(), 
                             a(href = 'https://www.linkedin.com/in/billy-stoneman-342179265/', 'Billy Stoneman', target = '_blank'), "(Virginia Tech, Undergraduate in Agribusiness)"),
                           br(), 
                           p("", style = "padding-top:10px;"),
                      
                    ),
                    column(6, align = "center",
                           h4(strong("VT Faculty Members")),
                           img(src = "Mario.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "500px"),
                           br(),
                           p(a(href = "https://www.linkedin.com/in/mario-a-ortez-9a542148/", 'Dr. Mario Ortez', target = '_blank'), "(Assistant Professor of Agribusiness & Entrepreneurship);",
                             br(), 
                             a(href = 'https://aaec.vt.edu/academics/undergraduate/dspg/team.html', 'Dr. Michael Cary', target = '_blank'), "(Research Assistant Professor of Agricultural and Applied Economics)" ),
                           p("A special thank you to Dr. Cary for his guidance and technical support in helping us design this webpage. His mentorship was crucial to making this project a success."),
                           br()
                           
                    )),
           inverse = T)))
           


# 3. Define server logic  ------------------------------------------------------------
server <- function(input, output) {
  # Execute the JavaScript code when the app is loaded--> for the DSPG logo
  shinyjs::runjs(jscode)
  

  
  temp_demo <- reactive({
    input$demographics
  })
  temp_demoyear <- reactive({
    as.integer(input$yearSelect_demo)
  })
  
  output$demographicsvar <- renderLeaflet({
    mapping2(temp_demo(), temp_demoyear())
  })
  
  output$fuelplot <- renderPlot({
    fuels
  })
  
  output$domest <- renderPlot({
    domestic
  })
  
  output$bupro <- renderPlot({
    prodx
  })
  
output$soycorx <- renderPlot({
  cands
})

output$yield <- renderPlot({
  yieldx
})

output$statesx <- renderPlot({
  statey
  
})
  
output$pacex <- renderPlot({
  pacey
})

output$exportsy <- renderPlot({
  exportsx
})
  
output$lossx <- renderPlot({
  lossy
})
    
}

# 4. Run the application-------------------------------------------------------------------

shinyApp(ui = ui, server = server)

