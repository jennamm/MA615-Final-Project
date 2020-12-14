library(shiny)
library(tidyr)
library(dplyr)
library(sf)
library(tmap)
library(tmaptools)
library(ggplot2)
library(tidyverse)

boston_data <- st_read("c7230a7a-4081-4743-b911-e18f66e1beca2020330-1-17gw6be.a4ds.shp")
measurements <- read.csv("Measurements_date.csv")
data <- read.csv("data.csv")

# Turn the data frame into sf data
epsg_wgs84 <- 4326
measurements %<>% st_as_sf(coords = c("Longitude", "Latitude")) %>% st_set_crs(epsg_wgs84)
# Make sure that measurements data has the same crs with boston_data
epsg_wgs84 <- 3857
measurements %<>% st_as_sf(coords = c("Longitude", "Latitude")) %>% st_transform(epsg_wgs84)
# Find the points of noise measurements that are in the Boston area
measurements$join <- lengths(st_intersects(measurements, boston_data))
measurements <- subset(measurements, measurements$join == 1)
joindata <- boston_data %>% st_join(measurements)
# Separating the first word in the "Genre" column - we will only use the first word
df_first_word <- separate(measurements, c("Sources"), "Sources")
df <- df_first_word[!df_first_word$Sources == "other", ]
# All
df$fac <- factor(df$Sources, ordered = TRUE, levels = c("quiet", "footsteps", "voices", "neighbor", "restaurant",
                                                        "hvac", "traffic", "trash", "pickup", "delivery", "dog",
                                                        "leaf", "car", "music", "trains", "party", "construction",
                                                        "alarm", "horn", "airplane", "fireworks"))
# Quiet
df1 <- df[df$Sources == "quiet", ]
df1$fac <- factor(df1$Sources, ordered = TRUE, levels = "quiet")
# Footsteps
df2 <- df[df$Sources == "footsteps", ]
df2$fac <- factor(df2$Sources, ordered = TRUE, levels = "footsteps")
# Voices
df3 <- df[df$Sources == "voices", ]
df3$fac <- factor(df3$Sources, ordered = TRUE, levels = "voices")
# Neighbor
df4 <- df[df$Sources == "neighbor", ]
df4$fac <- factor(df4$Sources, ordered = TRUE, levels = "neighbor")
# Restaurant
df5 <- df[df$Sources == "restaurant", ]
df5$fac <- factor(df5$Sources, ordered = TRUE, levels = "restaurant")
# Hvac
df6 <- df[df$Sources == "hvac", ]
df6$fac <- factor(df6$Sources, ordered = TRUE, levels = "hvac")
# Traffic
df7 <- df[df$Sources == "traffic", ]
df7$fac <- factor(df7$Sources, ordered = TRUE, levels = "traffic")
# Trash
df8 <- df[df$Sources == "trash", ]
df8$fac <- factor(df8$Sources, ordered = TRUE, levels = "trash")
# Pickup
df9 <- df[df$Sources == "pickup", ]
df9$fac <- factor(df9$Sources, ordered = TRUE, levels = "pickup")
# Delivery
df10 <- df[df$Sources == "delivery", ]
df10$fac <- factor(df10$Sources, ordered = TRUE, levels = "delivery")
# Dog
df11 <- df[df$Sources == "dog", ]
df11$fac <- factor(df11$Sources, ordered = TRUE, levels = "dog")
# Leaf Blower
df12 <- df[df$Sources == "leaf", ]
df12$fac <- factor(df12$Sources, ordered = TRUE, levels = "leaf")
# Car Music
df13 <- df[df$Sources == "car", ]
df13$fac <- factor(df13$Sources, ordered = TRUE, levels = "car")
# Music
df14 <- df[df$Sources == "music", ]
df14$fac <- factor(df14$Sources, ordered = TRUE, levels = "music")
# Trains
df15 <- df[df$Sources == "trains", ]
df15$fac <- factor(df15$Sources, ordered = TRUE, levels = "trains")
# Party
df16 <- df[df$Sources == "party", ]
df16$fac <- factor(df16$Sources, ordered = TRUE, levels = "party")
# Construction
df17 <- df[df$Sources == "construction", ]
df17$fac <- factor(df17$Sources, ordered = TRUE, levels = "construction")
# Alarm
df18 <- df[df$Sources == "alarm", ]
df18$fac <- factor(df18$Sources, ordered = TRUE, levels = "alarm")
# Horn
df19 <- df[df$Sources == "horn", ]
df19$fac <- factor(df19$Sources, ordered = TRUE, levels = "horn")
# Airplane
df20 <- df[df$Sources == "airplane", ]
df20$fac <- factor(df20$Sources, ordered = TRUE, levels = "airplane")
# Fireworks
df21 <- df[df$Sources == "fireworks", ]
df21$fac <- factor(df21$Sources, ordered = TRUE, levels = "fireworks")



# Define UI for application that draws a histogram
ui <- fluidPage(
    mainPanel(
        tabsetPanel(
            tabPanel("Map for Any Source of Noise",
                     tmapOutput("my_tmap"),
                     fluidRow(
                         column(4,
                                selectInput("Sources",
                                            "Source:",
                                            c("All",
                                              unique(df$Sources))))
                         )
                     )
            )
        
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    if(input$Sources == "All") {
        output$my_tmap <- renderTmap({
            tm_shape(boston_data) +
                tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
                tm_shape(df) +
                tm_dots(col = 'fac', size = 0.02, alpha = 0.5, palette = "Accent", title = 'Average Noise Score') +
                tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score by Source')
        })
    } else {
        output$my_tmap <- renderTmap({
            tm_shape(boston_data) +
                tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
                tm_shape(df, main = input$Sources) +
                tm_dots(col = 'faq', size = 0.02, alpha = 0.5, palette = "Accent", title = 'Average Noise Score') +
                tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score by Source')
        })
    }
    
    
    # # All
    # output$my_tmap_all <- renderTmap({
    #     tm_shape(boston_data) +
    #         tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
    #         tm_shape(df, main = input$Sources) +
    #         tm_dots(col = 'fac', size = 0.02, alpha = 0.5, palette = "Accent", title = 'Average Noise Score') +
    #         tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score by Source')
    # })
    # # Quiet
    # output$my_tmap1 <- renderTmap({
    #     tm_shape(boston_data) +
    #         tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
    #         tm_shape(df1) +
    #         tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "red", title = 'Average Noise Score') +
    #         tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (Quiet)')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
