library(shiny)
library(tidyr)
library(dplyr)
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(packrat)

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
# 20 dB
df1 <- df[df$Sources == "quiet", ]
df1$fac <- factor(df1$Sources, ordered = TRUE, levels = "quiet")
# 50 dB
df2 <- df[df$Sources == "footsteps", ]
df2$fac <- factor(df2$Sources, ordered = TRUE, levels = "footsteps")
# 60 dB
df3 <- df[df$Sources == c("voices", "neighbor", "restaurant", "hvac"), ]
df3$fac <- factor(df3$Sources, ordered = TRUE, levels = c("voices", "neighbor", "restaurant", "hvac"))
# 70 dB
df4 <- df[df$Sources == "traffic", ]
df4$fac <- factor(df4$Sources, ordered = TRUE, levels = "traffic")
# 80 dB
df5 <- df[df$Sources == c("trash", "pickup", "delivery", "dog"), ]
df5$fac <- factor(df5$Sources, ordered = TRUE, levels = c("trash", "pickup", "delivery", "dog"))
# 90 dB
df6 <- df[df$Sources == "leaf", ]
df6$fac <- factor(df6$Sources, ordered = TRUE, levels = "leaf")
# 100 dB
df7 <- df[df$Sources == c("car", "music", "trains"), ]
df7$fac <- factor(df7$Sources, ordered = TRUE, levels = c("car", "music", "trains"))
# 110 dB
df8 <- df[df$Sources == c("party", "construction", "alarm", "horn"), ]
df8$fac <- factor(df8$Sources, ordered = TRUE, levels = c("party", "construction", "alarm", "horn"))
# 120 dB
df9 <- df[df$Sources == "airplane", ]
df9$fac <- factor(df9$Sources, ordered = TRUE, levels = "airplane")
# 140 dB
df10 <- df[df$Sources == "fireworks", ]
df10$fac <- factor(df10$Sources, ordered = TRUE, levels = "fireworks")



# Define UI for application that draws a histogram
ui <- fluidPage(
    mainPanel(
        tabsetPanel(
            tabPanel("All Map",
                     tmapOutput("my_tmap_all")
            ),
            tabPanel("20 dB Map",
                     tmapOutput("my_tmap1")
            ),
            tabPanel("50 dB Map",
                     tmapOutput("my_tmap2")
            ),
            tabPanel("60 dB Map",
                     tmapOutput("my_tmap3")
            ),
            tabPanel("70 dB Map",
                     tmapOutput("my_tmap4")
            ),
            tabPanel("80 dB Map",
                     tmapOutput("my_tmap5")
            ),
            tabPanel("90 dB Map",
                     tmapOutput("my_tmap6")
            ),
            tabPanel("100 dB Map",
                     tmapOutput("my_tmap7")
            ),
            tabPanel("110 dB Map",
                     tmapOutput("my_tmap8")
            ),
            tabPanel("120 dB Map",
                     tmapOutput("my_tmap9")
            ),
            tabPanel("140 dB Map",
                     tmapOutput("my_tmap10")
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    # All
    output$my_tmap_all <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df, main = input$Sources) +
            tm_dots(col = 'fac', size = 0.02, alpha = 0.5, palette = "Accent", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score by Source')
    })
    # 20 dB
    output$my_tmap1 <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df1) +
            tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "red", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (20 dB)')
    })
    # 50 dB
    output$my_tmap2 <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df2) +
            tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "red", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (50 dB)')
    })
    # 60 dB
    output$my_tmap3 <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df3) +
            tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "Dark2", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (60 dB)')
    })
    # 70 dB
    output$my_tmap4 <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df4) +
            tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "red", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (70 dB)')
    })
    # 80 dB
    output$my_tmap5 <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df5) +
            tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "Dark2", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (80 dB)')
    })
    # 90 dB
    output$my_tmap6 <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df6) +
            tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "red", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (90 dB)')
    })
    # 100 dB
    output$my_tmap7 <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df7) +
            tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "Dark2", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (100 dB)')
    })
    # 110 dB
    output$my_tmap8 <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df8) +
            tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "Dark2", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (110 dB)')
    })
    # 120 dB
    output$my_tmap9 <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df1) +
            tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "red", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (120 dB)')
    })
    # 140 dB
    output$my_tmap10 <- renderTmap({
        tm_shape(boston_data) +
            tm_polygons('MedIllnes', palette = "YlGn", title = "Medical Illness") +
            tm_shape(df1) +
            tm_dots(col = 'fac', size = 1, alpha = 0.5, palette = "red", title = 'Average Noise Score') +
            tm_layout(main.title = 'Boston Vulnerability (Medical Illness) and Average Noise Score (140 dB)')
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
