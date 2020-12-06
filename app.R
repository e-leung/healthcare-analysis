library(shiny)
library(tidyverse)
library(leaflet)
library(geojsonio)
#require(devtools)
library(readxl)
#install_version("colorspace", version = "1.4.1", repos = "http://cran.us.r-project.org")

medicaid.data <- read.csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/finalproj/medicaid_data.csv")

state_health <- read.csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/finalproj/states.csv")
states <- geojson_read("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/finalproj/gz_2010_us_040_00_5m.json",
                       what = "sp")

ethnicity.data <- read.csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/finalproj/ethnicitydata.csv")

ethnicity.data <- ethnicity.data[1:3]

subset(ethnicity.data, rownames(ethnicity.data) %in% c("White", "Black"))

state_health$State <- state_health$State %>%
  str_replace_all("[:space:]$", "")

states@data <- left_join(states@data, 
                         state_health,
                         by = c("NAME" = "State"))

states@data$Uninsured.Rate.Change..2010.2015.<- states@data$Uninsured.Rate.Change..2010.2015. %>%
  str_replace_all("%", "")

medicaid.data <- medicaid.data %>%
  mutate(coverage.change.2016.2019 = X2019__Medicaid - X2016__Medicaid)

states@data$Uninsured.Rate.Change..2010.2015. <- as.numeric(states@data$Uninsured.Rate.Change..2010.2015.)
bin <- c(-15, -10, -8, -6, -4, -2, 0)
colors <- colorBin(palette = "RdYlBu",
                   domain = states@data$Uninsured.Rate.Change..2010.2015.,
                   bins = bin)

bin1 <- c(-0.04, -0.02, -0.01, 0, 0.01, 0.02, 0.04)
colors1 <- colorBin(palette = "YlOrRd",
                    domain = medicaid.data$coverage.change.2016.2019,
                    bins = bin1)

states@data <- states@data %>%
  mutate(Accepted.Expansion = ifelse(states@data$State.Medicaid.Expansion..2016. == "True",
                                     "accepted",
                                     "rejected"))

states@data$Uninsured.Rate..2015.<- states@data$Uninsured.Rate..2015. %>%
  str_replace_all("%", "")
states@data$Uninsured.Rate..2015.<- as.numeric(states@data$Uninsured.Rate..2015.)
medicaid.summary <- states@data %>%
  group_by(Accepted.Expansion) %>%
  summarise(mean.uninsured = mean(Uninsured.Rate..2015.))
medicaid.summary <- head(medicaid.summary, 2)

deductibles <- read.csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/finalproj/deductibles.csv")
names(deductibles)[names(deductibles)=="X.1.000.or.more"] <- "Percentage"
names(deductibles)[names(deductibles)=="X."] <- "Year"

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Introduction",
             h4("Made by Jackson Chen, Bryan Brito-Martinez, Eric Leung"),
             h4("Our project will examine how health insurance coverage in
             the United States has changed over the past decade,
             since the passing of the Affordable Care Act (ACA). Since 2010,
             healthcare has been a ")),
    tabPanel("Uninsured Rate Change between 2010 and 2015",
             leafletOutput(outputId = "leaflet1", 
                           height = 500)),
    tabPanel("States that accepted the 2016 Medicaid Expansion",
             leafletOutput(outputId = "leaflet2", 
                           height = 500)),
    tabPanel("Changes in uninsured rates by race and ethnicity",
             checkboxGroupInput("input1", "Choose a race/ethnicity", 
                         choices = c("Total", "White", "Black", "Hispanic",
                                     "Asian/Native Hawaiian and Pacific Islander",
                                     "American Indian/Alaska Native",
                                     "Multiple Races"),
                         selected = "Total"),
             plotOutput(outputId = "plot1"))
  )
)

server <- function(input, output, session) {
  output$leaflet1 <- renderLeaflet({
    states %>%
      leaflet() %>%
      addPolygons(fillColor = ~colors(Uninsured.Rate.Change..2010.2015.),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  label = paste(states@data$NAME, "had a",
                                states@data$Uninsured.Rate.Change..2010.2015.,
                                "percent change")) %>%
      setView(-135, 50, 3) %>%
      addLegend(pal = colors,
                title = "Uninsured rate change from 2010-2015",
                values = states@data$Uninsured.Rate.Change..2010.2015.)
  })
  output$leaflet2 <- renderLeaflet({
    states %>%
      leaflet() %>%
      addPolygons(fillColor = ~colors1(medicaid.data$coverage.change.2016.2019),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  label = paste(states@data$NAME, states@data$Accepted.Expansion, 
                                "the 2016 Medicaid Expansion")) %>%
      setView(-135, 50, 3) %>%
      addLegend(pal = colors1,
                title = "% change in total Medicaid coverage",
                values = medicaid.data$coverage.change.2016.2019)
  })
  output$plot1 <- renderPlot({
    ethnicity.data1 <- subset(ethnicity.data, ethnicity.data$Ethnicity %in% input$input1)
    ethnicity.data1 %>%
      ggplot(aes(x = factor(Year),
                 y = Uninsured.Rate,
                 group = Ethnicity,
                 color = Ethnicity)) +
      labs(x = "Year", y="Uninsured Rate") +
      geom_point() +
      geom_line() +
      theme_minimal()
  })
}

shinyApp(ui, server)