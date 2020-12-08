library(shiny)
library(tidyverse)
library(leaflet)
library(geojsonio)
#require(devtools)
library(readxl)
library(stringr)
#install_version("colorspace", version = "1.4.1", repos = "http://cran.us.r-project.org")

#https://www.kff.org/other/state-indicator/total-population/
medicaid.data <- read.csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/finalproj/medicaid_data.csv")

state_health <- read.csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/finalproj/states.csv")
states <- geojson_read("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/finalproj/gz_2010_us_040_00_5m.json",
                       what = "sp")

ethnicity.data <- read.csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/finalproj/ethnicitydata.csv")

ethnicity.data <- ethnicity.data[1:3]

#https://www.kff.org/other/state-indicator/nonelderly-up-to-100-fpl
poverty.data <- read.csv("/Users/ericleung/Desktop/Midd Year 4/Semester 1/MATH 216/finalproj/povertylevel.csv")
poverty.data <- poverty.data[, -1]


state_health$State <- state_health$State %>%
  str_replace_all("[:space:]$", "")

states@data <- left_join(states@data, 
                         state_health,
                         by = c("NAME" = "State"))

states@data$Uninsured.Rate.Change..2010.2015.<- states@data$Uninsured.Rate.Change..2010.2015. %>%
  str_replace_all("%", "")

medicaid.data <- medicaid.data %>%
  mutate(coverage.change.2016.2019 = round((X2019__Medicaid - X2016__Medicaid)*100, digits = 2))

states@data$Uninsured.Rate.Change..2010.2015. <- as.numeric(states@data$Uninsured.Rate.Change..2010.2015.)
bin <- c(-15, -10, -8, -6, -4, -2, 0)
colors <- colorBin(palette = "RdYlBu",
                   domain = states@data$Uninsured.Rate.Change..2010.2015.,
                   bins = bin)

bin1 <- c(-4, 0, 4)
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
             h5("Made by Jackson Chen, Bryan Brito-Martinez, Eric Leung"),
             
             p("Brief overview of the Affordable Care Act: The ACA had numerous impacts
               on the fast food, pharmaceutical, and of course, healthcare industries. For example,
               it made fast food restaurants display how many calories are in their food, allowing
               consumers to make smarter decisions. It forbade insurance companies from discriminating
               based on having a disability, or charging more due to pre-existing conditions. Most notably,
               it opened enrollment in Medicaid to everyone making up to 133% of the federal poverty line. 
               Businesses with over 50 employees must also offer health insurance to full-time employees or
               pay a fee.
               Additionally, it enacted a 'mandate' for insurance, meaning people who could 
               afford health insurance but did not enroll would be charged a fee."),
             
             p("Our project will examine how health insurance coverage in
             the United States has dramatically changed over the past decade,
             since the landmark passing of the Affordable Care Act (ACA). Since 2010,
             healthcare has been a pressing issue with the passing of the ACA,
             the Medicare For All Movement beginning in 2015, and the COVID-19
             pandemic. Our data will examine how uninsurance rates have changed 
             since the passing of the ACA, the effects of the Medicaid expansion
             in individual states, how different ethnic groups have different
             rates of uninsurance, and how the uninsured rate for people with 
             incomes below the federal poverty line decreased since 2010.")),
    tabPanel("Uninsured Rate Change between 2010 and 2015",
             leafletOutput(outputId = "leaflet1", 
                           height = 500),
             p("In the map above, we can see that there has been a decrease in the uninsured rate
                across the board in all 50 states. Oregon, California, Nevada, New Mexico,
                Kentucky, and West Virginia had the greatest decreases in uninsured rates between
                2010 and 2015 while North Dakota, Maine, and Massachusetts had the smallest change.
                So, since 2010, there has been a widespread decrease in uninsured rates throughout
                the country.")),
    tabPanel("States that accepted the 2016 Medicaid Expansion",
             p("We see that there has been an overall decrease in Medicaid 
                coverage throughout the 50 states. The change in Medicaid
                coverage could be due to people who were previously on Medicaid
                moving to employer insurance or to Medicare. The ACA mandate could
                also be a reason people are moving away from Medicaid and onto a
                private insurance plan. Below, we have a bar chart showing that
                states that accepted the Medicaid expansion have a lower uninsured rate,
                on average. This makes sense because the expansion opened Medicaid
                to people making 133% of the federal poverty line."),
             leafletOutput(outputId = "leaflet2", 
                           height = 500),
             p("Washington, Arizona, and Maine had the greatest increase to Medicare
               coverage, with 2% or greater increases. However, Maine rejected the
               expansion, indicating that other variables are at play."),
             plotOutput(outputId = "plot2")),
    tabPanel("Changes in uninsured rates by race and ethnicity",
             p("The Brookings Institution, a leading economic think tank, stated that
                'People of color are far more likely to be uninsured in America, 
                due in part to several states’ refusal to expand Medicaid.' To explore
                this statement further, we look at uninsured rates of different ethnic
                and racial groups in the US."),
             checkboxGroupInput("input1", "Choose a race/ethnicity", 
                                choices = c("Total", "White", "Black", "Hispanic",
                                            "Asian/Native Hawaiian and Pacific Islander",
                                            "American Indian/Alaska Native",
                                            "Multiple Races"),
                                selected = "Total"),
             plotOutput(outputId = "plot1"),
             p("For all ethnicities and races, we see atleast a 5-10% decrease in
               uninsured rates but Hispanics and American Indians'Alaska Natives saw a significant decrease
               in uninsured rates, almost 20%."),
             p("The largest decrease happened in 2013, which is mainly attributed to the major provisions
             of the ACA going into effect and the economy improving since the 2008 Great Recession.
               ")),
    tabPanel("Changes in coverage for people below the FPL",
             p("Looking at data from the KFF, we see that the healthcare coverge of 
               nonelderly people with incomes below the federal poverty line has increased
               significantly since 2010."),
             p("Healthcare coverage increased with an almost 10% increase in Medicaid coverage and
               a minor increase in employer coverage. Uninsured rates for this group decreased almost 
               at the same rate coverage by Medicaid increased, indicating that most people with incomes
               below the federal poverty line were able to enroll in Medicaid after the ACA provisions went
               into effect."),
             plotOutput(outputId = "plot3")),
    tabPanel("Conclusion and Discussion",
             p("The healthcare industry has changed significantly since the passing of the
             ACA in 2010. Uninsured rates have fallen in all 50 states, presumably due to
             the ACA mandate and Medicaid expansion. However, our dataset does not provide a
             conclusive answer to whether or not the Medicaid expansion helped drive down
             uninsured rates because Medicaid coverage varies depending on state. However,
             there is clear evidence that people living under the poverty line and people of color
             have lower uninsured rates now than they did in 2010.")
    )
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
                title = "Uninsured rate change from 2010-2015 (percentages)",
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
                                "the 2016 Medicaid Expansion and had a ",
                                medicaid.data$coverage.change.2016.2019, "%
                                change in Medicaid coverage between 2016 and 2019",
                                sep = "\n")) %>%
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
  output$plot2 <- renderPlot({
    medicaid.summary %>%
      ggplot(aes(x = Accepted.Expansion,
                 y = mean.uninsured,
                 fill = Accepted.Expansion)) +
      geom_col() +
      theme_minimal()
  })
  output$plot3 <- renderPlot({
    poverty.data %>%
      ggplot(aes(x = factor(Year),
                 y = Coverage,
                 group = Type,
                 color = Type)) +
      geom_line() +
      geom_point() +
      labs(x = "Year") +
      theme_minimal()
  })
}

shinyApp(ui, server)