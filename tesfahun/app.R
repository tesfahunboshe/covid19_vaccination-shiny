# Author: Tesfahun Tegene Boshe
# Date: May 2021

#set working directory
setwd("~\\sem 2\\Advanced programming in R\\myAPP\\covid19_vaccination-shiny")
# install the necessary packages
requiredPackages = c("magrittr","rvest","readxl","dplyr","maps","ggplot2","reshape2","ggiraph","RColorBrewer","shiny","geojsonio",
                     "shinythemes","shinydashboard","shinyWidgets","plotly","leaflet","data.table","scales","lattice","gridExtra")

for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }

# import data
countries <- read.csv("tesfahun\\countries_codes_and_coordinates.csv")
worldcountry = geojsonio::geojson_read("tesfahun\\50m.geojson", what = "sp")
Vaccinated <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"))
locations <- as.data.frame(fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv"))
source("alparslan\\source.R")


Vaccinated <- merge(x = Vaccinated[ , c("location","date","daily_vaccinations","total_vaccinations")], y = locations[ , c("location", "iso_code")], by = "location", all.x=TRUE)
# remove na's
Vaccinated <- Vaccinated[!is.na(Vaccinated$iso_code),]

# mapping color
COLOR = "#cc4c02"

### DATA PROCESSING: COVID-19 ###

# extract time stamp from Vaccinated
update <- tail(Vaccinated$date,1)

# check consistency of country names across datasets
if (all(unique(Vaccinated$location) %in% unique(countries$country))==FALSE)
{ print("Error: inconsistent country names")}

unavailable_countries <- unique(Vaccinated$location)[!sort(unique(Vaccinated$location))  %in% unique(countries$country)]

# extract dates
if (any(grepl("/", Vaccinated$date))) {
    Vaccinated$date <- format(as.Date(Vaccinated$date, format="%d/%m/%Y"),"%Y-%m-%d")
} else { Vaccinated$date = as.Date(Vaccinated$date, format="%Y-%m-%d") }
Vaccinated$date <- as.Date(Vaccinated$date)
earliest_date <- as.Date(min(Vaccinated$date),"%Y-%m-%d")
current_date <- as.Date(max(Vaccinated$date),"%Y-%m-%d")
cv_max_date_clean <- format(as.POSIXct(current_date),"%d %B %Y")

#join two tables to find country/continent info
Vaccinated <- merge(Vaccinated, countries[,-c(1,2,3,5,8,9,10)], by.x = "iso_code",by.y = "alpha3")
Vaccinated$daily_vaccinations[is.na(Vaccinated$daily_vaccinations)] <- 0
Vaccinated <- Vaccinated[order(Vaccinated$date),]

# select countries with iso_code matching to the map
countries_available_to_map <- Vaccinated %>% filter(iso_code %in% worldcountry$ADM0_A3)
if (all(countries_available_to_map$iso_code %in% worldcountry$ADM0_A3)==FALSE)
{ print("Error: inconsistent country names")}



# create plotting parameters for map
bins <- c(0,10,50,100,500,1000,Inf)
cv_pal <- colorBin("Oranges", domain = countries_available_to_map$daily_vaccinations, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% countries_available_to_map$iso_code, ]

# creat cv base map
basemap <- leaflet(plot_map) %>%
    addTiles() %>%
    addLayersControl(
        position = "bottomright") %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-60,~60,70) %>%
    addLegend("bottomleft", pal = cv_pal, values = ~countries_available_to_map$daily_vaccinations,
              title = "<small>Daily Vaccinations</small>")


# assign colours to countries to ensure consistency between plots
colors = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
colors_names = c(as.character(unique(Vaccinated$location)),"Global")
country_cols = colors[1:length(colors_names)]
names(country_cols) = colors_names

# Choices for drop-downs
Continents <- c("Global",unique(countries_available_to_map$continent_level))
names(Continents) <- Continents


# country names
list_countries <- unique(countries_available_to_map$location)
list_countries <- c("All",sort(list_countries))
names(list_countries) <- list_countries

ui <- bootstrapPage(
    tags$head(includeHTML("tesfahun\\gtag.html")),
    navbarPage("Vaccination", id="nav",
               theme = shinythemes::shinytheme("united"), collapsible = TRUE,
               tabPanel("Vaccination By Country",
                        div(class="outer",

                            tags$head(
                                # Include our custom CSS
                                includeCSS("tesfahun\\styles.css"),
                                includeScript("tesfahun\\gomap.js")
                            ),

                            # If not using custom CSS, set height of leafletOutput to a number instead of percent
                            leafletOutput("mymap", width="100%", height="100%"),

                            # Shiny versions prior to 0.11 should use class = "modal" instead.
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          h5(textOutput("date_selected"), align = "right"),
                                          h2("Filters"),

                                          selectInput("Continent", "Continent", Continents, selected = "Global"),
                                          selectInput("Country", "Country", list_countries, selected = "All",multiple = FALSE),
                                          shinyWidgets::sliderTextInput("date_input",
                                                                        label = h4("Date range"),
                                                                        choices = format(unique(Vaccinated$date), "%d %b %y"),
                                                                        selected = format(current_date, "%d %b %y"),
                                                                        grid = FALSE,
                                                                        animate=animationOptions(interval = 3000, loop = FALSE)),


                                          plotOutput("plotTotal", height = 200),
                                          plotOutput("histogramTotal", height = 250)
                            )
                        )),

               tabPanel("Forecast",

                        titlePanel("Settings"),
                        sidebarLayout(position = "left",

                                      sidebarPanel(selectInput("Country1", "Filter by country", list_countries, "Turkey", multiple=FALSE),

                                                   "Select plots",
                                                   checkboxInput("donum1", "Time series", value = T),
                                                   checkboxInput("donum2", "Residuals", value = F),
                                                   sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                                                   sliderInput("wt2","Weight 2",min=1,max=10,value=1)
                                      ),
                                      mainPanel("Plots",
                                                column(6,plotOutput(outputId="plotgraph", width="900px",height="500px"))
                                      ))






               ),

               tabPanel("About Us",
                        titlePanel("About Us"),
                        mainPanel(
                            includeCSS("tesfahun\\style2.css"),
                            includeHTML("tesfahun\\aboutus.html")
                        )

               )
    )
)



server <- function(input, output, session) {
    ## vaccination by country tab
    formatted_date = reactive({
        format(as.Date(input$date_input, format="%d %b %y"), "%Y-%m-%d")
    })
    continent = reactive({input$Continent})
    country = reactive({input$Country})

    output$date_selected <- renderText({
        format(as.POSIXct(formatted_date()),"%d %B %Y")
    })

    # filter for the map
    reactive_db = reactive({

        if(is.null(input$Country) | is.null(input$Continent)){
            Vaccinated %>% filter(date == formatted_date())
        }
        else {
            if("Global" == input$Continent){
                if("All" == input$Country){
                    Vaccinated %>% filter(date == formatted_date())
                }
                else {
                    Vaccinated %>% filter(date == formatted_date()) %>% filter(location %in% input$Country)
                }
            }
            else {
                if("All" == input$Country){
                    Vaccinated %>% filter(date == formatted_date())%>% filter(continent_level %in% input$Continent)
                }
                else {
                    Vaccinated %>% filter(date == formatted_date())%>% filter(continent_level %in% input$Continent)%>% filter(location %in% input$Country)
                }
            }
        }
    })


    output$mymap <- renderLeaflet({
        basemap
    })


    #events to listen to
    toListen <- reactive({
        list(input$Continent,input$Country,input$date_input)
    })

    observeEvent(toListen(), {
        leafletProxy("mymap") %>%
            clearMarkers() %>%
            clearShapes()  %>%
            addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(daily_vaccinations)^(1/5.5),

                             fillOpacity = 0.1, color = COLOR, group = "AstraZenecca",

                             label = sprintf("%s<br/>Vaccinated/1000: %g",reactive_db()$location,round(reactive_db()$daily_vaccinations/1000,0))%>%
                                 lapply(htmltools::HTML),

                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = COLOR),
                                 textsize = "15px", direction = "auto")
            )
    })


    output$plotTotal <- renderPlot({
        ggplot(aggregate(daily_vaccinations~date,countries_available_to_map,sum), aes(x=date, y=daily_vaccinations)) +
            geom_line()+
            geom_point()+
            labs(title="Daily cummulative Vaccination", x="date",y="vaccination")
    })

    output$histogramTotal <- renderPlot({
        ggplot(aggregate(daily_vaccinations~date,countries_available_to_map,sum), aes(x=date, y=daily_vaccinations)) +
            geom_line()+
            geom_point()+scale_y_log10()+labs(title="Log cumsum distribution across days", x="date",y="vaccination")
    })

    ## Forecasting part

    data_tur <- reactive({
        Data$new(Vaccinated,country = input$Country1)

    })


    model <- reactive({
        Model$new(ts_object = data_tur()$ts_data)
    })


    pt1 <- reactive({
        if (!input$donum1) return(NULL)
        model()$plot_forecast(model_name = 'ARIMA')
    })

    pt2 <- reactive({
        if (!input$donum2) return(NULL)
        model()$plot_residual_fit()
    })



    output$plotgraph = renderPlot({
        ptlist <- list(pt1(),pt2())
        wtlist <- c(input$wt1,input$wt2)
        # remove the null plots from ptlist and wtlist
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete]
        wtlist <- wtlist[to_delete]
        if (length(ptlist)==0) return(NULL)

        grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
    })




}

shinyApp(ui, server)
