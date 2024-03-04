#######
#import library
#install.packages("shiny")
#install.packages("leaflet")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
#install.packages("httr")
library(httr)
library(urltools)
library(lubridate)
#install.packages("plotly")
library(plotly)
library(dplyr)

########

Sys.setlocale(locale = "C")

header <- dashboardHeader(title = "My Dashboard")


sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'Menu',
    menuItem(
      tags$div(
        style = "display: flex; align-items: center;",
        tags$img(src = "https://imageshack.com/i/pnlQBgVVj", style = "width: 30px; height: 30px; border-radius: 50%; margin-right: 10px;"),
        "Đỗ Huy Hoàng(HE161734)"
      ),
      tabName = "tab1"
    ),
    menuItem('Enter city name', tabName = 'tab2', icon = icon('magnifying-glass'),
             textInput("cityNameInput", "Enter City Name"),  
             actionButton("getWeatherButton", "Get Weather")),
    menuItem('Weather',tabName = 'tab3', icon = icon('temperature-half')),
    menuItem('Forecast',tabName = 'tab4', icon = icon('table-cells'))    
  )
)


css <- "
  .custom-box .box-title {
    font-weight: normal;
    text-align: center;
    display: flex;
    justify-content: center;
    align-items: center;
  }
  .weather-info-box {
    display: flex;
    justify-content: center;
  }
  
"


body <- dashboardBody(
  tags$head(tags$style(HTML(css))),
  tabItems(
    tabItem(tabName = 'tab3',
            h1('Current Weather'),
            fluidRow(
              box(
                leafletOutput("weatherMap", width = "100%", height = "533px")
              ),
              box(style = "text-align: center; margin-bottom: 10px;",
                  HTML("<b><span style='font-size: 20px;'>"),
                  textOutput("locationAndDate"),
                  HTML("</span></b>")
              ),
              box(
                div(style = "text-align: center; margin-bottom: 10px;",
                    div("Weather Information", style = "font-weight: bold; font-size: 30px; padding: 5px; border: 1px solid #ccc; border-radius: 5px;"),
                ),
                class = "custom-box",
                uiOutput("weatherInfo")
                
              )
            )
    ),
    tabItem(tabName = 'tab4',
            h1(textOutput("forecastTitle")),
            textOutput("clickedLocation"),  
            
            fluidRow(
              selectizeInput("weatherParameterSelect", "Select Parameter",
                             choices = c("Temp" = "temp", "Feels_Like" = "feels_like", "Temp_Max" = "temp_max", "Temp_Min" = "temp_min", 
                                         "Pressure" = "pressure", "Sea_Level" = "sea_level", "Grnd_Level" = "grnd_level", "Humidity" = "humidity", 
                                         "Speed" = "wind_speed", "Deg" = "deg", "Gust" = "gust"),
                             selected = "temp"  
              ),
              plotlyOutput('forecastPlot', width = "100%", height = "500px")
            )
    )
  )
)


#ui
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)


#sever
server <- function(input, output,session) {
  
################################################################################
  ### WEATHER ############
  api_key <- "ac7b775816ab6ce08e6b1c4e7199e07e"
  locationName <- reactiveVal()
  clickedLocation <- reactiveVal(NULL)

  #########################
  observe({
    updateTabItems(session, "Menu", "tab3")
  })
  #########################
  forecast_data_for_city <- reactiveVal(NULL)
  selected_city_name <- reactiveVal(NULL)
  updateForecastForCity <- function(cityName) {
    city <- url_encode(cityName)
    API_call <- sprintf("https://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s", city, api_key)
    response <- GET(API_call)
    
    if (http_type(response) == "application/json") {
      data <- content(response, "parsed")
      selected_city_name(data$name)
      
      lat <- data$coord$lat
      lon <- data$coord$lon
      getWeatherForecast(lat, lon)
    }
  }
  #########################
  output$locationAndDate <- renderText({
    if (!is.null(locationName())) {
      date_time_str <- format(Sys.time(), format = "%a %d %b %Y")
      paste(locationName(),"-" ,date_time_str)
    }
  })
  #####################################
  #hàm search
  getWeatherForCity <- function(cityName) {
    updateForecastForCity(cityName)
    
    city <- url_encode(cityName)  
    API_call <- sprintf("https://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s", city, api_key)
    response <- GET(API_call)
    
    if (http_type(response) == "application/json") {
      data <- content(response, "parsed")
      locationName(data$name)
      
      
      current_temperature <- data$main$temp - 273.15  # Chuyển độ K thành độ C
      feels_like <- data$main$feels_like - 273.15
      humidity <- data$main$humidity
      temp_min <- data$main$temp_min
      temp_max <- data$main$temp_max
      pressure <- data$main$pressure
      sea_level <- data$main$sea_level
      grnd_level <- data$main$grnd_level
      weather_condition <- data$weather[[1]]$description
      visibility <- data$visibility / 1000
      wind_speed <- data$wind$speed 
      air_pressure <- data$main$pressure
      deg <- data$wind$deg
      gust = ifelse(!is.null(data$wind$gust), data$wind$gust, 0)
      
      
      
      
      output$weatherInfo <- renderUI({
        result <- tagList(
          box(
            div(paste(current_temperature, "°C"),
                class = "weather-info-box"),
            title = "Current temperature",
            status = "primary",
            solidHeader = TRUE,
          ),
          box(
            div(paste(feels_like, "°C"), class = "weather-info-box"),
            title = "Feels Like",
            status = "warning",
            solidHeader = TRUE,
            
          ),
          box(
            div(paste(humidity, "%"), class = "weather-info-box"),
            title = "Humidity",
            status = "success",
            solidHeader = TRUE,
            
          ),
          box(
            div(paste(weather_condition), class = "weather-info-box"),
            title = "Weather Condition",
            status = "info",
            solidHeader = TRUE,
            
          ),
          box(
            div(paste(visibility, "km"), class = "weather-info-box"),
            title = "Visibility",
            status = "danger",
            solidHeader = TRUE,
            
          ),
          box(
            div(paste(wind_speed, "km/h"), class = "weather-info-box"),
            title = "Wind Speed",
            status = "primary",
            solidHeader = TRUE,
            
          ),
          box(
            div(paste(air_pressure, "hPa"), class = "weather-info-box"),
            title = "Air Pressure",
            status = "warning",
            solidHeader = TRUE,
            
          )
        )

        return(result)
      })
      
      
    } else {
      output$weatherInfo <- renderText("Không thể lấy thông tin thời tiết.")
    }
    
  }
  observeEvent(input$getWeatherButton, {
    city_name <- input$cityNameInput
    if (city_name != "") {
      getWeatherForCity(city_name)
    }
  })
  ################################################################
  
  
  #################################################################
  # hàm lấy tt Hà Nội
  getWeatherForHanoi <- function() {
    lat <- 21.0277644
    lon <- 105.8341598
    API_call <- sprintf("https://api.openweathermap.org/data/2.5/forecast?lat=%s&lon=%s&appid=%s", lat, lon, api_key)
    response <- GET(API_call)
    
    if (http_type(response) == "application/json") {
      data <- content(response, "parsed")
      
      # Extract city name
      locationName(data$city$name)
      
      # Access other weather data using correct paths in the JSON
      current_temperature <- data$list[[1]]$main$temp - 273.15
      feels_like <- data$list[[1]]$main$feels_like - 273.15
      humidity <- data$list[[1]]$main$humidity
      temp_min <- data$list[[1]]$main$temp_min
      temp_max <- data$list[[1]]$main$temp_max
      pressure <- data$list[[1]]$main$pressure
      sea_level <- data$list[[1]]$main$sea_level
      grnd_level <- data$list[[1]]$main$grnd_level
      weather_condition <- data$list[[1]]$weather[[1]]$description
      visibility <- data$list[[1]]$visibility / 1000
      wind_speed <- data$wind$speed
      air_pressure <- data$list[[1]]$main$pressure
      deg <- data$list[[1]]$wind$deg
      gust <- data$wind$gust
      
      # Render the UI with updated data
      output$weatherInfo <- renderUI({
        result <- tagList(
          box(
            div(paste(current_temperature, "°C"),
                class = "weather-info-box"),
            title = "Current temperature",
            status = "primary",
            solidHeader = TRUE,
          ),
          box(
            div(paste(feels_like, "°C"), class = "weather-info-box"),
            title = "Feels Like",
            status = "warning",
            solidHeader = TRUE,
          ),
          box(
            div(paste(humidity, "%"), class = "weather-info-box"),
            title = "Humidity",
            status = "success",
            solidHeader = TRUE,
          ),
          box(
            div(paste(weather_condition), class = "weather-info-box"),
            title = "Weather Condition",
            status = "info",
            solidHeader = TRUE,
          ),
          box(
            div(paste(visibility, "km"), class = "weather-info-box"),
            title = "Visibility",
            status = "danger",
            solidHeader = TRUE,
          ),
          box(
            div(paste(wind_speed, "km/h"), class = "weather-info-box"),
            title = "Wind Speed",
            status = "primary",
            solidHeader = TRUE,
          ),
          box(
            div(paste(air_pressure, "hPa"), class = "weather-info-box"),
            title = "Air Pressure",
            status = "warning",
            solidHeader = TRUE,
          )
        )
        return(result)
      })
      
    } else {
      output$weatherInfo <- renderText("Không thể lấy thông tin thời tiết.")
    }
  }
  
  
  getWeatherForHanoi()
  #######################################################
  #######################################################
  output$weatherMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 105.8342, lat = 21.0285, zoom = 12)  
  })
  forecast_data <- reactiveVal(NULL)
  
  observeEvent(input$weatherMap_click, {
    click <- input$weatherMap_click
    if (!is.null(click)) {
      lat <- click$lat
      lng <- click$lng
      response <- GET(paste0("https://api.openweathermap.org/data/2.5/weather?lat=", lat, "&lon=", lng, "&appid=", api_key))
      
      if (http_type(response) == "application/json") {
        data <- content(response, "parsed")
        locationName(data$name)
        
        
        current_temperature <- data$main$temp - 273.15  # Chuyển độ K thành độ C
        feels_like <- data$main$feels_like - 273.15
        humidity <- data$main$humidity
        temp_min <- data$main$temp_min
        temp_max <- data$main$temp_max
        pressure <- data$main$pressure
        sea_level <- data$main$sea_level
        grnd_level <- data$main$grnd_level
        weather_condition <- data$weather[[1]]$description
        visibility <- data$visibility / 1000
        wind_speed <- data$wind$speed 
        air_pressure <- data$main$pressure
        deg <- data$wind$deg
        gust = ifelse(!is.null(data$wind$gust), data$wind$gust, 0)
        
        
        
        
        output$weatherInfo <- renderUI({
          result <- tagList(
            box(
              div(paste(current_temperature, "°C"),
                  class = "weather-info-box"),
              title = "Current temperature",
              status = "primary",
              solidHeader = TRUE,
            ),
            box(
              div(paste(feels_like, "°C"), class = "weather-info-box"),
              title = "Feels Like",
              status = "warning",
              solidHeader = TRUE,
              
            ),
            box(
              div(paste(humidity, "%"), class = "weather-info-box"),
              title = "Humidity",
              status = "success",
              solidHeader = TRUE,
              
            ),
            box(
              div(paste(weather_condition), class = "weather-info-box"),
              title = "Weather Condition",
              status = "info",
              solidHeader = TRUE,
              
            ),
            box(
              div(paste(visibility, "km"), class = "weather-info-box"),
              title = "Visibility",
              status = "danger",
              solidHeader = TRUE,
              
            ),
            box(
              div(paste(wind_speed, "km/h"), class = "weather-info-box"),
              title = "Wind Speed",
              status = "primary",
              solidHeader = TRUE,
              
            ),
            box(
              div(paste(air_pressure, "hPa"), class = "weather-info-box"),
              title = "Air Pressure",
              status = "warning",
              solidHeader = TRUE,
              
            )
          )
          return(result)
        })
        
        
      } else {
        output$weatherInfo <- renderText("Không thể lấy thông tin thời tiết.")
      }
    }
  })
  # Cập nhật tên vị trí
  output$locationName <- renderText({
    locationName()
  })
################################################################################
  ### FORECAST
  getWeatherForecast <- function(lat, lon) {
    API_call <- sprintf("https://api.openweathermap.org/data/2.5/forecast?lat=%s&lon=%s&appid=%s", lat, lon, api_key)
    response <- GET(API_call)
    
    if (http_type(response) == "application/json") {
      data <- content(response, "parsed")
      
      forecast_data(data$list)
      clickedLocation(data$city$name) 

    }
  }
  
  output$forecastTitle <- renderText({
    if (!is.null(clickedLocation())) {
      clickedLocation()
    } else {
      "5-Day Weather Forecast"
    }
  })
  
  
  output$forecastPlot <- renderPlotly({
    if (!is.null(forecast_data())) {
      selected_parameter <- input$weatherParameterSelect
      df <- lapply(forecast_data(), function(entry) {
        datetime <- as.POSIXct(entry$dt_txt)
        if (selected_parameter %in% names(entry$main)) {
          parameter_value <- entry$main[[selected_parameter]]
          data.frame(datetime = datetime, value = parameter_value)
        } else {
          data.frame(datetime = datetime, value = NA)
        }}) %>%
        bind_rows()
      
      # Vẽ biểu đồ bằng plotly
      plot <- ggplot(df, aes(x = datetime, y = value)) +
        geom_line(size = 1 , color = "red" ) +
        geom_point(color = "black", size = 3) +  
        labs(x = "Date and Time", y = selected_parameter) +
        theme_minimal()
      plot <- plot + xlab("Time")
      
        
      ggplotly(plot)
    }
  })
  
  observeEvent(input$weatherMap_click, {
    click <- input$weatherMap_click
    if (!is.null(click)) {
      lat <- click$lat
      lon <- click$lng
      getWeatherForecast(lat, lon)
    }
  })
  
  getForecastForHanoi <- function() {
    lat <- 21.0277644
    lon <- 105.8341598
    getWeatherForecast(lat, lon)
  }
  getForecastForHanoi()
  
}



#run app
shinyApp(ui = ui, server = server)

