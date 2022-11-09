#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(xgboost)
library(flexdashboard)
library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(lubridate)
library(shinyjs)
library(shinyWidgets)
library(highcharter)
library(fs)
library(dplyr)


# Core
library(tidyverse)
library(tidyquant)
library(scales)
library(plotly)

library(shinyWidgets)
library(shiny)
library(viridisLite)
library(treemap)


library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)



# Map
library(sf)
###library(mapview)
library(leaflet)

# Modeling
library(timetk)
library(parsnip)
library(collapsibleTree)

library(shinyauthr) #devtools::install_github("business-science/shinyauthr)



# bildirim_loc_tbl_selected <- readRDS(url("https://github.com/datalabb/sra/blob/main/00_Data/bildirim_loc_tbl_selected.rds"))

# bildirim_loc_tbl_selected <- readRDS("../sra/00_Data/bildirim_loc_tbl_selected.rds")


# APP ----

ui <- fluidPage(
  
  # JS ----
  shinyjs::useShinyjs(), # Include shinyjs
  
  shinythemes::themeSelector(),
  
  
  
  #theme = shinytheme("cyborg"),
  
  navbarPage(
    
    title = "Risk Forecast App",
    
    # setBackgroundColor("ghostwhite"),
    
    
    # setBackgroundColor(
    #   color = "#d7d8d6",
    #   gradient = c("linear", "radial"),
    #   direction = c("bottom", "top", "right", "left"),
    #   shinydashboard = FALSE
    # ),
    
    tabPanel("GENERAL", icon = icon("list-ul"), value = "summary",
             
             sidebarPanel(
               
               width = 3,
               
               
               div(
                 
                 pickerInput(
                   inputId = "risk",
                   label = "Risk", 
                   choices = unique(bildirim_loc_tbl_selected$RiskGroupName),
                   selected = "Hırsızlık",
                   multiple = TRUE
                 ),
                 
                 radioGroupButtons(
                   inputId = "period",
                   label = "Period",
                   choiceNames = c("Day", "Week", "Month", "Quarter", "Year"),
                   choiceValues =  c("day", "week", "month", "quarter", "year"), 
                   selected = "month",
                   individual = T,
                   width = '400px',
                   justified = T, direction = "vertical",
                   size = "sm",
                   checkIcon = list(
                     yes = tags$i(class = "fa fa-circle",
                                  style = "color: red"),
                     no = tags$i(class = "fa fa-circle-o",
                                 style = "color: steelblue"))
                 )
                 
               ),
               
               
               div(
                 id = "forecast_mode", 
                 
                 h4("Forecast Mode"),
                 
                 shinyWidgets::switchInput(
                   inputId = "forecast_mode",
                   value = FALSE,
                   handleWidth = 50,
                   labelWidth = 50,
                   inline = TRUE,
                   width = "50px",
                   onStatus = "success", 
                   offStatus = "danger"
                 ),
                 
                 conditionalPanel(
                   condition = "input.forecast_mode == 1",
                   numericInput(inputId = "length_out",
                                label = "Forecast Horizon",
                                value = 12,  
                                min = 1 )
                 )
                 
               ),
               
               div(
                 id =" apply_reset",
                 
                 # Apply & Reset Button
                 
                 actionButton(inputId = "apply",
                              label = "Apply",
                              icon = icon("play")),
                 
                 
                 actionButton(inputId = "reset" ,
                              label = "Reset", 
                              icon = icon("sync"))
                 
               ),
               
               br(),
               br(),
               
               #plotOutput("datalab")
               
               
             ),
             
             mainPanel(
               
               tabsetPanel(id="ui_tab",
                           
                           tabPanel("Viz",
                                    column(
                                      width = 8,
                                      plotlyOutput("plot")
                                      
                                    ),
                                    
                                    column(
                                      width = 4,
                                      plotlyOutput("avrgrisk")
                                    ),
                                    
                                    column(
                                      width = 8,
                                      leafletOutput(outputId = "mapp")
                                    )
                                    
                           ),
                           
                           tabPanel("Table",
                                    column(12, h4("Click a site"), div(DT::dataTableOutput("proje_table"), style = "font-size:70%")),
                                    
                                    column(
                                      width = 12,
                                      plotlyOutput("plot2")
                                      
                                    ),
                                    
                           )
               )
               
             ) # end of main panel
    )
  )
  
)


# Define server logic required to draw a histogram


server <- function(input, output, session) {
  
  
  # Loading modal to keep user out of trouble while map draws...
  showModal(modalDialog(title="MAP LOADING - PLEASE WAIT...","Please wait for map to draw before proceeding.",size="l",footer=NULL))
  
  # Remove modal when app is ready
  observe({
    req(map_tbl, mlid_param_asmnts)
    removeModal()
  })
  
  # Extract mlid/param level assessments
  mlid_param_asmnts=map_tbl$Birim
  
  # Datalab logo
  
  # moxbuller = function(n) {   
  #   u = runif(n)   
  #   v = runif(n)   
  #   x = cos(2*pi*u)*sqrt(-2*log(v))  
  #   y = sin(2*pi*v)*sqrt(-2*log(u))
  #   r = list(x=x, y=y)
  #   return(r) 
  # }
  # 
  # 
  # output$datalab <- renderPlot({
  #   r = moxbuller(50000) 
  #   par(bg="black") 
  #   par(mar=c(0,0,0,0)) 
  #   a <- plot(r$x,r$y, pch=".", col="#8d5fff", cex=1.2)
  #   a
  #   
  # })
  
  
  forecastmodebutton <- eventReactive(eventExpr = input$apply, {
    
    input$forecast_mode
  })
  
  forecasthorizon <- eventReactive(eventExpr = input$apply, {
    
    input$length_out
    
  })
  
  # data_table
  output$proje_table=DT::renderDataTable({
    
    DT::datatable(proje_risk_date, selection='multiple',
                  options = list(scrollY = '500px', paging = FALSE, scrollX = TRUE, searching=T)
    ) %>%
      #DT::formatStyle("Date", "Proje", backgroundColor = DT::styleColorBar(proje_risk_date$ToplamBildirim >= 10, "red"))  %>%
      DT::formatStyle("ToplamBildirim", backgroundColor = DT::styleEqual(5, "orange"))
  })
  
  # # reactive data table
  # 
  # 
  # # Table row click (to identify selected site & parameter)
  # observe({
  #   req(input$proje_table_rows_selected)
  #   row_click=input$proje_table_rows_selected
  #   # siteid=mlid_param_asmnts[row_click,"IR_MLID"]
  #   # reactive_objects$sel_param=mlid_param_asmnts[row_click,"R3172ParameterName"]
  #   # reactive_objects$sel_mlid=siteid
  # })
  # 
  # 
  # # Data table output
  # observe({
  #   req(proje_risk_date$Date)
  #   # table_data=profiles_wide[profiles_wide$ActivityIdentifier==reactive_objects$selectedActID,c("IR_MLID","ActivityStartDate","Depth_m","DO_mgL","pH","Temp_degC","do_exc","pH_exc","temp_exc")]
  #   # reactive_objects$table_data=table_data[order(table_data$Depth_m),]
  #   })
  # 
  # 
  # output$proje_table=DT::renderDataTable({
  #   req(proje_risk_date$Proje)
  #   DT::datatable(proje_risk_date$Proje, selection='multiple',
  #                 options = list(scrollY = '500px', paging = FALSE, scrollX = TRUE, searching=T)
  #                 ) %>%
  #     DT::formatStyle("ToplamBildirim", backgroundColor = DT::styleEqual(5, "orange"))
  #   })
  # 
  
  output$plot <- renderPlotly({
    
    if(forecastmodebutton() == FALSE) {
      
      p <- bildirim_loc_tbl_selected %>% 
        aggregate_risk_tbl(risk = input$risk, time_unit = input$period) %>% 
        plot_time_series_risk()
      
    } else {
      
      p <- bildirim_loc_tbl_selected %>% 
        aggregate_risk_tbl(risk = input$risk, time_unit = input$period) %>% 
        generate_forecast(length_out = forecasthorizon(), seed = 123) %>% 
        plot_forecast()
    }
    p
  })
  
  output$plot2 <- renderPlotly({
    
    if(forecastmodebutton() == FALSE) {
      
      p <- bildirim_loc_tbl_selected %>% 
        aggregate_risk_tbl(risk = input$risk, time_unit = input$period) %>% 
        plot_time_series_risk()
      
    } else {
      p <- bildirim_loc_tbl_selected %>% 
        aggregate_risk_tbl(risk = input$risk, time_unit = input$period) %>% 
        generate_forecast(length_out = forecasthorizon(), seed = 123) %>% 
        plot_forecast()
    }
    
    p
    
  })
  
  
  # Average Risk Gauge
  
  risk_gauge  <- eventReactive(
    eventExpr = input$risk, 
    valueExpr = {
      
      risk_rate_tbl_last3months <-bildirim_loc_tbl_selected %>% 
        aggregate_risk_tbl(risk = input$risk, time_unit = "month") %>% 
        filter(difftime(today(), Date, units = "days") <= 120) %>% 
        select(-label_text)
      
      
      avrg_risk_rate <- round(mean(risk_rate_tbl_last3months$newrate),digits = 1)
      
      avrg_risk_rate
      
      
      fig <- plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = avrg_risk_rate,
        title = list(text = "Risk Katsayısı"),
        type = "indicator",
        mode = "gauge+number+delta",
        #delta = list(reference = 7),
        gauge = list(
          axis =list(range = list(NULL, 10)),
          bar = list(color = "darkred"), 
          steps = list(
            list(range = c(0, 5), color = "white"),
            list(range = c(5, 7), color = "yellow"),
            list(range = c(7, 8), color = "orange"),
            list(range = c(8, 10), color = "black")),
          threshold = list(
            line = list(color = "red", width = 4),
            thickness = 0.75,
            value = 8))) 
      fig <- fig %>%
        layout(margin = list(l=20,r=30))
      
      }, 
    ignoreNULL = FALSE 
  )
  
  
  
  
  output$avrgrisk <- renderPlotly({
    risk_gauge()
  })
  
  
  
  observe({input$reset
    
    updatePickerInput(session = session,  
                      inputId = "risk",
                      selected = "Hırsızlık")
    
    updateSwitchInput(
      session = session, 
      inputId = "forecast_mode",
      value = FALSE
    )
    
    updateNumericInput(
      session = session, 
      inputId = "length_out",
      value = 12
    )
    
    updateRadioGroupButtons(
      session = session, 
      inputId = "period",
      selected = "month"
    )
    
    shinyjs::delay(ms = 300, expr = {
      shinyjs::click(id = "apply") })
    
  })
  
  # Map ----
  
  
  mapp_tbl  <- eventReactive(
    eventExpr = input$apply, 
    valueExpr = {
      
      map_tbl %>% 
        
        filter(RiskGroupName %in% input$risk)
      
    }, 
    ignoreNULL = FALSE 
  )
  
  output$mapp <- renderLeaflet({
    
    map_tbl2 <- mapp_tbl()
    
    
    birim_mapp <- leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = map_tbl2, lng = map_tbl2$Boylam, lat = map_tbl2$Enlem, radius = 2)
    
    birim_mapp
  })
  
  leafletOutput(outputId = "mapp")
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
