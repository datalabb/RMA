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
library(shinydashboard)
library(shinythemes)
library(DT)
library(readxl)
library(lubridate)
library(shinyjs)
library(shinyWidgets)
library(highcharter)
library(fs)
library(dplyr)
library(scales)


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
library(leaflet.extras)

# Modeling
library(timetk)
library(parsnip)
library(collapsibleTree)

library(shinyauthr) #devtools::install_github("business-science/shinyauthr)



# bildirim_loc_tbl_selected <- readRDS(url("https://github.com/datalabb/sra/blob/main/00_Data/bildirim_loc_tbl_selected.rds"))

# bildirim_loc_tbl_selected <- readRDS("../sra/00_Data/bildirim_loc_tbl_selected.rds")

# Functions ---- 


# Info Card Function ----

info_card <- function(title, subtitle, value, sub_value,
                      main_icon = "chart-line", sub_icon = "arrow-up",
                      bg_color = "default", text_color = "default", sub_text_color = "success") {
  
  div(
    class = "panel panel-default",
    div(
      class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
      p(class = "pull-right", icon(class = "fa-3x", main_icon)),
      h3(title),
      h6(subtitle),
      h1(value),
      p(
        class = str_glue("text-{sub_text_color}"),
        icon(sub_icon),
        tags$small(sub_value)
      )
    )
  )
  
}

if (interactive()) {

# APP ----

ui <- fluidPage(
  
  # JS ----
  shinyjs::useShinyjs(), # Include shinyjs
  
  shinythemes::themeSelector(),
  
  
  
  theme = shinytheme("cyborg"),
  
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
                 
                 prettyRadioButtons(
                   inputId = "risk",
                   label = h3("Risk"), 
                   choices = unique(bildirim_loc_tbl_selected$RiskGroupName),
                   selected = "Hırsızlık", status = "danger",
                   #shape = c("round", "square", "curve"),
                   shape = "curve"

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
               p(tags$b("Risk Diameter for Mapping")),
               sliderInput("diameter", label = NULL, min = 0, 
                           max = 100, value = 50),
               
               
               column(
                 width = 4,
                 dataTableOutput('table')
               )
               
               
               
               #plotOutput("datalab")
               
               
             ),
             
             mainPanel(
               
               tabsetPanel(id="ui_tab",
                           
                           tabPanel("Viz",
                                    
                                    column(
                                      width = 12,
                                      infoBoxOutput("ibox")
                                    ),
                                    
                                    column(
                                      width = 9,
                                      
                                      wellPanel(

                                          plotlyOutput("plot"),
                                          
                                          div(
                                            id = "input_buttons",
                                            actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
                                            actionButton(inputId = "map_add", label = NULL, icon = icon("map")),
                                            br(),
                                            br(),
                                            
                                            dropdown(
                                              
                                              div(
                                                column(
                                                  width = 8,
                                                  uiOutput(outputId = "single_card"),
                                                  
                                                  actionBttn(inputId = "info_button1", 
                                                             #label = "Veri Girişi",
                                                             style = "minimal", #“simple”, “bordered”, “minimal”, “stretch”, “jelly”, “gradient”, “fill”, “material-circle”, “material-flat”, “pill”, “float”, “unite”  
                                                             size = "sm",
                                                             color = "royal",
                                                             icon = icon("fa-light fa-circle-info")),
                                                  
                                                  column(
                                                    width = 12,
                                                    id="info1",
                                                    helpText("- Negative change in risk rate means that the forecasted risk rate is lower than the average risk rate of past 3 months."),
                                                    helpText("- Positive change means that the forecasted risk rate is higher than the average risk rate of past 3 months.")
                                                    
                                                  ) %>% hidden()

                                                ),
                                                
                                                column(
                                                  width = 4,
                                                  div(
                                                    class = "panel",
                                                    div(class = "panel-header", h4(HTML("<b>Analyst Commentary</b>"))),
                                                    div(
                                                      class = "panel-body",
                                                      textOutput(outputId = "analyst_commentary2"),
                                                      p(HTML("This analysis was made with the algorithm based on the data set consisting of the results recorded within a certain period. 
                                                             The estimation score and suggestions resulting from the analysis should be considered as a <b>'Decision Support Tool'</b>."))
                                                    )
                                                  )
                                                )
                                                
                                              ),

                                              circle = TRUE,
                                              status = "danger",
                                              icon = icon("cog"), size = "s",
                                              width = "1000px", color = "royal", 
                                              label = "Dynamic Forecast", right = F, 
                                              animate = T,
                                              style = "gradient" 
                                              
                                            )

                                          )
                                        
                                      ),
                                      
                                      div(
                                        id= "map_section",
                                        wellPanel(
                                          
                                         column(
                                            width = 6,
                                            tags$h4("Average Risk Rate of past 3 months and Prediction for the next 3 months", style="color:white"),
                                            tags$h6("Risk scale: 0-10", style="color:white"),
                                            dataTableOutput("riskratetbl")
                                            
                                          )
                                          
                                        )
                                      ) %>% hidden()

                                    ),
                                    
                                    column(
                                      width = 3,
                                      wellPanel(
                                        plotlyOutput("avrgrisk")
                                      )
                                      
                                    ),
                                    
                                    wellPanel(
                                      column(
                                        width = 9,
                                        leafletOutput(outputId = "mapp", width="100%", height="500px")
                                      ),
                                      
  
                                      column(
                                        width = 3,
                                        dataTableOutput('coord_table')
                                      )
                                      
                                    )
                                    



                           ),
                           
                           tabPanel("Table",
                                    column(12, h4("Click a site"), div(DT::dataTableOutput("proje_table"), style = "font-size:70%")),
                                    
                                    column(
                                      width = 12,
                                      plotlyOutput("plot2")
                                      
                                    )
                                    
                           )
               )
               
             ) # end of main panel
    )
  )
  
)


# Define server logic required to draw a histogram


server <- function(input, output, session) {
  
  
  # Data entry
  observeEvent(input$info_button1, {
    toggle(id = "info1", anim = TRUE)
  })
  
  # Toggle Input Settings ----
  observeEvent(input$card_add, {
    toggle(id = "card_section", anim = TRUE)
  })
  
  # Toggle Input Settings ----
  observeEvent(input$map_add, {
    toggle(id = "map_section", anim = TRUE)
  })
  
  
  
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
        title = list(text = "Risk Rate"),
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
            list(range = c(8, 10), color = "black"))))
          # threshold = list(
          #   line = list(color = "red", width = 4),
          #   thickness = 0.75,
          #   value = 8))) 
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
  
  # Coord. valuebox
  
  ## Longtitude
  
  coordinate_lng  <- eventReactive(
    eventExpr = input$mapp_click$lng, 
    valueExpr = {
      
      round(input$mapp_click$lng,2)
      
    }, 
    ignoreNULL = FALSE 
  )
  
  
  
  ## Latitude
  
  coordinate_lat  <- eventReactive(
    eventExpr = input$mapp_click$lat, 
    valueExpr = {
      
      round(input$mapp_click$lat,2)
      
    }, 
    ignoreNULL = FALSE 
  )
  
  # Coord Tbl
  
  output$coord_table <- DT::renderDataTable({
    
    coord_tbl <- data.frame(lng = as.numeric(coordinate_lng()),
                            lat = as.numeric(coordinate_lat()))
    
    DT::datatable(coord_tbl, options = list(paging = F, searching = F, dom = 't' ))
  })
  

  
  # Add Click markers
  
  observeEvent(input$mapp_click, {
    
    click <- input$mapp_click
    text<-paste("Latitude ", round(click$lat,2), "Longtitude ", round(click$lng,2))
    
    proxy <- leafletProxy("mapp")
    
    ## This displays the pin drop circle
    proxy %>% 
      clearGroup("new_point") %>%
      #clearMarkers() %>%
      #addPopups(input$mapp_click$lng,input$mapp_click$lat) %>%
      addCircles(click$lng, click$lat, radius=input$diameter*1000, color="black", group = "new_point") %>%
      addMarkers(lng=click$lng, lat = click$lat, popup = paste0(round(click$lng,2)," ", round(click$lat,2)))

  })
  
  # Map 
  
  output$mapp <- renderLeaflet({
    
    if(is.null(input$mapp_click$lng)) {
      
      risk_map_func(input$risk, input$diameter, 36.5, 41.5)
      
    } else {
      
      risk_map_func(input$risk, input$diameter, as.numeric(input$mapp_click$lng), as.numeric(input$mapp_click$lat))
      
    }
    
    
  })
  
  leafletOutput(outputId = "mapp")
  


  # Risk Rate Table
  
  
  # detail_table1_gh <- eventReactive(
  #   eventExpr = input$branch_selection1,
  #   valueExpr = {
  # 
  #     sube_gh_kapasite_tbl_app <- sube_gh_kapasite_tbl %>%
  #       select(OrgBolgeId, OrgSubeId, Ay, idari_per, saha_per, Toplam_birim_suresi, Toplam_yol_suresi, dag_kat, ort_il_trafik_kat, Toplam_saha_sure, GH_Kapasite) %>%
  #       mutate(Toplam_birim_suresi = round(Toplam_birim_suresi, digits = 1),
  #              Toplam_yol_suresi   = round(Toplam_yol_suresi,digits = 1),
  #              dag_kat             = round(dag_kat, digits = 2),
  #              ort_il_trafik_kat   = round(ort_il_trafik_kat,digits = 2),
  #              Toplam_saha_sure    = round(Toplam_saha_sure, digits = 1)) %>%
  #       filter(OrgSubeId %in% input$branch_selection1)
  # 
  # 
  #     DT::datatable(
  #       sube_gh_kapasite_tbl_app,
  #       selection = 'none',
  #       # style = 'bootstrap',
  #       class = "compact stripe",
  #       options = list(
  #         dom = 't',
  #         ordering = FALSE,
  #         pageLength = 15,
  #         scrollY = '200px')
  #     ) %>%
  #       DT::formatPercentage("GH_Kapasite", digits = 0) %>%
  # 
  #       DT::formatStyle(
  #         'Toplam_yol_suresi',
  #         background = DT::styleColorBar(sube_gh_kapasite_tbl_app$Toplam_yol_suresi, '#ACC2DD'),
  #         backgroundSize = '98% 88%',
  #         backgroundRepeat = 'no-repeat',
  #         backgroundPosition = 'center'
  #       ) %>%
  #       DT::formatStyle(
  #         'Toplam_birim_suresi',
  #         background = DT::styleColorBar(sube_gh_kapasite_tbl_app$Toplam_birim_suresi, 'lightblue'),
  #         backgroundSize = '98% 88%',
  #         backgroundRepeat = 'no-repeat',
  #         backgroundPosition = 'center'
  #       ) %>%
  #       DT::formatStyle(
  #         'Toplam_saha_sure',
  #         background = DT::styleColorBar(sube_gh_kapasite_tbl_app$Toplam_saha_sure, 'lightgray'),
  #         backgroundSize = '98% 88%',
  #         backgroundRepeat = 'no-repeat',
  #         backgroundPosition = 'center'
  #       )
  # 
  #   })
  
  
  risk_rate_shiny_tbl <- DT::datatable(
    average_risk_rates_w_forecast,
    selection = 'none',
    # style = 'bootstrap',
    class = "compact stripe", 
    options = list(
      dom = 't',
      ordering = FALSE,
      pageLength = 22)
  ) %>%
    DT::formatStyle(
      'riskrate',
      #background = DT::styleColorBar(average_risk_rates_w_forecast$riskrate, '#d7d8d6'),
      backgroundSize = '98% 88%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>% 
    DT::formatStyle(
      'forecasted_riskrate',
      #background = DT::styleColorBar(average_risk_rates_w_forecast$forecasted_riskrate, '#d7d8d6'),
      backgroundSize = '98% 88%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>% 
    DT::formatStyle(
      'Degisim_Oran_Yuzde',
      #backgroundColor = styleInterval(c(0), c('red', 'green')),
      #backgroundSize = '98% 88%',
      #backgroundRepeat = 'no-repeat', 
      color = styleInterval(c(0), c('green', 'red')),
      fontWeight = 'bold',
      backgroundPosition = 'right' 
    )
  
  output$riskratetbl <- renderDataTable(risk_rate_shiny_tbl)
                                       
  
  
  ##### Prediction_Section ----
  
  # Değişim Oranı
  
  degisim_oran_card <- function (risk = input$risk) {
    
    average_risk_rates_w_forecast_selected <- average_risk_rates_w_forecast %>% 
      filter(Risk == risk) %>% 
      select(Degisim_Oran_Yuzde)
    
    average_risk_rates_w_forecast_selected[[1]]
  }

  
  # Risk Rate
  
  risk_rate_card <- function (risk = input$risk) {
      
      average_risk_rates_w_forecast_selected <- average_risk_rates_w_forecast %>% 
        filter(Risk == risk) %>% 
        select(riskrate)
    average_risk_rates_w_forecast_selected[[1]]
    
    }



  #
  prediction_card <- eventReactive(input$apply, {
    column(
      width = 10,
      info_card(
        title     = h4("Dynamic Forecast"),
        subtitle = h5("Predicted risk change rate  for the next 3 months"),
        value     = h3(paste("%", degisim_oran_card(input$risk))),
        sub_value = h3(risk_rate_card(input$risk)),

        sub_icon  = ifelse(round(degisim_oran_card(input$risk),digits = 1) >= 0.01, "arrow-down", "arrow-up"),

        sub_text_color = ifelse(round(degisim_oran_card(input$risk),digits = 1) <= 0.01 , "success", "danger"), bg_color = "yellow"

      )
    )
  })


  #
  output$single_card <- renderUI({

    prediction_card()

  })
  
  # Infobox
  
  secili_risk_tablosu <- as.tibble(unique(bildirim_loc_tbl_selected$RiskGroupName))
  secili_risk_tablosu

  secilen_risk  <- eventReactive(
    eventExpr = input$risk, 
    valueExpr = {
      
      zzz <- secili_risk_tablosu %>% 
        filter(value %in% input$risk) %>% 
        paste()

    }, 
    ignoreNULL = FALSE 
  )
  

  output$ibox <- renderValueBox({
    infoBox(
      # "Risk",
      h5(secilen_risk(), style="color:red;font-weight: bold")
      # icon = icon("credit-card")
    )
  })


  

}
}

# Run the application 
shinyApp(ui = ui, server = server)
