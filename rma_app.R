
# SRA APP ----

# Bu projede Bildirimler ve SRA uygulaması üzerindeki veriler kullanılarak risk tahminleme yapılması amaçlanmaktadır.


### BURAYA KADAR OLANLAR TASLAK OLARAK YAPILDI.
#Year,day,quarter,month bazlı grafikler (Fnc. yapılacak)
#Kategorilere göre grafiğin şekillenmesi (Fnc. yapılacak)
#Grafik görseli düzenlenecek.
#Report type ve commentler güncellenecek


##################

#PACKAGES ----

if (!require("RColorBrewer")) {
  install.packages("xgboost")
  library(RColorBrewer)
}

library(xgboost)
library(flexdashboard)
library(shiny)
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
library(mapview)
library(leaflet)
library(leaflet.extras)

# Modeling
library(timetk)
library(parsnip)
library(collapsibleTree)

# Apriori

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
if (!require("arules")) {
  install.packages("arules")
  library(RColorBrewer)
}
if (!require("arulesViz")) {
  install.packages("arulesViz")
  library(RColorBrewer)
}

library(arules)
library(arulesViz)
library(lubridate)


# 1.0 TABLES ----

## 1.1. SMART BIRIMLER ----
smart_birimler_tbl <- readRDS("../RMA/Data/smart_birimler_saved.rds")


smart_birimler_tbl$Il <- smart_birimler_tbl$Il %>% iconv(to="UTF-8")
smart_birimler_tbl$Ilce <- smart_birimler_tbl$Ilce %>% iconv(to="UTF-8")

smart_birimler_tbl

## 1.2 LOKASYON ----
birimler_lokasyon_tbl <- readRDS("../RMA/Data/birimler_lokasyon_saved.rds")
birimler_lokasyon_tbl

## 1.3 BİLDİRİMLER ----
bildirim_tbl <- readRDS("../RMA/Data/bildirim_saved.rds")

str(bildirim_tbl)
bildirim_tbl <- bildirim_tbl %>% arrange(desc(NotificationTimestamp)) %>% 
  filter(difftime(today(), NotificationTimestamp, units = "days")<= 1460)

## 1.4 BİLDİRİM-LOKASYON JOIN ----
bildirim_loc_tbl <- bildirim_tbl %>% 
  inner_join(birimler_lokasyon_tbl, by = "UnitId")

bildirim_loc_tbl$Il <- str_to_upper(bildirim_loc_tbl$Il)
bildirim_loc_tbl$Ilce <- str_to_upper(bildirim_loc_tbl$Ilce)
bildirim_loc_tbl$Mahallesi <- str_to_upper(bildirim_loc_tbl$Mahallesi)


bildirim_loc_tbl$Il <- bildirim_loc_tbl$Il %>% iconv(to="UTF-8")
bildirim_loc_tbl$Ilce <- bildirim_loc_tbl$Ilce %>% iconv(to="UTF-8")
bildirim_loc_tbl$Mahallesi <- bildirim_loc_tbl$Mahallesi %>% iconv(to="UTF-8")
bildirim_loc_tbl$Adres <- bildirim_loc_tbl$Adres %>% iconv(to="UTF-8")
bildirim_loc_tbl$Category <- bildirim_loc_tbl$Category %>% iconv(to="UTF-8")
bildirim_loc_tbl$SubCategory <- bildirim_loc_tbl$SubCategory %>% iconv(to="UTF-8")
bildirim_loc_tbl$RiskGroupName <- bildirim_loc_tbl$RiskGroupName %>% iconv(to="UTF-8")

# 2.0 DATA PREPARATION ----

## Select/mutate/rename required fields in tables ----

#
#
bildirim_loc_tbl_selected <- bildirim_loc_tbl %>%
  select(Bölge, BranchId, Proje, Birim, AxBirimId, Enlem, Boylam, Il, Ilce, Segment,AltSegment, HizmetYeri, NotificationTimestamp,
         TespitEdilenAlan, GroupName, Category, SubCategory, RiskGroupName) %>%
  separate(col = NotificationTimestamp, sep = " ", into = c("Tarih", "Saat"), remove = FALSE) %>%
  select(-NotificationTimestamp) %>%
  mutate(Date = as.Date(Tarih)) %>%
  filter(GroupName != "Bilgi" ) 
  

bildirim_loc_tbl_selected
saveRDS(bildirim_loc_tbl_selected, file = "../RMA/Data/bildirim_loc_tbl_selected.rds")

bildirim_loc_tbl_selected_date <- bildirim_loc_tbl_selected %>%
  group_by(Date, Proje) %>%
  summarise(ToplamBildirim = length(SubCategory)) %>% 
  ungroup() 


proje_risk_date <- bildirim_loc_tbl_selected %>%
  mutate(Date = floor_date(Date, unit = "month")) %>%
  group_by(Date, Proje) %>%
  summarise(ToplamBildirim = length(SubCategory)) %>%
  ungroup() 
proje_risk_date

#

risk_loc_tbl_selected <- bildirim_loc_tbl_selected %>%
  filter(RiskGroupName != "Diğer")
#filter(RiskGroupName == "Hırsızlık")
risk_loc_tbl_selected

risk_loc_tbl_selected_date <-risk_loc_tbl_selected %>%
  group_by(Date) %>%
  summarise(ToplamBildirim = length(SubCategory)) %>%
  ungroup()

risk_loc_tbl_selected_date

#

risk_tbl <- bildirim_loc_tbl_selected_date %>%
  left_join(risk_loc_tbl_selected_date , by = "Date") %>%
  rename(ToplamBildirim = "ToplamBildirim.x",
         ToplamRisk = "ToplamBildirim.y") %>%
  mutate(RiskOranı = ToplamRisk/ToplamBildirim)


risk_tbl_rate <- risk_tbl %>%
  mutate(Date = floor_date(Date, unit = "month")) %>%
  group_by(Date) %>%
  summarise(OrtRisk = round(mean(RiskOranı), digits = 2)) %>%
  ungroup() %>% 
  mutate(newrate = round(((OrtRisk - min(OrtRisk)) / (max(OrtRisk) - min(OrtRisk)) ) * ((10 - 0) + 0),digits = 1)) 


risk_tbl_rate 

#
#new_value = ( (old_value - old_min) / (old_max - old_min) ) * (new_max - new_min) + new_min
#


d <- density(risk_tbl_rate$newrate)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")

max(risk_tbl_rate$newrate)
min(risk_tbl_rate$newrate)



#3.0 TS VIZ ----


aggregate_risk_tbl <- function(bildirim_loc_tbl_selected, risk = c("Terörist Tehdidi"), time_unit = "week") {
  
  bildirim_loc_tbl_selected_date <- bildirim_loc_tbl_selected %>%
    
    filter(RiskGroupName %in% risk) %>% 
    group_by(Date) %>%
    summarise(ToplamBildirim = length(SubCategory)) %>%
    ungroup()
  
  risk_tbl <- bildirim_loc_tbl_selected_date %>%
    left_join(risk_loc_tbl_selected_date, by = "Date") %>%
    rename(ToplamBildirim = "ToplamBildirim.y",
           ToplamRisk = "ToplamBildirim.x") %>%
    mutate(RiskOranı = ToplamRisk/ToplamBildirim)
  
  
  risk_tbl_rate <- risk_tbl %>%
    mutate(Date = floor_date(Date, unit = time_unit)) %>%
    group_by(Date) %>%
    summarise(OrtRisk = round(mean(RiskOranı), digits = 2)) %>%
    ungroup() %>% 
    mutate(newrate = round((((OrtRisk - min(OrtRisk)) / (max(OrtRisk) - min(OrtRisk)) ) * (10 - 0) + 0), digits = 1)) %>%
    select(-OrtRisk) %>% 
    mutate(label_text = str_glue("Date: {Date}
                                 Ort. Risk: {newrate}"))
  
  return(risk_tbl_rate)
}

ts_data_risk <- aggregate_risk_tbl(bildirim_loc_tbl_selected, risk = "Yangın", time_unit = "month")

ts_data_risk


# TS Visualization 

plot_time_series_risk <- function(ts_data_risk) {
  g3 <- ts_data_risk %>%
    ggplot(aes(Date, newrate)) +
    geom_line(col = "#2c3e50") +
    geom_point(aes(text = label_text), color = "#2c3e50", size = 0.5) +
    geom_smooth(method = "loess", span = 0.3) +
    theme_tq() +
    theme(plot.background = element_rect(fill = "#d7d8d6")) +
    expand_limits(y = 0) +
    labs(x = "", y = "") 
  
  ggplotly(g3, tooltip = "text") %>% 
    layout(margin = list(b = 150))
  
}

bildirim_loc_tbl_selected %>% 
  aggregate_risk_tbl(risk = "Hırsızlık", time_unit = "day") %>% 
  plot_time_series_risk()



#
smart_birimler_tbl_selected <- smart_birimler_tbl %>%
  select(OrgBolgeAd, OrgSubeAd, OrgSubeId, MusProjeAd, AxProjeId, Ad, AxBirimId, MusSegmentAd, MusAltSegmentAd, MusHizmetYeriAd, AcilisTarihi, KapanisTarihi) %>% 
  rename("MusBirimAd"  = Ad) %>% 
  mutate(AcilisTarihi = as.Date(AcilisTarihi),
         KapanisTarihi = as.Date(KapanisTarihi))

smart_birimler_tbl_selected


# FORECAST ----

# timetk

data <- bildirim_loc_tbl_selected %>% 
  aggregate_risk_tbl(risk = "Hırsızlık", time_unit = "day") 

data

data %>% tk_index() %>% tk_get_timeseries_signature()
data %>% tk_index() %>% tk_get_timeseries_summary()
tk_get_timeseries_unit_frequency()
data %>% tk_get_timeseries_variables()

data %>% tk_augment_timeseries_signature()

train_tbl <- data %>% tk_augment_timeseries_signature()
train_tbl


future_data_tbl <- data %>% 
  tk_index() %>% tk_make_future_timeseries(length_out = 12, inspect_weekdays = TRUE, inspect_months = TRUE) %>% 
  tk_get_timeseries_signature()

future_data_tbl %>% tail()
data %>%  tail()


# XGBOOST ----

seed <- 123
set.seed(seed)

model_xgboost <- boost_tree(mode = "regression", 
                            mtry = 20, 
                            trees = 500, 
                            min_n = 3, 
                            tree_depth = 8, 
                            learn_rate = 0.01, 
                            loss_reduction = 0.01) %>% 
  set_engine("xgboost") %>% 
  fit.model_spec(formula = newrate ~ . , data = train_tbl %>% select(-Date, -label_text, -diff))

model_xgboost

## predict ----

prediction_tbl <- predict(model_xgboost, new_data = future_data_tbl) %>% 
  bind_cols(future_data_tbl) %>% 
  select(.pred, index) %>% 
  rename(newrate = .pred,
         Date = index) %>% 
  mutate(label_text = str_glue("Date: {Date}
                               OrtRisk: {newrate}")) %>% 
  add_column(key = "Prediction")


output_tbl <- data %>% add_column(key = "Actual") %>% 
  bind_rows(prediction_tbl)

length_out <- 12
seed <- 123

generate_forecast <-  function(data, length_out = 12, seed = NULL) {
  
  train_tbl <- data %>% tk_augment_timeseries_signature()
  
  future_data_tbl <- data %>% 
    tk_index() %>% tk_make_future_timeseries(length_out = length_out, inspect_weekdays = TRUE,
                                             inspect_months = TRUE) %>% 
    tk_get_timeseries_signature()
  
  
  time_scale <- data %>% 
    tk_index() %>% 
    tk_get_timeseries_summary() %>% 
    pull(scale)
  
  if (time_scale == "year") {
    
    model <- linear_reg(mode = "regression") %>% 
      set_engine(engine = "lm") %>% 
      fit.model_spec(formula = newrate ~ . , data = train_tbl %>% select(newrate, index.num))
    
  } else {
    
    seed <- seed
    set.seed(seed)
    
    model <- boost_tree(mode = "regression", 
                        mtry = 20, 
                        trees = 500, 
                        min_n = 3, 
                        tree_depth = 8, 
                        learn_rate = 0.01, 
                        loss_reduction = 0.01) %>% 
      set_engine("xgboost") %>% 
      fit.model_spec(formula = newrate ~ . , data = train_tbl %>% select(-Date, -label_text, -diff))
    
  }
  
  prediction_tbl <- predict(model, new_data = future_data_tbl) %>% 
    bind_cols(future_data_tbl) %>% 
    select(.pred, index) %>% 
    rename(newrate = .pred,
           Date = index) %>% 
    mutate(label_text = str_glue("Date: {Date}
                                   OrtRisk: {newrate}")) %>% 
    add_column(key = "Prediction")
  
  output_tbl <- data %>% add_column(key = "Actual") %>% 
    bind_rows(prediction_tbl)
  
  return (output_tbl)
}

# Forecast tibble

bildirim_loc_tbl_selected %>% 
  aggregate_risk_tbl(risk = "Hırsızlık", time_unit = "month") %>% 
  generate_forecast(length_out = 12, seed = 123) 


# plot

data <- bildirim_loc_tbl_selected %>% 
  aggregate_risk_tbl(risk = "Hırsızlık", time_unit = "month") %>% 
  generate_forecast(length_out = 2, seed = 123)

g <- data %>% 
  ggplot(aes(Date, newrate, color = key)) +
  geom_line() +
  geom_point(aes(text = label_text), size = 0.5) +
  geom_smooth(method = "loess", span = 0.2) +
  
  theme_tq_dark() +
  scale_fill_tq(theme = "dark") +
  labs(x = "", y = "")


ggplotly(g, tooltip = "label_text")

# plot forecast

data <-  bildirim_loc_tbl_selected %>% 
  aggregate_risk_tbl(risk = "Hırsızlık", time_unit = "year") %>% 
  generate_forecast(length_out = 1, seed = 123)

plot_forecast <-function(data) {
  
  # Yearly - LM - Smoother
  
  time_scale <- data %>% 
    tk_index() %>%
    tk_get_timeseries_summary() %>% 
    pull(scale)
  
  
  # Only 1 Prediction - points
  
  n_predictions <- data %>% 
    filter(key == "Prediction") %>% 
    nrow()
  
  g <- data %>% 
    ggplot(aes(Date, newrate, color = key)) +
    geom_line() +
    # geom_point(aes(text = label_text), size = 0.5) +
    # geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    scale_color_tq() +
    theme(plot.background = element_rect(fill = "#d7d8d6")) +
    labs(x = "", y = "") +
    expand_limits(y = 0)
  
  #Yearly lm smoother
  
  if (time_scale == "year") {
    
    g <-  g +
      geom_smooth(method = "lm") 
  } else {
    g <- g + geom_smooth(method = "loess", span = 0.2)
  }
  
  # Only 1 Prediction
  
  if (n_predictions == 1) {
    g <-  g +
      geom_point(aes(text = label_text), size = 1) 
    
  } else {
    g <-  g +
      geom_point(aes(text = label_text), size = 0.2) 
    
  }
  
  
  ggplotly(g, tooltip = "label_text")
  
}

bildirim_loc_tbl_selected %>% 
  aggregate_risk_tbl(risk = "Hırsızlık", time_unit = "month") %>% 
  generate_forecast(length_out = 12, seed = 123) %>% 
  plot_forecast()


# 4.0 MAP ----

bildirim_loc_tbl_selected$Enlem <- as.double(bildirim_loc_tbl_selected$Enlem)
bildirim_loc_tbl_selected$Boylam <- as.double(bildirim_loc_tbl_selected$Boylam)


map_tbl <- bildirim_loc_tbl_selected %>%
  group_by(Bölge, Birim, Segment, HizmetYeri, Date, RiskGroupName, Boylam, Enlem) %>%
  summarise(ToplamBildirim = length(SubCategory)) %>% 
  filter(!is.na(Boylam)) %>%
  filter(!is.na(Enlem)) %>% 
  ungroup()


str(map_tbl)


birim_map <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = map_tbl, lng = map_tbl$Boylam, lat = map_tbl$Enlem, radius = 2)

birim_map


# 0.0 VISUALIZATIONS ----

## datalablogo ----

moxbuller = function(n) {   
  u = runif(n)   
  v = runif(n)   
  x = cos(2*pi*u)*sqrt(-2*log(v))  
  y = sin(2*pi*v)*sqrt(-2*log(u))
  r = list(x=x, y=y)
  return(r) 
}
r = moxbuller(50000) 
par(bg="black") 
par(mar=c(0,0,0,0)) 
plot(r$x,r$y, pch=".", col="red", cex=1.2)

# 


x = runif(1000)
y = x/runif(1000)

cexes = 10*y/max(y) # For circle size
par(bg="black") # I see a white background and I want it painted black.
par(mar=c(0,0,0,0)) # Margins? We don't kneed no stinkin' margins.
plot(x,log(y), pch=20, col="white", cex=cexes)



# Average Risk rate for last 3 months

risk_rate_tbl_last3months <-bildirim_loc_tbl_selected %>% 
  aggregate_risk_tbl(risk = "Terörist Tehdidi", time_unit = "month") %>% 
  filter(difftime(today(), Date, units = "days") <= 120) %>% 
  select(-label_text)

risk_rate_tbl_last3months

avrg_risk_rate <- round(mean(risk_rate_tbl_last3months$newrate),digits = 1)

avrg_risk_rate



# gauge 2 ----

library(plotly)

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
      list(range = c(8, 10), color = "black")),
    threshold = list(
      line = list(color = "red", width = 4),
      thickness = 0.75,
      value = 8))) 
fig <- fig %>%
  layout(margin = list(l=20,r=30))

fig

# Risk rate table for all risks

risk_tablosu <- data.frame(Risk = unique(bildirim_loc_tbl_selected$RiskGroupName))
risk_tablosu


df1 <- data.frame()
df1

t <- for (i in 1:length(risk_tablosu$Risk)) {
  
  all_risk_rates_tbl <- bildirim_loc_tbl_selected %>% 
    aggregate_risk_tbl(risk = risk_tablosu$Risk[[i]], time_unit = "month") %>% 
    filter(difftime(today(), Date, units = "days") <= 120) %>% 
    select(-label_text)
  
  avrg_allrisk_rate <- round(mean(all_risk_rates_tbl$newrate),digits = 1)

  output = print(avrg_allrisk_rate)
  
  df1 <- rbind(df1, output)
  
}

colnames(df1)[1]  <- "riskrate"

risk_df <- df1 %>% head(length(risk_tablosu$Risk))
risk_df

risk_rate_tbl <- cbind(risk_tablosu,risk_df) 
risk_rate_tbl <- risk_rate_tbl %>% 
  mutate(riskrate = ifelse(riskrate <= 0, 0.1, riskrate)) %>% 
  mutate(riskrate = ifelse(is.na(riskrate), 0.1, riskrate))
risk_rate_tbl
   
# Risk Forecast Tibble 

risk_tablosu2 <- data.frame(Risk = unique(bildirim_loc_tbl_selected$RiskGroupName)) %>% 
  filter(Risk != "Soygun")
risk_tablosu2

df2 <- data.frame()
df2

t2 <- for (i in 1:length(risk_tablosu2$Risk)) {
  
  risk_forecast_tbl <- bildirim_loc_tbl_selected %>% 
    aggregate_risk_tbl(risk = risk_tablosu2$Risk[[i]], time_unit = "month") %>% 
    generate_forecast(length_out = 12, seed = 123) %>% 
    tail(length_out) %>% 
    head(3) %>% 
    select(-label_text, -key)
  
  avrg_risk_rate2 <- round(mean(risk_forecast_tbl$newrate),digits = 1)
  
  output2 = print(avrg_risk_rate2)
  
  df2 <- rbind(df2, output2)
  
}

colnames(df2)[1]  <- "forecasted_riskrate"

risk_df2 <- df2 %>% head(length(risk_tablosu2$Risk))
risk_df2

risk_rate_tbl2 <- cbind(risk_tablosu2,risk_df2) 
risk_rate_tbl2 <- risk_rate_tbl2 %>% 
  mutate(forecasted_riskrate = ifelse(forecasted_riskrate <= 0, 0.1, forecasted_riskrate)) %>% 
  mutate(forecasted_riskrate = ifelse(is.na(forecasted_riskrate), 0.1, forecasted_riskrate))

risk_rate_tbl2

# Past and Future Risk Rates Tibble

average_risk_rates_w_forecast <- risk_rate_tbl %>% 
  left_join(risk_rate_tbl2, by = "Risk") %>%
  mutate(Degisim_Oran_Yuzde = round(((forecasted_riskrate - riskrate) / riskrate), digits = 1) ) %>% 
  arrange(desc(riskrate)) %>% 
  mutate(forecasted_riskrate = ifelse(is.na(forecasted_riskrate), 0.1, forecasted_riskrate)) %>% 
  mutate(Degisim_Oran_Yuzde = ifelse(is.na(Degisim_Oran_Yuzde), 0.1, Degisim_Oran_Yuzde)) %>% 
  mutate(Degisim_Oran_Yuzde = Degisim_Oran_Yuzde *100)

#average_risk_rates_w_forecast$Degisim_Oran = formattable::percent(average_risk_rates_w_forecast$Degisim_Oran)

average_risk_rates_w_forecast

average_risk_rates_w_forecast_func <- function (risk = "Hırsızlık") {
  
  average_risk_rates_w_forecast_selected_change <- average_risk_rates_w_forecast %>% 
    filter(Risk == risk) %>% 
    select(Degisim_Oran_Yuzde)
  average_risk_rates_w_forecast_selected_change
}

average_risk_rates_w_forecast_func("Asayiş")



average_risk_rates_w_forecast_func_last <- function (risk = "Hırsızlık") {
  
  average_risk_rates_w_forecast_selected <- average_risk_rates_w_forecast %>% 
    filter(Risk == risk) %>% 
    select(riskrate)
  average_risk_rates_w_forecast_selected[[1]]
}

average_risk_rates_w_forecast_func_last(risk = "Asayiş")



average_risk_rates_w_forecast_func_last <- eventReactive(input$apply, {
  
  average_risk_rates_w_forecast_selected_change <- function (risk = "Hırsızlık") {
    
    average_risk_rates_w_forecast %>% 
      filter(Risk == risk) %>% 
      select(Degisim_Oran_Yuzde)
  }
  
  average_risk_rates_w_forecast_selected(input$risk)
  
})


str(average_risk_rates_w_forecast_func_last(risk = "Asayiş"))

# 

secili_risk_tablosu <- as.tibble(unique(bildirim_loc_tbl_selected$RiskGroupName))
secili_risk_tablosu

str(secili_risk_tablosu)

paste(secili_risk_tablosu)


zzz <- secili_risk_tablosu %>% 
  filter(value %in% c("Hırsızlık", "Asayiş")) %>% 
  pull(value) %>% 
  paste()


zzz


secilen_risk  <- eventReactive(
  eventExpr = input$risk, 
  valueExpr = {
    
    zzz
    
  }, 
  ignoreNULL = FALSE 
)


#


  data1 <- bildirim_loc_tbl_selected %>%  
  # select(Proje, Birim, Tarih, RiskGroupName, SubCategory, Category) %>%
  group_by(Proje, Birim, Tarih, RiskGroupName, SubCategory, Category) %>%
  summarise(Sub = length(SubCategory)) %>%
  ungroup()


data1


data2<- data1 %>%
  group_by(Proje, Birim, RiskGroupName) %>%
  summarise(Min = min(Sub),
            Max = max(Sub)) %>%
  ungroup()

data2


data_combine <- data1 %>% 
  left_join(data2, by = c("Proje", "Birim","RiskGroupName"))

data_combine



data4<- data2 %>%
  inner_join(data3, by = "Birim") %>% #RiskGroupName 'e göre birlestirilir(?)
  select(-Proje.y , -RiskGroupName.y , -Sub.y , -Minimum.y, -Max, -Min) %>%
  rename(SubcategorySayısı = "Sub.x")




# Map ----

#Data
str(bildirim_loc_tbl)
bildirim_loc_tbl$Enlem <- as.numeric(bildirim_loc_tbl$Enlem)
bildirim_loc_tbl$Boylam<- as.numeric(bildirim_loc_tbl$Boylam)


bildirim_loc_tbl <- bildirim_loc_tbl %>%
  tail(50000)

#Subset Data
x <- bildirim_loc_tbl %>%
  dplyr::select(Bölge, Proje, Birim, Enlem,Boylam) 


x


#Mapping for all units



#map_all<- leaflet() %>%
#  addTiles() %>%
# addCircleMarkers(data = x, lng = x$Boylam, lat = x$Enlem, radius = 2)



#Function for distance



earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

earth.dist

#Filtering and mapping some specific -



x1 <- x %>%
  mutate(distance = earth.dist(32.53112,39.98345, x$Boylam, x$Enlem)) %>%
  filter(distance <= 50)
x1

x2 <- x %>%
  mutate(distance = earth.dist(29.45234,38.76534, x$Boylam, x$Enlem)) %>%
  filter(distance <= 50)
x2

x3 <- x %>%
  mutate(distance = earth.dist(35.45234,38.76534, x$Boylam, x$Enlem)) %>%
  filter(distance <= 50)
x3

nokta <- data.frame (Boylam  = c(32.53112, 29.45234,35.45234),
                  Enlem = c(39.98345,38.76534,38.76534)
                  )

nokta


map_filteredx <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = bildirim_loc_tbl, lng = bildirim_loc_tbl$Boylam, lat = bildirim_loc_tbl$Enlem, radius = 1) %>%
  addCircles(
    lng = nokta$Boylam[[1]],
    lat = nokta$Enlem[[1]], 
    radius = 50000,
    weight = 5,
    opacity = 0.5, 
    fill = T, fillColor = "red",
    fillOpacity = 0.1,
    # smoothFactor = 16,
    color = "red") %>% 
  addCircles(
    lng = x1$Boylam,
    lat = x1$Enlem,
    weight = 5,
    opacity = 0.5,
    fill = TRUE,
    fillOpacity = 0.1,
    color = "green") %>%
  addCircles(
    lng = nokta$Boylam[[2]],
    lat = nokta$Enlem[[2]], 
    radius = 50000,
    weight = 5,
    opacity = 0.5, 
    fill = T, fillColor = "red",
    fillOpacity = 0.1,
    # smoothFactor = 16,
    color = "red") %>% 
  addCircles(
    lng = x2$Boylam,
    lat = x2$Enlem,
    weight = 5,
    opacity = 0.5,
    fill = TRUE,
    fillOpacity = 0.1,
    color = "green") %>% 
  addCircles(
    lng = nokta$Boylam[[3]],
    lat = nokta$Enlem[[3]], 
    radius = 50000,
    weight = 5,
    opacity = 0.5, 
    fill = T, fillColor = "red",
    fillOpacity = 0.1,
    # smoothFactor = 16,
    color = "red") %>% 
  addCircles(
    lng = x3$Boylam,
    lat = x3$Enlem,
    weight = 5,
    opacity = 0.5,
    fill = TRUE,
    fillOpacity = 0.1,
    color = "green")


map_filteredx 


birimler_lokasyon_tbl$Enlem <- as.numeric(birimler_lokasyon_tbl$Enlem)
birimler_lokasyon_tbl$Boylam<- as.numeric(birimler_lokasyon_tbl$Boylam)

birimler_lokasyon_tbl <- birimler_lokasyon_tbl %>% 
  filter(!is.na(Enlem)) 
  
  

y <- birimler_lokasyon_tbl %>% 
  select(Birim, Enlem, Boylam) %>% 
  filter(!is.na(Enlem)) 
y



y <- y %>%
  mutate(distance = earth.dist(32.53000,39.98000, Boylam, Enlem)) %>%
  filter(distance <=250) 
y 

map_filtered <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = birimler_lokasyon_tbl, lng = birimler_lokasyon_tbl$Boylam, lat = birimler_lokasyon_tbl$Enlem, radius =1) %>%
  addPolygons(
    lng = y$Boylam,
    lat = y$Enlem,
    weight = 5,
    opacity = 0.5,
    fill = TRUE,
    fillOpacity = 0.1,
    smoothFactor = 1,
    color = "red")

map_filtered

## Risk Map ----

### Main Table ----

risk_loc_tbl_selected
str(risk_loc_tbl_selected)

risk_loc_tbl_selected$Enlem <- as.numeric(risk_loc_tbl_selected$Enlem)
risk_loc_tbl_selected$Boylam<- as.numeric(risk_loc_tbl_selected$Boylam)

birim_risk_tbl <- risk_loc_tbl_selected %>% 
  group_by(Segment, AltSegment, Bölge, BranchId, Proje, Birim, Enlem, Boylam, RiskGroupName) %>% 
  summarise(Total_Risk = length(RiskGroupName)) %>% 
  ungroup()

birim_risk_tbl



### Map ----

#### Filtered Table / Circles ----
birim_risk_tbl %>% view()

birim_risk_tbl_selected <- birim_risk_tbl %>% 
  filter(Proje == "COCA-COLA SATIŞ VE DAĞITIM A.Ş. (M03822)") %>% 
  filter(RiskGroupName == "Hırsızlık") %>% 
  arrange(desc(Total_Risk)) %>% 
  head(3)

birim_risk_tbl_selected

#### Distance ----

dist <- 50

loc1 <- birim_risk_tbl %>%
  mutate(distance = earth.dist(birim_risk_tbl_selected$Boylam[[1]],birim_risk_tbl_selected$Enlem[[1]], birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
  filter(distance <= dist)
loc1

loc2 <- birim_risk_tbl %>%
  mutate(distance = earth.dist(birim_risk_tbl_selected$Boylam[[2]],birim_risk_tbl_selected$Enlem[[2]], birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
  filter(distance <= dist)
loc2

loc3 <- birim_risk_tbl %>%
  mutate(distance = earth.dist(birim_risk_tbl_selected$Boylam[[3]],birim_risk_tbl_selected$Enlem[[3]], birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
  filter(distance <= dist)
loc3

#### Results / Map ----

risk_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = birim_risk_tbl, lng = birim_risk_tbl$Boylam, lat = birim_risk_tbl$Enlem, radius = 1) %>%
  #
  addCircles(
    lng = birim_risk_tbl_selected$Boylam[[1]],
    lat = birim_risk_tbl_selected$Enlem[[1]], 
    radius = 50000,
    weight = 5,
    opacity = 0.5, 
    fill = T, fillColor = "red",
    fillOpacity = 0.1,
    # smoothFactor = 16,
    color = "red") %>% 
  addCircles(
    lng = loc1$Boylam,
    lat = loc1$Enlem,
    weight = 5,
    opacity = 0.5,
    fill = TRUE,
    fillOpacity = 0.1,
    color = "green") %>%
  
  #
  addCircles(
    lng = birim_risk_tbl_selected$Boylam[[2]],
    lat = birim_risk_tbl_selected$Enlem[[2]], 
    radius = 50000,
    weight = 5,
    opacity = 0.5, 
    fill = T, fillColor = "red",
    fillOpacity = 0.1,
    # smoothFactor = 16,
    color = "red") %>% 
  addCircles(
    lng = loc2$Boylam,
    lat = loc2$Enlem,
    weight = 5,
    opacity = 0.5,
    fill = TRUE,
    fillOpacity = 0.1,
    color = "green") %>% 
  
  #
  addCircles(
    lng = birim_risk_tbl_selected$Boylam[[3]],
    lat = birim_risk_tbl_selected$Enlem[[3]], 
    radius = 50000,
    weight = 5,
    opacity = 0.5, 
    fill = T, fillColor = "red",
    fillOpacity = 0.1,
    # smoothFactor = 16,
    color = "red") %>% 
  addCircles(
    lng = loc3$Boylam,
    lat = loc3$Enlem,
    weight = 5,
    opacity = 0.5,
    fill = TRUE,
    fillOpacity = 0.1,
    color = "green")


risk_map


### Area Based Risk ----

nested_table <-  birim_risk_tbl %>% 
  filter(RiskGroupName == "Asayiş") %>%
  #filter(AltSegment == "AVM") %>% 
  group_by(Birim) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(nested_id = row_number())

nested_table


nested_table_filtered <- nested_table %>% 
  filter(Birim == "ANKARA FORUM (M03419-0010)") %>% 
  filter(filter(RiskGroupName == "Hırsızlık"))
nested_table_filtered

#
dist <- 100

loc1 <- birim_risk_tbl %>%
  mutate(distance = earth.dist(birim_risk_tbl_selected$Boylam[[1]],birim_risk_tbl_selected$Enlem[[1]], birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
  filter(distance <= dist)
loc1

# Risk_Map_Function ----

contenido <- paste(sep = "<br/>",
                   #paste0("<img src='https://www.r-project.org/logo/Rlogo.png", "' />"),
                   paste0("<b>Name: </b>", "Frigorífico Anglo"),
                   paste0("<b>Place: </b>", "Fray Bentos, Uruguay"),
                   paste0("<a href='https://en.wikipedia.org/wiki/Frigor%C3%ADfico_Anglo_del_Uruguay", "'>Link</a>"))

risk_map_func <- function(maprisk = "Hırsızlık", diameter = 100, selected_lng = 36.2, selected_lat = 29.2){
  
  dist <- diameter
  
  birim_say <- length(nested_table$Birim)
  glimpse(birim_say) 
  
  df <- data.frame()
  df
  
  x <- for (i in 1:birim_say) {
    
    area_risk <- birim_risk_tbl %>% 
      
      mutate(distance = earth.dist(nested_table$data[[i]]$Boylam,nested_table$data[[i]]$Enlem, birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
      filter(RiskGroupName == maprisk) %>% 
      filter(distance <= dist)
    
    riskmean <- mean(area_risk$Total_Risk)
    
    output = print(riskmean)
    
    df <- rbind(df, output)
    
  }
  
  df
  names(df)[1] ="avrgrisk"
  str(df)
  df %>% head()
  
  location_risk_tbl <- bind_cols(nested_table, df) %>% 
    arrange(desc(avrgrisk)) 

  
  #location_risk_tbl <- location_risk_tbl[55:59,]
  
  #location_risk_tbl
  
  #### Arae Based Risk Map ----
  
  ##### Area Based Distance ----
  
  
  loca1 <- birim_risk_tbl %>%
    mutate(distance = earth.dist(location_risk_tbl$data[[1]]$Boylam,location_risk_tbl$data[[1]]$Enlem, birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
    filter(distance <= dist)
  loca1
  
  loca2 <- birim_risk_tbl %>%
    mutate(distance = earth.dist(location_risk_tbl$data[[2]]$Boylam,location_risk_tbl$data[[2]]$Enlem, birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
    filter(distance <= dist)
  loca2
  
  loca3 <- birim_risk_tbl %>%
    mutate(distance = earth.dist(location_risk_tbl$data[[3]]$Boylam,location_risk_tbl$data[[3]]$Enlem, birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
    filter(distance <= dist)
  loca3
  
  loca4 <- birim_risk_tbl %>%
    mutate(distance = earth.dist(location_risk_tbl$data[[4]]$Boylam,location_risk_tbl$data[[4]]$Enlem, birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
    filter(distance <= dist)
  loca4
  
  loca5 <- birim_risk_tbl %>%
    mutate(distance = earth.dist(location_risk_tbl$data[[5]]$Boylam,location_risk_tbl$data[[5]]$Enlem, birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
    filter(distance <= dist)
  loca5

  loca6 <- birim_risk_tbl %>%
    mutate(distance = earth.dist(as.numeric(selected_lng),as.numeric(selected_lat), birim_risk_tbl$Boylam, birim_risk_tbl$Enlem)) %>%
    filter(distance <= dist)
  loca6


  
  risk_map <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = birim_risk_tbl, lng = birim_risk_tbl$Boylam, lat = birim_risk_tbl$Enlem, radius = 1) %>%
    addSearchOSM() %>%
    addReverseSearchOSM() %>% 
     
    #
    addCircles(
      lng = location_risk_tbl$data[[1]]$Boylam,
      lat = location_risk_tbl$data[[1]]$Enlem, 
      radius = dist*1000,
      weight = 5,
      opacity = 0.5, 
      fill = T, fillColor = "red",
      fillOpacity = 0.1,
      # smoothFactor = 16,
      color = "red") %>%
    addPopups(data = location_risk_tbl$data[[1]], 
              lng = location_risk_tbl$data[[1]]$Boylam, 
              lat = location_risk_tbl$data[[1]]$Enlem, 
              popup=contenido, 
              options = popupOptions(closeButton = TRUE)) %>%
    # setView(lng = location_risk_tbl$data[[1]]$Boylam,
    #         lat = location_risk_tbl$data[[1]]$Enlem,
    #         zoom=7) %>%
    addCircles(
      lng = loca1$Boylam,
      lat = loca1$Enlem,
      weight = 5,
      opacity = 0.5,
      fill = TRUE,
      fillOpacity = 0.1,
      color = "green") %>%
    
    #
    addCircles(
      lng = location_risk_tbl$data[[2]]$Boylam,
      lat = location_risk_tbl$data[[2]]$Enlem,
      radius = dist*1000,
      weight = 5,
      opacity = 0.5, 
      fill = T, fillColor = "red",
      fillOpacity = 0.1,
      # smoothFactor = 16,
      color = "red") %>% 
    addCircles(
      lng = loca2$Boylam,
      lat = loca2$Enlem,
      weight = 5,
      opacity = 0.5,
      fill = TRUE,
      fillOpacity = 0.1,
      color = "green") %>% 
    
    #
    addCircles(
      lng = location_risk_tbl$data[[3]]$Boylam,
      lat = location_risk_tbl$data[[3]]$Enlem, 
      radius = dist*1000,
      weight = 5,
      opacity = 0.5, 
      fill = T, fillColor = "red",
      fillOpacity = 0.1,
      # smoothFactor = 16,
      color = "red") %>% 
    addCircles(
      lng = loca3$Boylam,
      lat = loca3$Enlem,
      weight = 5,
      opacity = 0.5,
      fill = TRUE,
      fillOpacity = 0.1,
      color = "green") %>%
    
    #
    addCircles(
      lng = location_risk_tbl$data[[4]]$Boylam,
      lat = location_risk_tbl$data[[4]]$Enlem, 
      radius = dist*1000,
      weight = 5,
      opacity = 0.5, 
      fill = T, fillColor = "red",
      fillOpacity = 0.1,
      # smoothFactor = 16,
      color = "red") %>% 
    addCircles(
      lng = loca4$Boylam,
      lat = loca4$Enlem,
      weight = 5,
      opacity = 0.5,
      fill = TRUE,
      fillOpacity = 0.1,
      color = "green") %>% 
    
    #
    addCircles(
      lng = location_risk_tbl$data[[5]]$Boylam,
      lat = location_risk_tbl$data[[5]]$Enlem, 
      radius = dist*1000,
      weight = 5,
      opacity = 0.5, 
      fill = T, fillColor = "red",
      fillOpacity = 0.1,
      # smoothFactor = 16,
      color = "red") %>% 
    addCircles(
      lng = loca5$Boylam,
      lat = loca5$Enlem,
      weight = 5,
      opacity = 0.5,
      fill = TRUE,
      fillOpacity = 0.1,
      color = "green") %>% 
    
    #
    addCircles(
      lng = selected_lng,
      lat = selected_lat,
      radius = dist*1000,
      weight = 5,
      opacity = 0.5,
      fill = T, fillColor = "black",
      fillOpacity = 0.1,
      # smoothFactor = 16,
      color = "black") %>%
    addCircles(
      lng = loca6$Boylam,
      lat = loca6$Enlem,
      weight = 5,
      opacity = 0.5,
      fill = TRUE,
      fillOpacity = 0.1,
      color = "green") 

  
  risk_map
}


risk_map_func("Hırsızlık", 50)


###

