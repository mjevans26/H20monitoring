library(plotly)
library(lme4)

plot_ly(data = portal_raw[portal_raw$CharacteristicName == "Mercury" & portal_raw$ResultSampleFractionText == "Total",])%>%
  add_trace(type = "scatter", mode = "markers", color = ~ MonitoringLocationIdentifier,
            x = ~Mined, y = ~ResultMeasureValue,
            name = 'Cadmium')

plot_ly(data = portal_raw[portal_raw$CharacteristicName == "pH" &
                            portal_raw$MonitoringLocationIdentifier == "11NPSWRD-CUGA_CPSU_RR",])%>%
  add_trace(type = "scatter", mode = "markers",
            x = ~Date, y = ~ResultMeasureValue,
            name = 'pH')

hist(portal_raw$ResultMeasureValue[portal_raw$CharacteristicName == 'pH'])
hist(portal_raw$ResultMeasureValue[portal_raw$CharacteristicName == 'Cadmium' & portal_raw$ResultMeasureValue < 1])
hist(portal_raw$ResultMeasureValue[portal_raw$CharacteristicName == 'Copper'])
hist(portal_raw$ResultMeasureValue[portal_raw$CharacteristicName == 'Lead'])
hist(portal_raw$ResultMeasureValue[portal_raw$CharacteristicName == 'Mercury'])
hist(portal_raw$ResultMeasureValue[portal_raw$CharacteristicName == 'Turbidity'])
hist(portal_raw$ResultMeasureValue[portal_raw$CharacteristicName == 'Total suspended solids'])

simple_plot <- function(character, measure){
  plot_ly(data = portal_raw[portal_raw$CharacteristicName == character & portal_raw$ResultSampleFractionText == measure,])%>%
    add_trace(type = "scatter", mode = "markers",
              x = ~pMine, y = ~Values,
              name = ~paste(measure, character))
}

simple_plot("Cadmium", "Dissolved")

time_plot <- function(character, measure, min){
  sts <- sites$MonitoringLocationIdentifier[sites$count > min]
  c1 <- portal_raw$MonitoringLocationIdentifier %in% sts
  c2 <- portal_raw$CharacteristicName == character
  c3 <- portal_raw$ResultSampleFractionText == measure
  
  plot_ly(data = portal_raw[c1 & c2 & c3,])%>%
    add_trace(type = "scatter", mode = "markers",
              x = ~pMine, y = ~Values, color = ~ MonitoringLocationIdentifier,
              name = ~paste(measure, character), visible = "legendonly",
              text = ~paste("Condition:", HydrologicCondition, "<br>Event:", HydrologicEvent),
              hoverinfo = "text")
}


make_df <- function(measure, type, count){
  sts <- sites$MonitoringLocationIdentifier[sites$count > count]
  df <- dplyr::filter(portal_raw, CharacteristicName == measure,
              ResultSampleFractionText == type,
              MonitoringLocationIdentifier %in% sts)
  return(df)
}

summary(
  lmer(data = make_df("Copper", "Dissolved", 10), 
     lgValues ~ (1|MonitoringLocationIdentifier) + pMine)
)
