library(shiny)
library(shinydashboard)
library(stringr)
library(ggplot2)
library(DT)
library(maps)
library(leaflet)

function (input, output, session) {
  percentages = c("Admission Rate (%)",
                  "Low Income Repayment Rate (3yr %)",
                  "Middle Income Repayment Rate (3yr %)",
                  "High Income Repayment Rate (3yr %)",
                  "Default Rate (%)",
                  "Loans (%)",
                  "Pell Grants (%)")
  
  currencies = c("Average Tuition ($)",
                 "Average Family Income ($)",
                 "Median Family Income ($)",
                 "Median Debt ($)",
                 "Average Income (Dependents $)",
                 "Average Income (Independents $)")
  
  myData <- reactive({
    collegedata %>%
      select(`School Type`, val=input$densityVal)
  })
  
  output$table <- DT::renderDataTable({
    datatable(collegedata, rownames=FALSE, options=(list(scrollX = TRUE))) %>%
      formatCurrency(currencies) %>%
      formatPercentage(percentages)
  })
  
  output$densityPlot <- renderPlot({
    if (input$densityVal %in% percentages) {
      ggplot(myData(), aes(val, fill=`School Type`)) + geom_density(alpha=0.3) + xlab(input$densityVal) + ylab("Density") + scale_x_continuous(labels = scales::percent)
    } else if (input$densityVal %in% currencies) {
      ggplot(myData(), aes(val, fill=`School Type`)) + geom_density(alpha=0.3) + xlab(input$densityVal) + ylab("Density") + scale_x_continuous(labels = scales::dollar)
    } else {
      ggplot(myData(), aes(val, fill=`School Type`)) + geom_density(alpha=0.3) + xlab(input$densityVal) + ylab("Density")
    }
  })
  
  state_data <- collegedata %>% group_by(State) %>% summarise(`Admission Rate (%)`=mean(`Admission Rate (%)`),
                                                              `Average Family Income ($)`=mean(`Average Family Income ($)`),
                                                              `Median Family Income ($)`=mean(`Median Family Income ($)`),
                                                              `Default Rate (%)`=mean(`Default Rate (%)`),
                                                              `Low Income Repayment Rate (3yr %)`=mean(`Low Income Repayment Rate (3yr %)`),
                                                              `Middle Income Repayment Rate (3yr %)`=mean(`Middle Income Repayment Rate (3yr %)`),
                                                              `High Income Repayment Rate (3yr %)`=mean(`High Income Repayment Rate (3yr %)`))

  state_map = map("state", fill=TRUE, plot=FALSE)
  state_data$FullState <- tolower(state.name[match(state_data$State, state.abb)])

  state_data[state_data$State == "DC", "FullState"] <- "district of columbia" 
  state_data[state_data$State == "WA", "FullState"] <- "washington:main" #Rename states to match with names in map() program
  state_data[state_data$State == "MI", "FullState"] <- "michigan:south"
  state_data[state_data$State == "NY", "FullState"] <- "new york:main"
  state_data[state_data$State == "VA", "FullState"] <- "virginia:main"
  state_data[state_data$State == "NC", "FullState"] <- "north carolina:main"
  state_data[state_data$State == "MA", "FullState"] <- "massachusetts:main"
  state_data <- state_data %>% filter(!is.na(FullState)) 
  state_data <- state_data %>% filter(!(State %in% c("PR", "GU", "VI", "AK", "HI")))
  state_data <- state_data %>% filter(FullState %in% state_map$names)
  
  state_data <- state_data %>% arrange(FullState)
  
  split_list <- function(x) {
    idx <- 1 + cumsum(is.na(x))
    not.na <- !is.na(x)
    split(x[not.na], idx[not.na])
  }
  
  split_x = split_list(state_map$x)
  split_y = split_list(state_map$y)
  
  split_x <- split_x[state_map$names %in% state_data$FullState]
  split_y <- split_y[state_map$names %in% state_data$FullState]
  
  state_map$x = c()
  state_map$y = c()
  
  for (i in 1:length(split_x)) {
    state_map$x = c(state_map$x, split_x[[i]], NA)
    state_map$y = c(state_map$y, split_y[[i]], NA)
  }
  
  output$mapPlot <- renderLeaflet({
    if (input$mapVal %in% percentages) {
      mypalette <- colorBin(c('#fee0d2',
                            '#fcbba1',
                            '#fc9272',
                            '#fb6a4a',
                            '#ef3b2c',
                            '#cb181d',
                            '#a50f15'), 
                          domain = c(min(as.data.frame(state_data[, input$mapVal])),
                                     max(as.data.frame(state_data[, input$mapVal]))),
                          na.color = NA,
                          pretty = TRUE, 
                          bins = 6)
    } else {
      mypalette <- colorBin(c('#fee0d2',
                              '#fcbba1',
                              '#fc9272',
                              '#fb6a4a',
                              '#ef3b2c',
                              '#cb181d',
                              '#a50f15'), 
                            domain = c(min(as.data.frame(state_data[, input$mapVal])),
                                       max(as.data.frame(state_data[, input$mapVal]))),
                            na.color = NA,
                            pretty = TRUE, 
                            bins = 6)
    }
    
    if (input$mapVal %in% percentages) {
      labels = sprintf("<strong>%s</strong><br/>%0.2f%%",
        str_to_title(lapply(strsplit(state_data[, "FullState"][["FullState"]], ":"), function (x) {x[[1]]})),
        100*state_data[, c("FullState", input$mapVal)][[input$mapVal]]
      ) %>% lapply(htmltools::HTML)
    } else if (input$mapVal %in% currencies) {
      labels = sprintf("<strong>%s</strong><br/>$%0.2f",
        str_to_title(lapply(strsplit(state_data[, "FullState"][["FullState"]], ":"), function (x) {x[[1]]})),
        state_data[, c("FullState", input$mapVal)][[input$mapVal]]
      ) %>% lapply(htmltools::HTML)
    }

    leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(data = state_map,
                  fillColor = ~mypalette(as.vector(state_data[, input$mapVal][[input$mapVal]])),
                  fillOpacity = 0.6,
                  color = "white",
                  weight = 1.5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                  ), 
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>% 
        addLegend(position = 'topleft',
                  pal = mypalette,
                  values = state_data[, input$mapVal],
                  opacity = 0.6,
                  title = input$mapVal,
                  labFormat = labelFormat(prefix=ifelse(input$mapVal %in% currencies, "$", ""),
                                          suffix=ifelse(input$mapVal %in% percentages, "%", ""),
                                          transform = function (x) if (input$mapVal %in% percentages) 100*x else x))
  })
  
  output$scatterPlot <- renderPlot({
    g <- ggplot(collegedata, aes_(as.name(input$scatterx),
                             as.name(input$scattery),
                             color=~`School Type`)) + geom_point() + xlab(input$scatterx) + ylab(input$scattery)
    g + scale_x_continuous(labels = ifelse(input$scatterx %in% percentages, scales::percent, scales::dollar)) +
      scale_y_continuous(labels = ifelse(input$scattery %in% percentages, scales::percent, scales::dollar))
  })
}

