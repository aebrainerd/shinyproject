library(shiny)
library(shinydashboard)

choices = c("Admission Rate (%)",
            "Average Family Income ($)",
            "Median Family Income ($)",
            "Default Rate (%)",
            "Low Income Repayment Rate (3yr %)",
            "Middle Income Repayment Rate (3yr %)",
            "High Income Repayment Rate (3yr %)",
            "Median Debt ($)")
shinyUI(
  dashboardPage(skin = "blue",
                dashboardHeader(title="College Scorecard"),
                dashboardSidebar(sidebarMenu(
                  menuItem("View Data", tabName="table", icon=icon("table")),
                  menuItem("Univariate Analysis", tabName="density", icon=icon("line-chart")),
                  menuItem("Bivariate Analysis", tabName="scatter", icon=icon("line-chart")),
                  menuItem("Geographical Analysis", tabName="map", icon=icon("map"))
                )),
                dashboardBody(tabItems(
                  tabItem(tabName = "table",
                          fluidRow(box(DT::dataTableOutput("table")))),
                  tabItem(tabName = "density",
                          selectizeInput(inputId = "densityVal",
                                         label = "Variable to plot:",
                                         choices = choices),
                          fluidRow(plotOutput("densityPlot"))),
                  tabItem(tabName = "map",
                          selectizeInput(inputId = "mapVal",
                                         label = "Varible to plot:",
                                         choices = choices),
                          fluidRow(leafletOutput("mapPlot"))),
                  tabItem(tabName = "scatter",
                          fluidRow(column(4, selectizeInput(inputId = "scatterx",
                                         label = "Variable on x-axis:",
                                         choices = choices)),
                                   column(4, selectizeInput(inputId = "scattery",
                                         label = "Variable on y-axis:",
                                         choices = choices))),
                          fluidRow(plotOutput('scatterPlot')))
                ))
  ))

