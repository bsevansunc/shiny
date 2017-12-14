#========================================================================================*
# ---- SET-UP ----
#========================================================================================*

library(shiny)
library(shinythemes)
library(tidyr)
library(markdown)
library(dplyr)
library(readr)
library(DT)
library(stringr)
library(leaflet)
library(sp)

# No strings as factors!

options(stringsAsFactors = FALSE)


select <- dplyr::select
filter <- dplyr::filter

# Get vector of species names:

choiceSpecies <- c('Show all',
                   'Black-crowned night heron',
                   'Brown pelican',
                   'Black-bellied plover',
                   'Long-billed curlew',
                   'Pacific loon',
                   "Swainson's hawk")


# This function makes a fluid row with white space on either side:

centeredRow <- function(content){
  fluidRow(
    column(1, ''),
    column(10, content),
    column(1, '')
  )
}

#========================================================================================*
# ---- UI ----
#========================================================================================*

shinyUI(
  navbarPage(
    title = 'Follow that bird!',
    theme = shinytheme('yeti'),
    #--------------------------------------------------------------------------------------*
    # --- HOME PAGE ----
    #--------------------------------------------------------------------------------------*
    tabPanel("Home"),
    #--------------------------------------------------------------------------------------*
    # --- THE FULL ANNUAL CYCLE  ----
    #--------------------------------------------------------------------------------------*
    tabPanel("The full annual cycle"),
    #--------------------------------------------------------------------------------------*
    # --- BIRD PROFILES  ----
    #--------------------------------------------------------------------------------------*
    navbarMenu('Bird profiles'),
    #--------------------------------------------------------------------------------------*
    # --- TRACKING DEVICES ----
    #--------------------------------------------------------------------------------------*
    navbarMenu("Tracking devices"),
    #--------------------------------------------------------------------------------------*
    # --- TRACKING DATA ----
    #--------------------------------------------------------------------------------------*
    navbarMenu("Tracking data",
               tabPanel("Tracking table",
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(
                              column(11, selectizeInput('sppTable', 'Bird species:',
                                                        choices = choiceSpecies)),
                              column(1, '')),
                            fluidRow(
                              column(11, uiOutput('uiBirdIDTable')),
                              column(1, '')),
                            fluidRow(
                              column(11, uiOutput('uiDateRangeTable')),
                              column(1, '')),
                            fluidRow(uiOutput('birdPicTable')),
                            width = 3, position = 'left'),
                          mainPanel(
                            fluidRow(
                              centeredRow(DT::dataTableOutput('tableQuery'))
                            ),
                            width = 9, position = 'right')
                        )),
               tabPanel("Tracking map",
                        br(),
                        fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              tags$style(type="text/css",
                                         ".selectize-input { font-size: 14px; line-height: 18px;} "),
                              h3('Mapping bird tracks'),
                              br(),
                              fluidRow(
                                column(11, 'To view tracking data, select the bird species and identity of bird you would like to look at and then click the "Map it!" button'),
                                column(1, '')
                              ),
                              hr(),
                              fluidRow(
                                column(11, selectizeInput('sppMap', 'Bird species:',
                                                          choices = choiceSpecies)),
                                column(1, '')),
                              fluidRow(
                                column(11, uiOutput('uiBirdIDMap')),
                                column(1, '')),
                              fluidRow(
                                column(11, actionButton('mapIt', 'Map it!'))
                              ),
                              width = 4, position = 'left'),
                            mainPanel(
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              fluidRow(
                                centeredRow(leafletOutput("mapOut", width = "90%", height=580))
                              ),
                              width = 8, position = 'right')
                          )
                        )
               )
    )
  )
)