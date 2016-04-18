library(shiny)
library(dplyr)
library(tidyr)
library(DT)

encounters <- read.csv('encounters2.csv',
                       stringsAsFactors = FALSE) %>%
  tbl_df %>%
  filter(encounterType == 'band') %>%
  select(hub:bandCombo, bandYear) %>%
  distinct

hubChoices <- encounters %>%
  select(hub) %>%
  distinct %>%
  arrange(hub) %>%
  .$hub

shinyApp(
  ui = fluidPage(
    br(),
    fluidRow(
      column(1, ''),
      column(11, h2('Neighborhood Nestwatch resight data sheet'))
      ),
    hr(),
    fluidRow(
      column(1, ''),
      column(3, selectInput('hubForm', 'Regional hub:', hubChoices)),
      column(3, selectInput('siteForm', 'site', ' ')),
      column(5, '')
    ),
    hr(),
    br(),
    fluidRow(
      column(1, ''),
      column(5, p(strong('Visit Date (yyyy-mm-dd):', ' _________________________'))),
      column(1, ''),
      column(5, p(strong('Observer(s) initials:', ' _________________________')))
    ),
    # DT::dataTableOuput('t1'),
    hr(),
    fluidRow(
      column(1, ''),
      column(3, h3('Banded birds summary:')),
      column(8, '')
    ),
    fluidRow(
      column(1, ''),
      column(6, DT::dataTableOutput('t1')),
      column(5, imageOutput('birdBranch', height = 55))
    ),
    hr(),
    fluidRow(
      column(1, ''),
      column(11, h3('Detailed banding history:'))
    ),
    fluidRow(
      column(1, ''),
      column(7, DT::dataTableOutput('tbl')),
      column(4, '')
    )
  ),
  
  server = function(input, output, session) {
    
    # Subset sites shown in ui by hub:
    
    observe({
      siteNames <- encounters %>%
        select(hub, site) %>%
        distinct %>% 
        filter(hub == input$hubForm) %>%
        arrange(site) %>%
        .$site
      updateSelectInput(session, 'siteForm', choices = siteNames)
    })
    
    # Subset encounters data frame by hub and site:
    
    encountersSubset <- reactive(
      encounters %>%
        filter(hub == input$hubForm,
               site == input$siteForm) %>%
        arrange(species, bandYear, bandCombo) %>%
        select(species:bandCombo, bandYear)
    )
    
    # Summary table:
    
    summaryTable <- reactive(
      encountersSubset() %>%
        select(species, bandNumber) %>%
        group_by(species) %>%
        summarize(Banded = length(bandNumber))
    )
    
    output$t1 = DT::renderDataTable(
      DT::datatable(summaryTable() %>%
                      mutate(observedUnbanded = ' '),
                    class = 'cell-border stripe',
                    options = list(dom = 't',
                                 pageLength = nrow(summaryTable()),
                                 columnDefs = list(
                                   list(
                                     width = '.5in', targets = '_all'
                                   ),
                                   list(
                                     className = 'dt-center', targets = '_all'
                                   )
                                 )
                                 ),
                                 rownames = FALSE
                  )
    )
    
    output$birdBranch <- renderImage({
      list(src = 'nnBirdBranchImage.png',
           width = 300,
           height = 116, 
           position = 'middle')
    }, deleteFile = FALSE)

    
    output$tbl = DT::renderDataTable(
      DT::datatable(encountersSubset() %>%
                      mutate(R = ' ',
                             Longitude = ' ',
                             Latitude = ' ',
                             Accuracy = ' '),
                    class = 'table-bordered', # 'cell-border stripe',
                    rownames = FALSE,
                    options = list(dom = 't',
                                   pageLength = nrow(encountersSubset()),
                                   autoWidth = TRUE,
                                   columnDefs = list(
                                     list(
                                       width = '.5in', targets = '_all'
                                       ),
#                                      list(
#                                        width = '3.5in', targets = c(2)
#                                      ),
                                     list(
                                       className = 'dt-center', targets = '_all'
                                       )
                                   )
                                   ))
    )
  }
)


