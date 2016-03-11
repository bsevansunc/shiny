library(markdown)

# Choices: 

timeOfDay <- format(seq(ISOdate(2000, 1, 1), ISOdate(2000,1,2), 
                        by = 'min'), '%H:%M') %>% unique %>% sort

spp <- c('AMRO', 'BCCH', 'BRTH', 'CARW', 'GRCA', 'HOWR', 'NOCA','NOMO','SOSP', 'UNCH')

shinyApp(
  ui = navbarPage("Neighborhood Nestwatch technician data submission interface",
    tabPanel('1. Visit data:',
             sidebarLayout(
                sidebarPanel(
                  h4(strong('1. Visit data')),
                  fluidRow(
                    column(4,
                      selectInput('studyRegion','Hub:',
                              c('Atlanta' = 'Atlanta',
                                'Gainesville' = 'Gainesville',
                                'Pittsburgh' = 'Pittsburgh',
                                'Raleigh' = 'Raleigh',
                                'Springfield' = 'Springfield'))),
                      column(8,
                      selectizeInput('site', 'Site:', #'e2', '2. Multi-select', 
                                     choices = c('REITBOBMD1', 'MARRPETMD1', 'BARNDORMD1', 'MARRPETMD2'),
                                     multiple = FALSE ))),
                  fluidRow(
                    column(6,
                           dateInput('date', label = 'Date:', value = Sys.Date())),
                    column(6,
                           textInput("observer", "Observer initials:"))),
                  h4(strong('2. Banding effort')),
                  fluidRow(
                    column(6, selectizeInput('startNetTime', 'Nets opened:', 
                                             choices = timeOfDay,
                                             selected = '07:00', multiple = F)), 
                    column(6, selectizeInput('endNetTime', 'Nets closed:', 
                                             choices = timeOfDay, 
                                             selected = '11:30', multiple = F))),
                  fluidRow(
                    column(6, selectizeInput('netCount', 'Number of nets:',
                                             choices = seq(0, 12, by = 0.5), 
                                             selected = 3.5, multiple = F)),
                    column(6, selectizeInput('netHours', 'Net hours:',
                                          choices = seq(0, 24, by = 0.01), 
                                          selected = 15.75, multiple = F))),
                  h4(strong('3. Resight effort')),
                  fluidRow(
                    column(6, selectizeInput('startRsTime', 'Started resight:', 
                                             choices = timeOfDay,
                                             selected = '11:30', multiple = F)), 
                    column(6, selectizeInput('endRsTime', 'Ended resight:', 
                                             choices = timeOfDay,
                                             selected = '12:30', multiple = F))),
                  fluidRow(
                    column(6, textInput('distance', 'Distance travelled (path distance in meters):'))),
                    br(),
                  actionButton("submit", "Submit", class = "btn-primary"),
                  width = 6),
              mainPanel(
                fluidRow(column(2, ''),
                         column(10, fluidRow(h2(strong('Start by entering visit data'))))),
                br(),
                fluidRow(column(2, ''),
                         column(10, p('test'))),
                width = 6),
              position = 'right')
             ),
    tabPanel('2. Banding data',
             sidebarLayout(
               sidebarPanel(
                 fluidRow(column(3, selectizeInput('bandTime', 'Time:', 
                                                   choices = timeOfDay,
                                                   selected = '07:30', multiple = F)),
                          column(3, textInput('bander', 'Bander initials:'))),
                 fluidRow(column(3, selectizeInput('species', 'Species:',
                                                   choices = spp, selected = 'AMRO', multiple = F))),
                 actionButton('update', 'Submit banding data'),
                 width = 9),
               mainPanel(
                 fluidRow(column(2, ''),
                          column(10, fluidRow(h2(strong('Banding data'))))),
                 br(),
                 fluidRow(column(2, ''),
                          column(10, p('test'))),
                 width = 3),
               position = 'right'
             )),
    tabPanel('3. Resight data'),
    tabPanel('4. Point count data'),
    tabPanel('5. Nest data'),
    tabPanel('6. Habitat survey data')),
    server = function(input, output, session) {
      observe({
        session$sendCustomMessage(type = 'testmessage',
                                  message = list(a = 1, b = 'text',
                                                 controller = input$controller))
      })
            #     observe({
            #       
            #       infile <- input$datfile
            #       
            #       print(infile)
            #       if(is.null(infile))
            #         return(NULL)
            #       
            #       d <- read.csv(infile$datapath, header = T)
            #       
            #       updateSelectInput(session, 'dropdown_1', choices = names(d))
            #       updateSelectInput(session, 'dropdown_2', choices = names(d))
            #       
            #     })
              }
            )