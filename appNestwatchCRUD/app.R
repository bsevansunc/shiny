library(markdown)

shinyApp(
  ui = navbarPage("Neighborhood Nestwatch technician data submission interface",
    tabPanel('1. Visit data (start here):',
             sidebarLayout(
    sidebarPanel(div(
      id = "form",
      h4(strong('1. Visit data')),
      # p('Please select your study region from the drop menu.'),
      # actionLink("do", "?"),
      fluidRow(column(4,
      # div(style="display:inline-block",
          selectInput('studyRegion','Hub:',
                  c('Atlanta' = 'Atlanta',
                    'Gainesville' = 'Gainesville',
                    'Pittsburgh' = 'Pittsburgh',
                    'Raleigh' = 'Raleigh',
                    'Springfield' = 'Springfield',
                    'DC' = 'DC'))),
      #textInput("site", "Site:", ""),
      # div(style="display:inline-block",
          column(8,
          selectizeInput('site', 'Site:', #'e2', '2. Multi-select', 
                         choices = c('REITBOBMD1', 'MARRPETMD1', 'BARNDORMD1', 'MARRPETMD2'),
                         multiple = FALSE )
#           selectInput('site','Site',
#                       c('REITBOBMD1' = 'Atlanta',
#                         'MARRPETMD1' = 'Gainesville',
#                         'BARNDORMD1' = 'Pittsburgh',
#                         'PANCSAMMD1' = 'Raleigh',
#                         'TEST' = 'Springfield',
#                         'TEST1' = 'DC'))
      )),
#       div(style="display:inline-block",
#           textInput(inputId="site", label="Site", value = NA)),
#       div(class="row-fluid",
#           div(class="span1",textInput("xlimitsmin", label = "x-min", value = 0.0)), 
#           div(class="span1",textInput("xlimitsmax", label = "x-max", value = 0.5))
#       ),
#       div(style="display:inline-block",
#           dateInput('date', label = 'Date:', value = Sys.Date())),
#       div(style="display:inline-block",
#         textInput("observer", "Observer(s):")),
      fluidRow(
        column(6,
               dateInput('date', label = 'Date:', value = Sys.Date())),
        column(6,
               textInput("observer", "Observer(s):"))),
      h4(strong('2. Banding effort')),
      fluidRow(
        column(6, selectizeInput('startNetTime', 'Nets opened:', 
                                 choices = format(seq(ISOdate(2000, 1, 1), 
                                                      ISOdate(2000,1,2), by = 'min'), '%H:%M') %>%
                                   unique %>% sort, selected = '07:00', multiple = F)), 
        column(6, selectizeInput('endNetTime', 'Nets closed:', 
                                 choices = format(seq(ISOdate(2000, 1, 1), 
                                                      ISOdate(2000,1,2), by = 'min'), '%H:%M') %>%
                                   unique %>% sort, selected = '11:30', multiple = F))),
      fluidRow(
        column(6, selectizeInput('netCount', 'Number of nets:',
                                 choices = seq(0, 12, by = 0.5), selected = 3.5, multiple = F)),
        
        column(6, selectizeInput('netHours', 'Net hours:',
                              choices = seq(0, 24, by = 0.01), selected = 15.75, multiple = F))),
      h4(strong('3. Resight effort')),
      fluidRow(
        column(6, selectizeInput('startRsTime', 'Started resight:', 
                                 choices = format(seq(ISOdate(2000, 1, 1), 
                                                      ISOdate(2000,1,2), by = 'min'), '%H:%M') %>%
                                   unique %>% sort, selected = '11:30', multiple = F)), 
        column(6, selectizeInput('endRsTime', 'Ended resight:', 
                                 choices = format(seq(ISOdate(2000, 1, 1), 
                                                      ISOdate(2000,1,2), by = 'min'), '%H:%M') %>%
                                   unique %>% sort, selected = '12:30', multiple = F))),
      textInput('distance', 'Distance travelled (path distance in meters):'),
      actionButton("submit", "Submit", class = "btn-primary")
    ), width = 3),
  mainPanel('fill'))),
  tabPanel('2. Banding data'),
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