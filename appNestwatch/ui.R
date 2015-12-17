library(shiny)

shinyUI(fluidPage(
  titlePanel ('Neighborhood Nestwatch Site Selection Tool'),
  sidebarLayout(position = 'left',
                sidebarPanel(h3(strong('Site selection dashboard')),
                             # Define study region:
                             h4(strong('1. Select Study Region:')),
                             p('Please select your study region from the drop menu.'),
                             selectInput('studyRegion','',
                                         c('Atlanta, GA' = 'atlanta',
                                           'Gainesville, FL' = 'gainesville',
                                           'Pittsburgh, PA' = 'pittsburgh',
                                           'Raleigh, NC' = 'raleigh',
                                           'Springfield, MA' = 'springfield',
                                           'Washington, DC' = 'dc')),
                             # Data upload, exhisting sites:
                             h4(strong('2. Upload existing site data:')),
                             p('Important! The input file must be a three-column csv file of exiting 
                               Neighborhood Nestwatch sites with the fields "siteID","longitude" and 
                               "latitude" provided in that order.'),
                             fileInput('existingSites','',
                                       accept = c('test/csv', 'text/comma-separated-values,text/plain','.csv')),
                             # Data upload, potential sites:
                             h4(strong('3. Upload data for prospective sites:')),
                             p('Important! The input file must be a three-column csv file of exiting 
                               Neighborhood Nestwatch sites with the fields "siteID","longitude" and
                               "latitude" provided in that order.'),
          fileInput('potentialSites','',
            accept = c('test/csv', 'text/comma-separated-values,text/plain','.csv')),
      # Define resolution Resolution:
        h4(strong('4. Provide the target resolution:')),
          p('This is the distance in meters from each household center for which 
            you will calculate the proportion of impervious surface'),
          numericInput('radius','',500, 50, 5000, step = 50),
      # Submit data for analysis:
        h4(strong('5. Select your sites!')),
        submitButton('Submit!')),
#         br(),
#       # Download output:
#         h4(strong('6. Download output')),
#           p('Use the button below if you would like to download a csv file of the 
#             proportion of impervious surface surrounding existing and potential sites.'),
#           downloadButton('downloadTabular', 'Download Tabular Output')),
# Graphical summary:
mainPanel(
  tabsetPanel(
    tabPanel('Map Viewer',
             fluidRow(column(9,leafletOutput('map')),
                      column(3, 
#                              checkboxGroupInput('mapLayers',
#                                      h4('Show Layers:'),
#                                      c('Existing Sites' = 'existing',
#                                     'Potential Sites' = 'potential',
#                                     'Impervious Surface' = 'imp'))
                             fluidRow(h3(strong('Show Layers:')), hr(),
                                         checkboxInput('show','Existing Sites2', FALSE),
                                         checkboxInput('showPotential','Potential Sites', FALSE),
                                         checkboxInput('showImp','Impervious Surface', FALSE))
#                                                 h4('Show Layers:'),
#                                                 c('Existing Sites' = 'existing',
#                                                   'Potential Sites' = 'potential',
#                                                   'Impervious Surface' = 'imp'))
                        )),
             p(), hr(),
             fluidRow(
               column(3,downloadButton('downloadXYData','Download LatLong')),
               column(3,downloadButton('downloadKML','Download KML')),
               column(3,downloadButton('downloadKMZ','Download KMZ'))
             )
    ),
    tabPanel('Summary Plots',plotOutput('plot')),
    tabPanel('Data Table', tableOutput('table'))
)
)
)))