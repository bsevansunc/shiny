#---------------------------------------------------------------------------------*
# ---- PAGE TEXT, VISIT ----
#---------------------------------------------------------------------------------*

textVisit <- p(
  h3('Start by entering your visit data:'), 
  p('These data only need to be entered once for each site visit. After entering data into a field, press enter and tab to the next field. When you finish entering the data for each of the fields, make sure to double-check all of your entries for accuracy then press the', em('Submit visit data'),  'button.', strong('Do not enter banding, resight, point count, nest, or habitat survey data prior to completing and submitting this form!')),
  hr(),
  p(strong('Regional Hub:'),
  'Select your study region from the list.'),
  br(),
  p(strong('Site:'), 
    'Begin typing the site code and a list of options will appear. Either select the site from the list or press enter once you have finished entering the site data (i.e., only one choice in the dropdown menu). The site is the first four letters of the last name of the participant, the first three letters of their first name, the two-letter state abbreviation, and a number that signifies whether a participant has moved. For example, if a participant was named Robert Reitsma, they lived in Maryland, and this was the original Nestwatch site associated with the participant, the site would be REITROBMD1. If the participant moved to a new location, the site code for the new location would be REITROBMD2.'),
  br(),
  p(strong('Date:'), 
    'Select the date of your visit from the calendar. All dates are to be provided in the international date standard format ISO 8601 (YYYY-MM-DD).', em('Note: that if you visited a site on multiple dates, you would enter visit data for each visit separately.')),
  br(),
  p(strong('Observer initials:'), ' Please enter the three-letter initials of EACH technician who visit a site, separating entries by a comma. For example, if Thomas Brandt Ryder and Brian S. Evans visited a site, they would enter "TBR,BSE" in this field. If the technician does not have a middle name, you may enter their two-letter initials. Please be sure to use the same initials across all of your visits!  Note: To avoid autocorrect woes, you may want to turn autocorrect off on your computer operating system or in your web application (See:',
                       tags$a(href = "https://support.apple.com/kb/PH18451?locale=en_US",
                              "How to disable autocorrect in Mac Yosemite"),
                       ',',
                       tags$a(href = "http://www.windowscentral.com/how-disable-spell-checker-windows-10", 
                              'How to disable autocorrect in Windows 10'),
                       ')'),
  br(),
  p(strong('Location:'), 'Only record location data if you took a new location reading with a GPS unit (e.g., moved site center or new site). When recording your location, be sure that your GPS unit is set to WGS 1984.'),
  br(),
  p(strong('Number of X m nets:'), 'This is the total number of nets you put out of a given size, regardless of whether a net was moved or a net was put up later in the day.'),
  br(),
  p(strong('Total time open (minutes), X m nets:'), 'This is the total time, in minutes, in which nets of a given size class were open. For example, if you had three 6 m nets open for a period of two hours each, the total time in minutes would be  3 nets x (60 minutes per hour x 2 hours) = 360 minutes.'),
  br(),
  p(strong('Start resight time, End resight time:'), 'These are the times in which you began and ended your targeted resight effort. All times are to be entered in the 24-hour clock (ISO 8601 time format), for example 1 pm would be provided as 13:00.', strong(em('Note: targeted resighting does not include time spent at a site doing other activities (e.g., nest searching or banding).'))),
  br(),
  p(strong('Path distance traveled:'), ' If you had a GPS unit with you, this is the path distance that you recorded on your GPS unit, in meters.', em('Note: To find out how to record path distance on a Garmin eTrex, please see', a(href = "http://www8.garmin.com/learningcenter/on-the-trail/etrex/", "this link."))),
  br(),
  p(strong('Observed, unbanded:'), 'As a measure of resight effort, we would like you to record the number of unbanded individuals observed during your', em('targeted'), 'resight foray. Only include counts of species that were previously color-banded at the current site.  For example, if no CARWs were ever color-banded at that site, do not count unbanded CARW during your foray. If you have encountered all individuals for a given species prior to targeted resighting, do not record counts for this species.'),
  br(),
  p(strong('Did you band or encounter banded birds during your visit?'), 'We want you to keep track of your visits regardless of whether you encountered a banded bird during your visit. It sometimes happens that you are not successful in your efforts to band or re-encounter a bird during a visit. If this is the case, select', em('no.')),
  br(),
  p(strong('Visit notes:'), 'Have anything else to say about your visit (not about individual birds)? Put it here!')
  )

#---------------------------------------------------------------------------------*
# ---- PAGE TEXT, ENCOUNTER ----
#---------------------------------------------------------------------------------*

# Introductory text for encounter page:

introTextEncounter <- p(
  h3('Encounter data are entered in two steps:'),
  tags$ol(
    tags$li(strong('Enter encounter record:'),
            tags$ul(
              tags$li('Enter one record for each individual, whether captured, recaptured, or resighted.'),
              tags$li('If you do not have data for a given field, leave that field blank.'),
              tags$li('After entering all available data press the ', em('Add record to table'),'button.'),
              tags$li('If you are unsure of the band number of a resighted bird use the', em('Query records'), 'form on the next tab to find the identity of the resighted bird.'),
              tags$li('If the available fields on the data entry form are not blank (with the exception of Id, Date, and Encounter Type), click the ',em('Clear fields'), 'button.'))),
    tags$li(strong('Data-proofing and submission of encounter records:'),
            tags$ul(
              tags$li('After entering all of the records from your visit, compare table values with your paper record.'),
              tags$li('If you find mistakes, select the record with your mouse -- you will notice that the fields above will be filled with that records entry and can simply be modified.'),
              tags$li('If you need to remove the entire record, simply select that record with your mouse and click', em('Delete record.')),
              tags$li('After you are confident of the quality of data provided, press the ', em('Submit encounter data'), 'button.'))
            )))

# Field option descriptions for encounter page:

fieldDescriptionsEncounter <- p(
  p(strong('Id:'), 'You do not need to enter any data into this field.'),
  br(),
  p(strong('Time:'), ' This is the time (24-hour format) in which you began processing the bird.'),
  br(),
  p(strong('Observer initials:'), ' Enter the initials of the technician or techicians (comma-separated) who measured or resighted the bird.'),
  br(),
  p(strong('Encounter type:'), ' Select the type of encounter from the list. If you resighted a bird during your targeted resight efforts, select ', em('Resight-targeted.'), ' If you resighted a bird outside of targeted resight efforts, select ', em('Resight-incidental.'), 'If a participant provided you with paper records of their resights, select', em('Resight-participant'), 'but make sure to use the date of the resight rather than your visit date!'),
  br(),
  p(strong('Species:'), ' Drop-down menu choices only include Nestwatch focal species. Store all other encounter data locally.'),
  br(),
  p(strong('Band number:'), 'Separate band prefix (first 3 or 4 digits, always before the dash) and suffix (last 5 digits, always after the dash) with a "-". Do not include any spaces. If you did not add an aluminum band, type "NA". If you resighted a bird and were not able to identify the band in the query table (on the next tab), type "UNK".'),
  br(),
  p(strong('Color combo:'), ' Enter color combinations as L/L,R/R. Do not include any spaces. If a leg has no band, this is entered as "-" (a single dash). Color abbreviations must match those used on the website for Nestwatch participants.'),
  br(),
  p(strong('Age-Fat:'), ' Please select values for age, sex, breeding condition, and fat from the provided lists, entering', em('UNK'), 'if you were unable to identify the field value.'),
  br(),
  p(strong('Mass-Tarsus:'), ' Enter the measurement data in each field (please error-check prior to submission!).'),
  br(),
  p(strong('ID (Feather, Toenail, Blood, Fecal, Attachment):'), 'Please provide a unique identifier for your sample here.'),
  br(),
  p(strong('Notes:'), 'Any other observations to report on this bird? Provide it here!')
)

#---------------------------------------------------------------------------------*
# ---- PAGE TEXT, QUERY ----
#---------------------------------------------------------------------------------*
          
textQuery <- p('Query the table below to search for the band number associated with a given resight. While only 5 rows of data are shown, the table includes the initial record (band encounter) across all Neighborhood Nestwatch banding records and regional hubs. You can adjust the number of rows viewed using the "Show __ entries" drop-down button. You can sort the data table by column by clicking the column header. For example, to sort the data table by date, you would click on the "Date" column header. Query fields are at the bottom of each column. Use these fields to subset the data table. Data may be subset using partial matching. For example, typing "bu" in the Color combo field will match blue color bands on either leg or position. Likewise, typing "bu/bu" will query all records in which either leg has a blue over blue color band combo. You can use the search tool on the upper-right to query all fields simultaneously. Query fields are not case-sensitive.')

#---------------------------------------------------------------------------------*
# ---- PAGE TEXT, POINT COUNT ----
#---------------------------------------------------------------------------------*

introTextPc <- p(
  h3('Point count data are entered in two steps:'),
  tags$ol(
  tags$li(strong('Enter point count record:'),
          tags$ul(
            tags$li('Fields Site-Point count notes only need to be entered once. You will notice that Site, Observer, and Date have already been filled by your entries in the Visit data panel. If these fields are blank, you may have forgotten to add visit data!'),
            tags$li('Please provide one record per observation. For example, if you saw 3 Northern Cardinal at a distance of 30 meters within a given time window, this would be a single observation. The table at the bottom of this page will display each record that you have added to the table. If you detected one more NOCA in that same time period 40m away, that would be another observation. If you visually detected 2 NOCAs but at different distance intervals at the same time, these would be two separate observations. If you detected two NOCAs, one visually and one by ear at the same time in the same distance interval, this would also be two separate observations.'),
            tags$li('After entering all available data across fields press the ', em('Add record to table'),'button.')
            )),
  tags$li(strong('Data-proofing and submission of point count records:'),
          tags$ul(
            tags$li('After entering all of the point count records from your visit, compare table values with your paper record.'),
            tags$li('If you find mistakes, select the record with your mouse -- you will notice that the fields above will be filled with that records entry and can simply be modified.'),
            tags$li('If you need to remove the entire record, simply select that record with your mouse and click', em('Delete record.')),
            tags$li('After you are confident of the quality of data provided, press the ', em('Submit encounter data'), 'button.')) 
  ))
)

fieldDescriptionsPc <- p(
  p(strong('Observer:'), 'If more than one person conducted the count formally (for example, if counts were conducted using a multiple observer protocol), please enter separate site-level point count records for each observer.'),
  br(),
  p(strong('Start time:'), 'The time you began your point count (24-hour clock).'),
  br(),
#   strong('Location:'), 'Only record location data if you took a new location reading with a GPS unit (e.g., moved site center or new site). When recording your location, be sure that your GPS unit is set to WGS 1984.',
#   br(),
#   strong('Temperature:'), 'The temperature in degrees Celsius during your point count. If you do not bring a thermometer with you on your visit, these data are readily available using weather Apps on your phone or after the fact using the website Weather Underground.', tags$a(href="www.wunderground.com", 'Click here to go to Weather Underground.'),
#   br(),
#   strong('Sky (0-5):'), 'The sky index gives us a general measure of the weather conditions. Entries are:',
#   tags$ul(
#     tags$li('0: Clear (barely a cloud in the sky!)'),
#     tags$li('1: Partly cloudy/partly sunny (some clouds, but predominantly sunny)'),
#     tags$li('2: Mostly cloudy (the sky is more than 50 % covered with clouds)'),
#     tags$li('3: Cloudy (no sun to be seen)'),
#     tags$li('4: Light rain'),
#     tags$li('5: Rain')
#   ),
#   br(),
#   strong('Wind (Beaufort scale)'), 'The Beaufort scale is a universal wind scale. While the scale goes up to 12, only values 0-6 apply to Nestwatch visits. To learn about the Beaufort scale, ', tags$a(href = 'http://www.spc.noaa.gov/faq/tornado/beaufort.html', 'click here.'),
#   br(),
#   strong('Ambient noise (dB):'), 'Only record ambient noise if you have an SPL (sound pressure level) meter.',
#   br(),
  p(strong('Point count, time:'), 'This is the time segment of the point count, in minutes. For example, if you broke a 10 minute point count into 3, 2, and 5 minute segments, all observations in the first time interval would be recorded as 3.'),
  br(),
  p(strong('Species:'), 'Type your species into the field (do not try to scroll through all of the entries ... this is all of the birds in N. America!). If you do not know the 4-letter alpha code, search the query table below.'),
  br(),
  p(strong('Distance:'), 'This is the distance interval, in meters, in which a bird was observed.')
)

textAouQuery <- p(strong('Query, AOU codes:'), 'If you do not know the 4-letter AOU alpha code for a species that you observed during your point count, you can use this simple query table to find it. To search for a species, just type the common or scientific name in the search box. Search entries are not case-sensitive.')