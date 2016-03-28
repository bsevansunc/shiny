#---------------------------------------------------------------------------------*
# ---- TOOLTIPS ----
#---------------------------------------------------------------------------------*

# ---- Visit tooltips: ------------------------------------------------------------

ttRegion <- p(strong('Regional Hub:'), ' Select your study region from the list.')

ttSite <- p(strong('Site:'), ' Begin typing the site code and a list of options will appear. Either select the site from the list or press enter once you have finished entering the site data (i.e., only one choice in the dropdown menu). The site is the first four letters of the last name of the participant, the first three letters of their first name, the two-letter state abbreviation, and a number that signifies whether a participant has moved. For example, if a participant was named Robert Reitsma, they lived in Maryland, and this was the original Nestwatch site associated with the participant, the site would be REITROBMD1. If the participant moved to a new location, the site code for the new location would be REITROBMD2.')

ttVisitDate <- p(strong('Date:'), ' Select the date of your visit from the calendar. All dates are to be provided in the international date standard format ISO 8601 (YYYY-MM-DD).', em('Note: that if you visited a site on multiple dates, you would enter visit data for each visit separately.'))

ttVisitObserver <- p(strong('Observer initials:'), ' Please enter the two or three letter initials of EACH technician who visit a site, separating entries by a comma. For example, if Robert Reitsma and Brian S. Evans visited a site, they would enter RR, BSE in this field. Note: To avoid autocorrect woes, you may want to teach your initials to your browser.')

ttNetCount <- p(strong('Number of nets:'), ' This is the total number of nets you put out, regardless of whether a net was moved or a net was put up later in the day.Begin typing the number of net and a list of options will appear. You can either select the number of nets from the list or press enter once you have finished typing the value. Number of nets is provided per 6 m length (e.g., 6 m = 1, 12 m = 2 nets).')

ttNetHours <- p(strong('Net hours:'), 'You can either select net hours from the list or press enter once you have finished typing the value. To calculate net hours, make sure to keep track of how long each net was open during your visit. Multiply the length of time each net was open by the net length (see "Number of nets"), sum the nets, and record the number here. For example, if you had a 12 m open for three hours and two 6 m nets opened for 4 hours each, the net hours would be 2 nets x 3 hours + 2 nets x 4 hours = 14 net hours.')

ttStartEndResightTime <- p(strong('Start resight time, End resight time:'), 'These are the times in which you began and ended your targeted resight effort. All times are to be entered in the 24-hour clock (ISO 8601 time format), for example 1 pm would be provided as 13:00.', em('Note: targeted resighting does not include time spent at a site doing other activities (e.g., nest searching or banding).'))

ttPathDistance <- p(strong('Path distance traveled:'), ' If you had a GPS unit with you, this is the path distance that you recorded on your GPS unit, in meters.',
                    helpText('Note: To find out how to record path distance on a Garmin eTrex, please see',
                             a(href = "http://www8.garmin.com/learningcenter/on-the-trail/etrex/",
                               target="_blank", "this link.")))

helpText(
  "For more information on this tool please go to the following address: ",
  a(href="http://www.example.com", target="_blank", "Example")
)


ttObservedUnbanded <- p(strong('Observed, unbanded:'), 'As a measure of resight effort, we would like you to record the number of unbanded individuals observed during your', em('targeted'), 'resight foray. Only include counts of species that were previously color-banded at the current site.  For example, if no CARWs were ever color-banded at that site, do not count unbanded CARW during your foray. If you have encountered all individuals for a given species prior to targeted resighting, do not record counts for this species.')

ttEncounteredBirds <- p(strong('Did you band or encounter banded birds during your visit?'), 'We want you to keep track of your visits regardless of whether you encountered a banded bird during your visit. It sometimes happens that you are not successful in your efforts to band or re-encounter a bird during a visit. If this is the case, select', em('no.'))

ttVisitNotes <- p(strong('Visit notes:'), 'Have anything else to say about your visit (not about individual birds)? Put it here!')

# ---- Encounter tooltips: --------------------------------------------------------

ttId <- p(strong('Id:'), 'You do not need to enter any data into this field.')

ttBandTime <- p(strong('Time:'), ' This is the time (24-hour format) in which you began processing the bird.')

ttBanderInitials <- p(strong('Observer initials:'), ' Enter the initials of the technician or techicians (comma-separated) who measured or resighted the bird.')

ttEncounterType <- p(strong('Encounter type:'), ' Select the type of encounter from the list. If you resighted a bird during your targeted resight efforts, select ', em('Resight-targeted.'), ' If you resighted a bird outside of targeted resight efforts, select ', em('Resight-incidental.'), 'If a participant provided you with paper records of their resights, select', em('Resight-participant'), 'but make sure to use the date of the resight rather than your visit date!')

ttSpecies <- p(strong('Species:'), ' Drop-down menu choices only include Nestwatch focal species. Store all other encounter data locally.')

ttBandNumber <- p(strong('Band number:'), 'Separate band prefix and suffix with a "-". Do not include any spaces. If you did not add an aluminum band, type "NA". If you resighted a bird and were not able to identify the band in the query table (on the next tab), type "UNK".')

ttColorCombo <- p(strong('Color combo:'), ' Enter color combinations as L/L, R/R. Legs are separated with a comma and then a space. If a leg has no band, this is entered as "-" (a single dash). Color abbreviations must match those used on the website for Nestwatch participants.')

ttAgeThroughFat <- p(strong('Age-Fat:'), ' Please select values for age, sex, breeding condition, and fat from the provided lists, entering', em('UNK'), 'if you were unable to identify the field value.')

ttMassThroughTarsus <- p(strong('Mass-Tarsus:'), ' Enter the measurement data in each field (please error-check prior to submission!).')

ttFeatherID <- p(strong('Feather ID:'), ' Please provide the ID of the feather sample.')

ttBandNotes <- 'Any other observations to report on this bird? Provide it here!'

#---------------------------------------------------------------------------------*
# ---- PAGE TEXT ----
#---------------------------------------------------------------------------------*

textVisit <- p(strong('Start by entering your visit data.'), 'These data only need to be entered once for each site visit. After entering data into a field, press enter and tab to the next field. When you finish entering the data for each of the fields, make sure to double-check all of your entries for accuracy then press the', em('Submit visit data'),  'button.', strong('Do not enter banding, resight, point count, nest, or habitat survey data prior to completing and submitting this form!'))

textBandingIntro <- p(strong('AFTER entering visit data you are ready to enter your encounter records!'), ' This page is divided into two sections: ', br(), strong('1) Encounter record:'))

textBandingList1 <- tags$ul(
  tags$li('Enter one record for each individual, whether captured, recaptured, or resighted.'),
  tags$li('If you do not have data for a given field, leave that field blank.'),
  tags$li('After entering all available data press the ', em('Add record to table'),'button.'),
  tags$li('If you are unsure of the band number of a resighted bird use the', em('Query records'), 'form on the next tab to find the identity of the resighted bird.'),
  tags$li('If the available fields on the data entry form are not blank (with the exception of Id, Date, and Encounter Type), click the ',em('Clear fields'), 'button.'))
          
textBandingIntro2 <- p(strong('2) Data-proofing and submission of encounter records:'))

textBandingList2 <- tags$ul(
  tags$li('After entering all of the records from your visit, compare table values with your paper record.'),
  tags$li('If you find mistakes, select the record with your mouse -- you will notice that the fields above will be filled with that records entry and can simply be modified.'),
  tags$li('If you need to remove the entire record, simply select that record with your mouse and click', em('Delete record.')),
  tags$li('After you are confident of the quality of data provided, press the ', em('Submit encounter data'), 'button.'))

textQuery <- p('Query the table below to search for the band number associated with a given resight. While only 5 rows of data are shown, the table includes the initial record (band encounter) across all Neighborhood Nestwatch banding records and regional hubs. You can adjust the number of rows viewed using the "Show __ entries" drop-down button. You can sort the data table by column by clicking the column header. For example, to sort the data table by date, you would click on the "Date" column header. Query fields are at the bottom of each column. Use these fields to subset the data table. Data may be subset using partial matching. For example, typing "bu" in the Color combo field will match blue color bands on either leg or position. Likewise, typing "bu/bu" will query all records in which either leg has a blue over blue color band combo. You can use the search tool on the upper-right to query all fields simultaneously. Query fields are not case-sensitive.')

textPcIntro0 <- h3('Point count data are entered in three steps:')

textPcIntro1 <- tags$ol(
  tags$li(p(strong('Submit site-level point count data:'),'This section is where you enter your site data and the conditions at the site during your point count. You only need to enter these data once per point count. After filling in all of the fields, click the', em('Submit site-level point count data'), 'button.')),
  tags$li(p(strong('Add bird count records:'), 'This section is where you add records your actual counts of species. You will provide one record per observation. For example, if you saw 3 Northern Cardinal at a distance of 30 meters within a given time window, this would be a single observation. The table at the bottom of this page will display each record that you have added to the table.')),
  tags$li(p(strong('Submit bird count data:'), 'When you are done adding all of the records above, look through the table at the bottom of the page. Compare this table with your paper copy and modify any records as necessary. Once you are confident that your data are error free, click the', em('Submit point count data'), 'button'))
)

textAouQuery <- p(strong('Query, AOU codes:'), 'If you do not know the 4-letter AOU alpha code for a species that you observed during your point count, you can use this simple query table to find it. To search for a species, just type the common or scientific name in the search box. Search entries are not case-sensitive.')