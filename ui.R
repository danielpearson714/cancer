# Create UI
fluidPage(theme = my_theme,
          titlePanel(title = div(img(src="appimage.png", height = 150, width = 550))),
          navbarPage("Catchment Area Research Dashboard",
            tabsetPanel(
                # Tab 1                        
                navbarMenu("Clinical Trials",
                    tabPanel(title = "Clinical Trials Enrollment at CINJ",
                             sidebarLayout(
                                sidebarPanel(width = 3,
                                 createPickerInput("disease_site", "Disease Site", site_list),
                                 createPickerInput("data4", "Data Table 4 Type", data4_list, selected = "Interventional"),
                                 createPickerInput("protocol", "Protocol Type", proto_list, selected = "Treatment"),
                                 createPickerInput("phase", "Phase", phase_list, selected = phase_list),
                                 createPickerInput("tsg", "Subject Tumor Study Group", tsg_list, selected = tsg_list)),
                                mainPanel(
                                    fluidRow(column(2, createPickerInput("trial_site", "Choose RWJBH Site", trial_list, selected = "CINJ")),
                                             column(1, checkboxGroupInput(inputId  = "year",
                                                                          label    = "Year",
                                                                          choices  = all_years,
                                                                          selected = all_years)),
                                             column(1, checkboxGroupInput(inputId  = "gender",
                                                                          label    = "Gender",
                                                                          choices  = all_sex,
                                                                          selected = all_sex)),
                                             column(2, checkboxGroupInput(inputId  = "race_ethnicity",
                                                                          label    = "Race/Ethnicity",
                                                                          choices  = all_races,
                                                                          selected = all_races))),
                                    fluidRow(column(9, plotOutput("accrual")),
                                             column(3, span(textOutput("n_value"), style = "font-size: 36px; color: darkred; font-style:bold, text-align: center")))
                                )))),
               # Tab 2
               navbarMenu("Biospecimens",
                tabPanel(title = "Total Samples", "Available Biospecimen Samples by Race/Ethnicity & Gender - Protocol 001006 (Total Patient Samples as of 11/24/2020)",
                         reactableOutput("brs")),
                tabPanel(title = "Unique Samples", "Available Biospecimen Samples by Race/Ethnicity & Gender - Protocol 001006 (Unique Samples as of 11/24/2020)",
                         reactableOutput("brs2"))),
              # Tab 3     
              navbarMenu("Cancer Incidence, Screening and Risk Factors",
                tabPanel(title = "Cancer and Risk Factors", fluid = TRUE,
                         fluidRow(column(3, varSelectInput(inputId = "x_axis",
                                                           label = "Choose x-axis",
                                                           data = risk,
                                                           selected = "Overall.Cancer.Incidence")),
                                  column(3, varSelectInput(inputId = "y_axis",
                                                  label = "Choose y-axis",
                                                  data = risk,
                                                  selected = "Overall.Cancer.Mortality"))),
                         mainPanel(
                             fluidRow(column(8, plotOutput("cancer_risk", width = "1000px", height = "800px")),
                                      column(4, reactableOutput("risk_react", width = "400px", height = "800px"))))
                ),
                tabPanel(title = "Top-12 Cancers (Radar Chart)",
                         createPickerInput("county_select", "Choose NJ county", county_list2, selected = "ATLANTIC"),
                         plotOutput("county_radar"))
                         
                ),
                # Tab 4                        
                navbarMenu("RWJBH Tumor Registries",
                    tabPanel(title = "Analytic Cases by Disease Site", fluid = TRUE,
                             fluidRow(column(4, selectInput(inputId = "rwj_site",
                                                            label = "Choose RWJBH Registry",
                                                            choices = rwj_list,
                                                            selected = "New Brunswick")),
                                      column(2, checkboxGroupInput(inputId = "report_year",
                                                          label = "Year",
                                                          choices = recent_years,
                                                          selected = recent_years[1])),
                                      column(6, sliderInput(inputId = "age_range",
                                                            label = "Select age range:",
                                                            min = 0,
                                                            max = 100,
                                                            value = c(0, 100))),
                             plotOutput("disease_site"))
                    ),
                    tabPanel(title = "Analytic Cases by Disease Site v2", fluid = TRUE,
                             fluidRow(column(3, createPickerInput("rwj_site2", "Choose RWJBH Registry", rwj_list, selected = "New Brunswick")),
                                      column(1, checkboxGroupInput(inputId  = "report_year2",
                                                                   label    = "Select Year(s)",
                                                                   choices  = recent_years,
                                                                   selected = recent_years)),
                                      column(2, checkboxGroupInput(inputId  = "gender2",
                                                                   label    =  "Select Gender(s)",
                                                                   choices  = all_sex,
                                                                   selected = all_sex)),
                                      column(2, checkboxGroupInput(inputId = "race",
                                                               label = "Select Race/Ethnicity",
                                                               choices = c("White" = "White",
                                                                           "Black" = "Black",
                                                                           "Asian" = "Asian",
                                                                           "Hispanic/Latino" = "Hispanic/Latino",
                                                                           "Other/Unknown" = "Other/Unknown"),
                                                               selected = c("White", "Black", "Asian", "Hispanic/Latino", "Other/Unknown"))),
                                      column(4, sliderInput(inputId = "age_range2",
                                                           label = "Select age range:",
                                                           min = 0,
                                                           max = 100,
                                                           value = c(0, 100)))),
                                 plotOutput("disease_site2", height = "100%", width = "100%"),
                                 downloadBttn("case_report", label = "Generate Report")
                    ),
                    tabPanel(title = "Age Distribution by Race/Ethnicity",
                            fluidRow(
                                column(6, createPickerInput("registry1", "Choose registry site", rwj_list, selected = rwj_list),
                                          createPickerInput("dis1", "Choose disease site", dis_list, selected = dis_list),
                                          plotOutput("boxplot1")),
                                column(6, createPickerInput("registry2", "Choose registry site", rwj_list, selected = rwj_list),
                                          createPickerInput("dis2", "Choose disease site", dis_list, selected = dis_list),
                                          plotOutput("boxplot2"))
                            )
                    )
                ),
                # Tab 5                        
                navbarMenu("Maps",
                    tabPanel(title = "County Map",
                             tmapOutput("countymap1", height = "600px", width = "75%")),
                    
                    tabPanel(title = "Air Pollutant Map",
                             tmapOutput("air_risk", height = "600px", width = "75%")),
                    
                    tabPanel(title = "County Map v2", "Cancer-Related Risk Factors by County",
                             varSelectInput(inputId  = "county_vars",
                                            label    = "Choose variable",
                                            data     = county_risk2 %>% select(2:24),
                                            selected = "Obese"),
                             leafletOutput("countymap2", height = "750px", width = "75%")),
                    
                    tabPanel(title = "Air Pollutant Map v2", "Estimated Cancer Risk per 1M Residents, by Air Toxin (2014 National Air Toxics Assessment)",
                             selectInput(inputId = "air_risk2",
                                         label = "Choose air pollutant",
                                         choices = c("X1.3.Butadiene" = "X1.3.Butadiene",
                                                     "Acetaldehyde" = "Acetaldehyde",
                                                     "Benzene" = "Benzene",
                                                     "Ethylene.Oxide" = "Ethylene.Oxide",
                                                     "Formaldehyde" = "Formaldehyde",
                                                     "Naphthalene" = "Naphthalene"),
                                         selected = "Acetaldehyde"),
                             leafletOutput("pollution", height = "750px", width = "75%"))
                    )
            ) # end tabsetPanel
        ) # end navbarPage
) # end fluidPage