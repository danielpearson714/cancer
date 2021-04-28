# Create UI
dashboardPage(skin = ("blue"),
        dashboardHeader(titleWidth = sidebar_width),
        dashboardSidebar(width = sidebar_width,
                         tags$p(),
                         tags$p(),
                         uiOutput("sidebarItems"),
                         tags$p(),
                         div(id = "appimage", img(src="appimage.png", height = 75))),
        dashboardBody(
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
            tags$head(tags$style(get_custom_html())),
             tags$script(HTML(paste('$(document).ready(function() { $("header").find("nav").append(\'<span class="bodyTitle">', app_title, '</span>\'); })'))),
            tabsetPanel(id = "tabset",
               # Tab 1                        
               navbarMenu("Clinical Trials",
                    tabPanel(title = "Clinical Trials Enrollment at CINJ",
                             value = "clinical_trials",
                             tags$p(),
                             fluidRow(column(2, createPickerInput("trial_site", "Choose RWJBH Site", trial_list, selected = "CINJ"),
                                             checkboxGroupInput(inputId  = "year",
                                                                  label    = "Year",
                                                                  choices  = all_years,
                                                                  selected = all_years),
                                             checkboxGroupInput(inputId  = "gender",
                                                                  label    = "Gender",
                                                                  choices  = all_sex,
                                                                  selected = all_sex),
                                             checkboxGroupInput(inputId  = "race_ethnicity",
                                                                  label    = "Race/Ethnicity",
                                                                  choices  = all_races,
                                                                  selected = all_races)),
                                      column(10, tags$p(), plotlyOutput("accrual", height = "700px")))
                    )
               ),
               # Tab 2
               navbarMenu("Biospecimens",
                tabPanel(title = "Total Samples", 
                         value = "total_samples",
                         tags$p(),
                         HTML("<h4><center>Available Biospecimen Samples by Race/Ethnicity & Gender - Protocol 001006 (Total Patient Samples as of 11/24/2020)</center></h4>"),
                         reactableOutput("brs", height = "700px")),
                tabPanel(title = "Unique Samples",
                         value = "unique_samples",
                         tags$p(),
                         HTML("<h4><center>Available Biospecimen Samples by Race/Ethnicity & Gender - Protocol 001006 (Unique Samples as of 11/24/2020)</center></h4>"),
                         reactableOutput("brs2", height = "700px"))),
              # Tab 3     
              navbarMenu("Cancer Incidence, Screening and Risk Factors",
                tabPanel(title = "Cancer and Risk Factors",
                         value = "cancer_risk_factors",
                         tags$p(),
                         fluidRow(column(9, tags$p(), tags$p(), plotOutput("cancer_risk", height = "700px")),
                                  column(3, reactableOutput("risk_react", height = "700px")))
                ),
                tabPanel(title = "Top-12 Cancers (Radar Chart)",
                         value = "top12_cancers",
                         tags$p(),
                         plotOutput("county_radar"))),
                # Tab 4                        
                navbarMenu("RWJBH Tumor Registries",
                    tabPanel(title = "Analytic Cases by Disease Site",
                             value = "analytic_cases",
                             tags$p(),
                             plotOutput("disease_site", height = "700px")),
                    tabPanel(title = "Analytic Cases by Disease Site v2",
                             value = "analytic_cases_v2",
                             plotOutput("disease_site2", height = "700px")
                    ),
                    tabPanel(title = "Age Distribution by Race/Ethnicity",
                             value = "age_distribution",
                             fluidRow(column(6, plotOutput("boxplot1", height = "700px")),
                                      column(6, plotOutput("boxplot2", height = "700px"))))
                ),
                # Tab 5                        
                navbarMenu("Maps",
                    tabPanel(title = "County Map",
                             value = "county_map",
                             tmapOutput("countymap1", height = "700px")),
                    
                    tabPanel(title = "Air Pollutant Map",
                             value = "air_pollutant_map",
                             tmapOutput("air_risk", height = "700px")),
                    
                    tabPanel(title = "County Map v2", 
                             value = "county_map_v2",
                             "Cancer-Related Risk Factors by County",
                             leafletOutput("countymap2", height = "700px")),
                    
                    tabPanel(title = "Air Pollutant Map v2", 
                             value = "air_pollutant_map_v2",
                             "Estimated Cancer Risk per 1M Residents, by Air Toxin (2014 National Air Toxics Assessment)",
                             leafletOutput("pollution", height = "700px"))
                    )
            ) # end tabsetPanel
        ), # end dashboardBody
        title = app_title
)#end dashboardPage