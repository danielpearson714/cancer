# Create UI

theme <- create_theme(
        adminlte_color(
                light_blue = "#1D5F88"),
        adminlte_sidebar(
                dark_bg = "#7F7F7F",
                dark_color = "black"
        ),
        bs4dash_layout(
                sidebar_width = sidebar_width
        )
)

dashboardPage(
        skin = "blue",
        freshTheme = theme,
        dashboardHeader(titleWidth = sidebar_width),
        dashboardSidebar(width = sidebar_width,
                         minified = FALSE,
                         tags$div(class = "menutopspacing"),
                         uiOutput("sidebarItems"),
                         div(id = "appimage", img(src="appimage.png", height = 75))),
        dashboardBody(
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
            tags$head(tags$style(get_custom_html())),
             tags$script(HTML(paste('$(document).ready(function() { $("header").find("nav").append(\'<span class="bodyTitle">', app_title, '</span>\'); })'))),
            tabsetPanel(id = "tabset",
                        
                        tabPanel("About STRIDE", fluid = TRUE,
                                 fluidRow(
                                         column(6,
                                                #br(),
                                                h4(p("STRIDE at Rutgers Cancer Institute of New Jersey")),
                                                h5(p("Surveillance, Tracking and Reporting through Informed Data Collection and Engagement (STRIDE) is an interactive data and visualization dashboard that includes clinical trials enrollment, biospecimen inventory, tumor registry analytic cases, and catchment area information related to cancer burden, behavioral and environmental risk factors, and demographics")),
                                                br(),
                                                h5(p("The STRIDE Dashboard may be useful to researchers who are hoping to better integrate the CINJ catchment area into their research and better understand the communities they serve.")),
                                                br(),
                                                h5(p("New features and updates to current information will be added as data becomes available. Any comments, suggestions or questions are welcomed at daniel.pearson@rutgers.edu"),
                                                   p(), ".")
                                                
                                                #hr(),
                                                
                                         ),
                                         column(6,
                                                #br(),
                                                #             HTML('<img src="GregPicCrop.png", height="110px"
                                                # style="float:right"/>','<p style="color:black"></p>'),
                                                h4(p("About the Data")),
                                                h5(p("Clinical Trials: Data in this section include clinical trials enrollment at CINJ and RWJBarnabas Health affiliates from 2017 to 2020. Data may be filtered by patient demographics, as well as specific trial information such as phase, tumor study group, and protocol type."),
                                                   p("Biospecimens: This searchable table includes current biospecimen counts for protocol #001006. Data may be filtered by disease site, gender, race/ethnicity as well as sample type."),
                                                   p("Cancer Rates, Screening & Risk Factors: This section utilizes publicly available data to provide comparative, interactive visualzations. This includes the ability to plot variables on x- and y-axes and visualize incidence and mortality trends over time."),
                                                   p("RWJBH Tumor Registries: These visualiztions are backed by analytic cases data from each of the RWJ Barnabas Health tumor registries. The current version allows users to filter by demographic information for disease sites; future updates will include more in-depth information about analytic cases within the RWJBarnabas Health system."),
                                                   p("New Jersey Maps: Current interactive maps include cancer incidence and screening rates, along with selected behavioral risk factors. This section also contains an interactive map of environmental risk factors, including air pollution, power plant sites, and NPL superfund sites."),
                                                   
                                                ),
                                                br()
                                         )
                                 ),
                                 br(),
                                 hr(),
                                 h5("Sources:"),
                                 h6(
                                         p("Clinical Trials Enrollment Data: RBHS OnCore")),
                                           
                                 h6(
                                         p("Biospecimen Data: CINJ Office of Human Research Services")),
                                
                                 h6(
                                         p("Analytic Cases Data: RWJBarnabas Health Tumor Registries")),
                                        
                                           
                                 h6(
                                         p("Cancer Incidence and Mortality Data:",
                                           a("New Jersey State Cancer Registry",
                                             href = "https://www.cancer-rates.info/nj/"))),
                                 h6(
                                         p("Behavioral Risk Factors:",
                                           a("New Jersey State Health Assessment Data (NJSHAD) Behavioral Risk Factor Survey (BRFS)",
                                             href = "https://www-doh.state.nj.us/doh-shad/home/Welcome.html"))),
                                 h6(
                                         p("Envoronmental Risk Factors: ",
                                           a("US EPA National Air Toxics Assessment (NATA)",
                                             href = "https://www.epa.gov/national-air-toxics-assessment"))),
                                 h6(
                                         p("Other Cancer-Related Data: ",
                                           a("CDC United States Cancer Statistics (USCS)",
                                             href = "https://www.cdc.gov/cancer/uscs/dataviz/download_data.htm"))),
                                 
                                 h5("Built by Daniel Pearson, Community Outreach and Engagment at Rutgers Cancer Institute of New Jersey with the power of",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                    "by",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                                    ".")
                        ),
            
                        
                        
                        
               # Tab 1                        
               navbarMenu("Clinical Trials",
                    tabPanel(title = "Clinical Trials Enrollment at CINJ",
                             value = "clinical_trials",
                             tags$div(class = "topspacing"),
                             fluidRow(column(2, createPickerInput("trial_site", "Choose RWJBH Site", trial_list, selected = "CINJ"),
                                             createCheckboxGroupInput(inputId  = "year",
                                                                      label    = "Year",
                                                                      choices  = all_years),
                                             createCheckboxGroupInput(inputId  = "gender",
                                                                      label    = "Gender",
                                                                      choices  = all_sex),
                                             createCheckboxGroupInput(inputId  = "race_ethnicity",
                                                                      label    = "Race/Ethnicity",
                                                                      choices  = all_races)),
                                      column(10, tags$p(), plotlyOutput("accrual", height = plot_height)))
                    )
               ),
               # Tab 2
               navbarMenu("Biospecimens",
                tabPanel(title = "Total Samples", 
                         value = "total_samples",
                         tags$div(class = "topspacing"),
                         HTML("<h4><center>Available Biospecimen Samples by Race/Ethnicity & Gender - Protocol 001006 (Total Patient Samples as of 11/24/2020)</center></h4>"),
                         reactableOutput("brs", height = plot_height)),
                tabPanel(title = "Unique Samples",
                         value = "unique_samples",
                         tags$div(class = "topspacing"),
                         HTML("<h4><center>Available Biospecimen Samples by Race/Ethnicity & Gender - Protocol 001006 (Unique Samples as of 11/24/2020)</center></h4>"),
                         reactableOutput("brs2", height = plot_height))),
              # Tab 3     
              navbarMenu("Cancer Rates, Screening and Risk Factors",
                tabPanel(title = "Cancer and Risk Factors",
                         value = "cancer_risk_factors",
                         tags$div(class = "topspacing"),
                         fluidRow(column(9, tags$p(), tags$p(), plotlyOutput("cancer_risk", height = plot_height)),
                                  column(3, reactableOutput("risk_react", height = plot_height)))
                ),
                
                tabPanel(title = "Cancer Rate Trends (1999-2017)",
                         value = "cancer_trends",
                         tags$div(class = "topspacing"),
                         fluidRow(column(12, girafeOutput("trend_plot1", height = plot_height))),
                                  
                         ),
                
                tabPanel(title = "County Incidence and Mortality (2014-2018)",
                         value = "county_girafe",
                         tags$div(class = "topspacing"),
                         fluidRow(column(12, girafeOutput("girafe_plot1", height = plot_height))),
                         
                ),
                
                tabPanel(title = "County Behavioral Risk Factors (2013-2017)",
                         value = "county_brfs",
                         tags$div(class = "topspacing"),
                         fluidRow(column(12, girafeOutput("girafe_plot2", height = plot_height))),
                         
                ),
                
                tabPanel(title = "Top-12 Cancers (Radar Chart)",
                         value = "top12_cancers",
                         tags$div(class = "topspacing"),
                         plotOutput("county_radar", height = plot_height))),
                # Tab 4                        
                navbarMenu("RWJBH Tumor Registries",
                    tabPanel(title = "Analytic Cases by Disease Site",
                             value = "analytic_cases",
                             tags$div(class = "topspacing"),
                             plotOutput("disease_site", height = plot_height)),
                    tabPanel(title = "Analytic Cases by Disease Site v2",
                             value = "analytic_cases_v2",
                             tags$div(class = "topspacing"),
                             plotlyOutput("disease_site2", height = plot_height)
                    ),
                    tabPanel(title = "Age Distribution by Race/Ethnicity",
                             value = "age_distribution",
                             tags$div(class = "topspacing"),
                             fluidRow(column(6, plotOutput("boxplot1", height = plot_height)),
                                      column(6, plotOutput("boxplot2", height = plot_height))))
                ),
                # Tab 5                        
                navbarMenu("New Jersey Maps",
                    tabPanel(title = "County Map",
                             value = "county_map",
                             tags$div(class = "topspacing"),
                             tmapOutput("countymap1", height = plot_height)),
                    
                    tabPanel(title = "Air Pollutant Map",
                             value = "air_pollutant_map",
                             tags$div(class = "topspacing"),
                             tmapOutput("air_risk", height = plot_height)),
                    
                    tabPanel(title = "County Map v2", 
                             value = "county_map_v2",
                             tags$div(class = "topspacing"),
                             leafletOutput("countymap2", height = plot_height)),
                    
                    tabPanel(title = "Air Pollutant Map v2", 
                             value = "air_pollutant_map_v2",
                             tags$div(class = "topspacing"),
                             leafletOutput("pollution", height = plot_height))
                    )
            ) # end tabsetPanel
        ), # end dashboardBody
        controlbar = dashboardControlbar(id = "controlBar",
                                         width = sidebar_width,
                                         overlay = FALSE,
                                         tags$div(id = "controlbar_content",
                                                  HTML("<h4><center>Plot settings</center></h4>"),
                                                  tags$hr(),
                                                  fluidRow(column(1), 
                                                           column(10, createCheckboxInput("axis_type", "Log scale"))))),
        title = app_title
)#end dashboardPage