library(shinythemes)
library(shinyWidgets)
library(tmap)
library(tmaptools)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(knitr)
library(ggrepel)
library(extrafont)
library(extrafontdb)
library(devtools)
library(tidytext)
library(tigris)
library(csv)
library(forcats)
library(usethis)
library(sp)
library(data.table)
library(reactable)
library(tidycensus)
library(tidyr)
library(stringr)
library(paletteer)
library(RColorBrewer)
library(bslib)
library(ggradar2)
library(ggradar)
library(tibble)
library(scales)
library(viridis)
library(htmltools)
library(htmlwidgets)

################################ Dataframes
nj_counties <- tigris::counties("NJ", class = "sf")
dashboard_risk <- read.csv("dashboard_risk.csv")
county_risk <- nj_counties %>% 
    left_join(dashboard_risk, by = c("GEOID" = "County_ID")) %>% 
    st_transform(4326)

nj_tracts <- tigris::tracts("NJ", class = "sf", )
risk <- read.csv("pollutantrisk.csv") %>% 
    mutate_at(5, as.character)
nj_tracts <- nj_tracts %>% 
    left_join(risk, by = c("GEOID" = "Tract")) %>% 
    st_transform(4326)

registry_new <- read.csv("All Sites Cleaned - 2019.csv")
master_report <- read.csv("Master Report.csv")
cinj2 <- read.csv("cinj2.csv")
new_trials <- read.csv(("new_trials.csv"))
new_trials <- new_trials %>% 
  mutate_at(7, as.numeric)
brs <- read.csv("BRS.csv")

options(tigris_use_cache = TRUE)
census_api_key("81cc090027aa172987dc489efcbb5576416671ba")

npl_sites <- read.csv("NPLsuperfunds.csv")
npl_sites <- st_as_sf(npl_sites, coords = c("LONGITUDE", "LATITUDE"))   
st_crs(npl_sites) <- 4326
st_crs(npl_sites)

pp_sites <- read.csv("powerplantSHP.csv")
pp_sites <- st_as_sf(pp_sites, coords = c("LONGITUDE", "LATITUDE"))
st_crs(pp_sites) <- 4326
st_crs(pp_sites)
################################# input choice lists
site_list <- as.list(sort(unique(new_trials$Disease.Site)))
rwj_list <- as.list(sort(unique(master_report$RWJBH.Site)))
proto_list <- as.list(sort(unique(new_trials$Protocol.Type)))
phase_list <- as.list(sort(unique(new_trials$Phase)))
tsg_list <- as.list(sort(unique(new_trials$Subject.Tumor.Study.Group)))
data4_list <- as.list(sort(unique(new_trials$Data.Table.4.Report.Type)))
clin_list <- as.list(sort(unique(master_report$Clin_Stage)))
path_list <- as.list(sort(unique(master_report$Path_Stage)))
trial_list <- as.list(sort(unique(new_trials$X)))
dis_list <- as.list(sort(unique(master_report$Disease.Site)))
risk <- dashboard_risk %>% 
    select(-county, -NAME, -County_ID)
risk_list <- as.list(sort(colnames(risk)))

county_list2 <- as.list(sort(unique(dashboard_risk$county)))
county_risk2 <- county_risk %>% 
  select(18:43, -NAME.y) 

brf_list <- as.list(sort(colnames(county_risk2 %>% 
                                    select(2:24))))
################################## ACS Data
tract_moe <- get_acs(geography = "tract", state = "NJ", geometry = FALSE, 
                 variables = c(median_income = "B19013_001",
                               spanish_speaking = "B06007_003",
                               less_than_well = "B06007_005",
                               black_aa = "B02001_003",
                               insurance = "B27001_001",
                               education = "B06009_001",
                               high_school = "B15003_017",
                               hispanic = "B03002_003",
                               stamps = "B19058_001",
                               drive = "B08101_009",
                               walk = "B08101_033"),
                 summary_var = c(population = "B01003_001")) %>% 
  mutate(pct = 100 * (estimate / summary_est)) %>% 
  select(GEOID, NAME, variable, estimate, pct) %>% 
  pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = c(estimate, pct))

tract_moe2 <- get_acs(geography = "tract", state = "NJ", geometry = TRUE, 
                  variables = c(population = "B01003_001")) %>% 
  select(-variable, -moe) %>% 
  rename(population = estimate)

##################################### Color Pallette for Risk Factor reactable (Tab 3)
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

good_color <- make_color_pal(c("#FFCDD2FF", "#EF9A9AFF", "#E57373FF", "#F44336FF", "#D32F2FFF"), bias = 2)


##################################### bs_lib custom theme
my_theme <- bs_theme(
  bg = "white", fg = "midnightblue", primary = "darkred",
  base_font = font_google("Roboto")
)

##################################### Add Year column to trials data (Tab 1)


####################################### UI

ui <- fluidPage(theme = my_theme,

                
                titlePanel(title = div(img(src="RWJBHlogo.png", height = 150, width = 550))),
                
                navbarPage("Catchment Area Research Dashboard",
                
                    tabsetPanel(
######################################### Tab 1                        
                      navbarMenu("Clinical Trials",
                        tabPanel(title = "Clinical Trials Enrollment at CINJ",
                            sidebarLayout(
                            sidebarPanel(width = 3,
                                         pickerInput(inputId = "disease_site",
                                                     label = "Disease Site",
                                                     choices = site_list,
                                                     multiple = TRUE,
                                                     options = list('actions-box' = TRUE),
                                                     selected = site_list),
                                         pickerInput(inputId = "data4",
                                                            label = "Data Table 4 Type",
                                                            choices = data4_list,
                                                            multiple = TRUE,
                                                            options = list('actions-box' = TRUE),
                                                            selected = "Interventional"),
                                         pickerInput(inputId = "protocol",
                                                            label = "Protocol Type",
                                                            choices = proto_list,
                                                            multiple = TRUE,
                                                            options = list('actions-box' = TRUE),
                                                            selected = "Treatment"),
                                         pickerInput(inputId = "phase",
                                                            label = "Phase",
                                                            choices = phase_list,
                                                            multiple = TRUE,
                                                            options = list('actions-box' = TRUE),
                                                            selected = phase_list),
                                         pickerInput(inputId = "tsg",
                                                            label = "Subject Tumor Study Group",
                                                            choices = tsg_list,
                                                            multiple = TRUE,
                                                            options = list('actions-box' = TRUE),
                                                            selected = tsg_list)),
                            mainPanel(
                            fluidRow(
                            column(2,
                                   pickerInput(inputId = "trial_site",
                                               label = "Choose RWJBH Site",
                                               choices = trial_list,
                                               multiple = TRUE,
                                               options = list('actions-box' = TRUE),
                                               selected = "CINJ")),
                            column(1, 
                                 checkboxGroupInput(inputId = "year",
                                                    label = "Year",
                                                    choices = c("2016" = "2016",
                                                                "2017" = "2017",
                                                                "2018" = "2018",
                                                                "2019" = "2019",
                                                                "2020" = "2020"),
                                                    selected = c("2016", "2017", "2018", "2019", "2020"))),
                            column(1, 
                                 checkboxGroupInput(inputId = "gender",
                                                    label = "Gender",
                                                    choices = c("Female" = "Female",
                                                                "Male" = "Male"),
                                                    selected = c("Female", "Male"))),
                            column(2,
                                 checkboxGroupInput(inputId = "race_ethnicity",
                                                    label = "Race/Ethnicity",
                                                    choices = c("White" = "White",
                                                           "Black/African American" = "Black/African American",
                                                           "Asian/Pacific Islander" = "Asian/Pacific Islander",
                                                           "Hispanic/Latino" = "Hispanic/Latino",
                                                           "American Indian or Alaska Native" = "American Indian or Alaska Native",
                                                           "Other/Unknown" = "Other/Unknown"),
                                                    selected = c("White", "Black/African American", "Asian/Pacific Islander", "Hispanic/Latino", "Other/Unknown", "American Indian or Alaska Native"))),
                                
                            
                                 fluidRow(
                                   column(9,
                                 plotOutput("accrual")),
                                   column(3,
                                 span(textOutput("n_value"), style = "font-size: 36px; color: darkred; font-style:bold, text-align: center"))
                                 )
                        ))))),
################################### Tab 2
                       navbarMenu("Biospecimens",
                        tabPanel(title = "Total Samples", "Available Biospecimen Samples by Race/Ethnicity & Gender - Protocol 001006 (Total Patient Samples as of 11/24/2020)",
                                 reactableOutput("brs")),
                        tabPanel(title = "Unique Samples", "Available Biospecimen Samples by Race/Ethnicity & Gender - Protocol 001006 (Unique Samples as of 11/24/2020)",
                                 reactableOutput("brs2"))),
                        
 ################################## Tab 3     
                      navbarMenu("Cancer Incidence, Screening and Risk Factors",
                        tabPanel(title = "Cancer and Risk Factors", fluid = TRUE,
                                 fluidRow(
                                   column(3,
                                 varSelectInput(inputId = "x_axis",
                                                label = "Choose x-axis",
                                                data = risk,
                                                selected = "Overall.Cancer.Incidence")),
                                 column(3,
                                 varSelectInput(inputId = "y_axis",
                                                label = "Choose y-axis",
                                                data = risk,
                                                selected = "Overall.Cancer.Mortality"))),
                                 mainPanel(
                                   fluidRow(
                                   column(8,
                                 plotOutput("cancer_risk", width = "1000px", height = "800px")),
                                   column(4,
                                 reactableOutput("risk_react", width = "400px", height = "800px"))
                                 ))),
                        
                        
                        
                        tabPanel(title = "Top-12 Cancers (Radar Chart)",
                                 pickerInput(inputId = "county_select",
                                             label = "Choose NJ county",
                                             choices = county_list2,
                                             selected = "ATLANTIC",
                                             multiple = TRUE,
                                             options = list('actions-box' = TRUE)
                                                ),
                                 plotOutput("county_radar"))
                                 
                        ),
##################################### Tab 4                        
                        navbarMenu("RWJBH Tumor Registries",
                        tabPanel(title = "Analytic Cases by Disease Site", fluid = TRUE,
                                 fluidRow(
                                 column(4,
                                        selectInput(inputId = "rwj_site",
                                                    label = "Choose RWJBH Registry",
                                                    choices = rwj_list,
                                                    selected = "New Brunswick")),
                                 column(2,
                                 checkboxGroupInput(inputId = "report_year",
                                                    label = "Year",
                                                    choices = c("2019" = "2019",
                                                                "2020" = "2020"),
                                                    selected = c("2019"))),
                                 column(6,
                                        sliderInput(inputId = "age_range",
                                                    label = "Select age range:",
                                                    min = 0,
                                                    max = 100,
                                                    value = c(0, 100))),
                                 plotOutput("disease_site")
                                 
                        )),
                        tabPanel(title = "Analytic Cases by Disease Site v2", fluid = TRUE,
                                 fluidRow(
                                   column(3,
                                                pickerInput(inputId = "rwj_site2",
                                                            label = "Choose RWJBH Registry",
                                                            choices = rwj_list,
                                                            multiple = TRUE,
                                                            options = list('actions-box' = TRUE),
                                                            selected = "New Brunswick")),
                                     
                                     column(1,
                                            checkboxGroupInput(inputId = "report_year2",
                                                               label = "Select Year(s)",
                                                               choices = c("2019" = "2019",
                                                                           "2020" = "2020"),
                                                               selected = c("2019", "2020"))),
                                     column(2,
                                            checkboxGroupInput(inputId = "gender2",
                                                               label =  "Select Gender(s)",
                                                               choices = c("Male" = "Male",
                                                                           "Female" = "Female"),
                                                               selected = c("Male", "Female"))),
                                     column(2,
                                            checkboxGroupInput(inputId = "race",
                                                               label = "Select Race/Ethnicity",
                                                               choices = c("White" = "White",
                                                                           "Black" = "Black",
                                                                           "Asian" = "Asian",
                                                                           "Hispanic/Latino" = "Hispanic/Latino",
                                                                           "Other/Unknown" = "Other/Unknown"),
                                                               selected = c("White", "Black", "Asian", "Hispanic/Latino", "Other/Unknown"))),
                                     column(4,
                                            sliderInput(inputId = "age_range2",
                                                        label = "Select age range:",
                                                        min = 0,
                                                        max = 100,
                                                        value = c(0, 100)))),
                                     plotOutput("disease_site2", height = "100%", width = "100%"),
                                     downloadBttn("case_report", label = "Generate Report")
                                 ),
                        
                        tabPanel(title = "Age Distribution by Race/Ethnicity",
                                 fluidRow(
                                column(6,
                                 pickerInput(inputId = "registry1",
                                             label = "Choose registry site",
                                             choices = rwj_list,
                                             selected = rwj_list,
                                             multiple = TRUE,
                                             options = list('actions-box' = TRUE)
                                 ),
                                 pickerInput(inputId = "dis1",
                                             label = "Choose disease site",
                                             choices = dis_list,
                                             selected = dis_list,
                                             multiple = TRUE,
                                             options = list('actions-box' = TRUE)
                                 ),
                                 plotOutput("boxplot1")),
                                column(6,
                                       pickerInput(inputId = "registry2",
                                                   label = "Choose registry site",
                                                   choices = rwj_list,
                                                   selected = rwj_list,
                                                   multiple = TRUE,
                                                   options = list('actions-box' = TRUE)
                                       ),
                                       pickerInput(inputId = "dis2",
                                                   label = "Choose disease site",
                                                   choices = dis_list,
                                                   selected = dis_list,
                                                   multiple = TRUE,
                                                   options = list('actions-box' = TRUE)
                                       ),
                                           
                                       plotOutput("boxplot2")
                        
                        )))),
                        
########################################## Tab 5                        
                        navbarMenu("Maps",
                        
                        tabPanel(title = "County Map",
                                 tmapOutput("countymap1", height = "600px", width = "75%")),
                        
                        tabPanel(title = "Air Pollutant Map",
                                 tmapOutput("air_risk", height = "600px", width = "75%")),
                        
                        tabPanel(title = "County Map v2", "Cancer-Related Risk Factors by County",
                                 varSelectInput(inputId = "county_vars",
                                                label = "Choose variable",
                                                data = county_risk2 %>% 
                                                  select(2:24),
                                                selected = "Obese"),
                                 leafletOutput("countymap2", height = "750px", width = "75%")),
                        
                        tabPanel(title = "Air Pollutant Map v2", "Estimated Cancer Risk per 1M Residents, by Air Toxin (2014 National Air Toxics Assessment)",
                                 selectInput(inputId = "air_risk2",
                                                label = "Choose air pollutant",
                                                choices = c(
                                                  "X1.3.Butadiene" = "X1.3.Butadiene",
                                                  "Acetaldehyde" = "Acetaldehyde",
                                                  "Benzene" = "Benzene",
                                                  "Ethylene.Oxide" = "Ethylene.Oxide",
                                                  "Formaldehyde" = "Formaldehyde",
                                                  "Naphthalene" = "Naphthalene"),
                                                selected = "Acetaldehyde"),
                                 leafletOutput("pollution", height = "750px", width = "75%"))
                        )
                        )))

################################### SERVER
server <- function(input, output) {
    
    cinj_shiny <- reactive({new_trials %>% 
            filter(Disease.Site != "") %>% 
            filter(Gender %in% input$gender,
                   X %in% input$trial_site,
                   Disease.Site %in% input$disease_site,
                   Year %in% input$year,
                   Race.Ethnicity %in% input$race_ethnicity,
                   Data.Table.4.Report.Type %in% input$data4,
                   Protocol.Type %in% input$protocol,
                   Phase %in% input$phase,
                   Subject.Tumor.Study.Group %in% input$tsg)
        
    })
    
    ### Tab 1: Clinical Trials Accrual
    output$accrual <- renderPlot({
        
      cinj_react <- cinj_shiny()
      
        ggplot(cinj_react) +
            geom_histogram(aes(x = Age), fill = "firebrick3", color = "black", binwidth = 1) +
            scale_x_continuous("Patient Age at Enrollment", limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
            scale_y_continuous(NULL) +
            ggtitle(paste("Clinical Trials Enrollment (by Disease Site) - ", input$disease_site)) +
            labs(caption = "OnCore Subject Search: 1/5/2021 (Does not include studies where Disease Site is not captured)") +
            theme(plot.title = element_text(size = 20),
                  axis.text = element_text(size = 16, color = "black"),
                  axis.title = element_text(size = 17, color = "black"),
                  panel.background = element_rect(fill = "aliceblue"))
    }, height = 600, width = 1000)
    
    output$n_value <- renderText({
      
      cinj_react <- cinj_shiny() %>%
        n_distinct()
      
      paste("n =", cinj_react)
    })
    
    ### Tab 2: Biospecimen Tables
    
    brs <- brs %>% 
      rename(Race_Ethnicity = X)
    brs2 <- brs %>% 
      group_by(SEQ., SPECIMEN_TYPE) %>% 
      slice_head(n = 1)
    
    
    output$brs <- renderReactable({
      brs_react <- brs %>% 
        group_by(DISEASE_SITE, SPECIMEN_TYPE, Race_Ethnicity, GENDER) %>% 
        count() %>% 
        arrange(DISEASE_SITE, SPECIMEN_TYPE) %>% 
        pivot_wider(names_from = SPECIMEN_TYPE, values_from = n) %>% 
        relocate(Tissue, .after = GENDER) %>% 
        reactable(groupBy = c("DISEASE_SITE", "Race_Ethnicity"),
                  defaultColDef = colDef(
                    align = "center",
                    headerStyle = list(
                      background = "lightgray",
                      color = "black"
                    ),
                    footerStyle = list(
                      fontWeight = "bold"
                    )),
                  style = list(
                    fontFamily = "Sans-Serif"
                  ),
                  columns = list(
                    Race_Ethnicity = colDef(name = "Race/Ethnicity"),
                    DISEASE_SITE = colDef(name = "Primary Disease Site",
                                          footer = "TOTAL",
                                          filterable = TRUE),
                    GENDER = colDef(name = "Gender"),
                    Blood = colDef(aggregate = "sum",
                                   footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")
                    ),
                    `Outside Paraffin Blocks` = colDef(aggregate = "sum",
                                                       footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    Tissue = colDef(aggregate = "sum",
                                    name = "Tumor Tissue",
                                    footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    Urine = colDef(aggregate = "sum",
                                   footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    `Glass Slides` = colDef(aggregate = "sum",
                                            footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    `Body Fluid` = colDef(aggregate = "sum",
                                          footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    `Bone Marrow` = colDef(aggregate = "sum",
                                           footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    Biomarker = colDef(aggregate = "sum",
                                       footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }"))
                  ),
                  columnGroups = list(
                    colGroup(name = "", columns = c("Race_Ethnicity", "DISEASE_SITE")),
                    colGroup(name = "Specimen Type",
                             columns = c(
                               "Biomarker",
                               "Blood",
                               "Body Fluid",
                               "Bone Marrow",
                               "Glass Slides",
                               "Outside Paraffin Blocks",
                               "Tissue",
                               "Urine"
                             ))),
                  
                  bordered = TRUE,
                  highlight = TRUE,
                  striped = TRUE)
      print(brs_react)
      })
      
    
    output$brs2 <- renderReactable({
      brs_react2 <- brs2 %>% 
        group_by(DISEASE_SITE, SPECIMEN_TYPE, Race_Ethnicity, GENDER) %>% 
        count() %>% 
        arrange(DISEASE_SITE, SPECIMEN_TYPE) %>% 
        pivot_wider(names_from = SPECIMEN_TYPE, values_from = n) %>% 
        relocate(Tissue, .after = GENDER) %>% 
        reactable(groupBy = c("DISEASE_SITE", "Race_Ethnicity"),
                  defaultColDef = colDef(
                    align = "center",
                    headerStyle = list(
                      background = "lightgray",
                      color = "black"
                    ),
                    footerStyle = list(
                      fontWeight = "bold"
                    )),
                  style = list(
                    fontFamily = "Sans-Serif"
                  ),
                  columns = list(
                    Race_Ethnicity = colDef(name = "Race/Ethnicity"),
                    DISEASE_SITE = colDef(name = "Primary Disease Site",
                                          footer = "TOTAL",
                                          filterable = TRUE),
                    GENDER = colDef(name = "Gender"),
                    Blood = colDef(aggregate = "sum",
                                   footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")
                    ),
                    `Outside Paraffin Blocks` = colDef(aggregate = "sum",
                                                       footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    Tissue = colDef(aggregate = "sum",
                                    name = "Tumor Tissue",
                                    footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    Urine = colDef(aggregate = "sum",
                                   footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    `Glass Slides` = colDef(aggregate = "sum",
                                            footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    `Body Fluid` = colDef(aggregate = "sum",
                                          footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    `Bone Marrow` = colDef(aggregate = "sum",
                                           footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }")),
                    Biomarker = colDef(aggregate = "sum",
                                       footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return '' + total.toFixed(0)
      }"))
                  ),
                  columnGroups = list(
                    colGroup(name = "", columns = c("Race_Ethnicity", "DISEASE_SITE")),
                    colGroup(name = "Specimen Type",
                             columns = c(
                               "Biomarker",
                               "Blood",
                               "Body Fluid",
                               "Bone Marrow",
                               "Glass Slides",
                               "Outside Paraffin Blocks",
                               "Tissue",
                               "Urine"
                             ))),
                  
                  bordered = TRUE,
                  highlight = TRUE,
                  striped = TRUE)
      print(brs_react2)
      })
      
    
    
    ### Tab 3: Cancer & Risk Factors
    
    dashboard_risk2 <- reactive({dashboard_risk})
    
    output$cancer_risk <- renderPlot({
        
        ggplot(data = dashboard_risk2(), 
               aes_string(x = input$x_axis, y = input$y_axis)) +
            geom_point( 
                       shape = 21, fill = "firebrick3", color = "black", alpha = 0.85, size = 7) +
            geom_label_repel(aes(label = county), size = 3) +
            ggtitle("Cancer Incidence Rates and Behavioral Risk Factors") +
            theme(plot.title = element_text(size = 20),
                  axis.text = element_text(size = 16, color = "black"),
                  axis.title = element_text(size = 17, color = "black"),
                  panel.background = element_rect(fill = "aliceblue"))
    })
    
    output$risk_react <- renderReactable({
      dashboard_risk2() %>% 
        select(county, input$x_axis, input$y_axis) %>% 
        reactable(pagination = FALSE,
                  compact = TRUE,
                  theme = reactableTheme(
                    borderWidth = "4px"
                  ),
                  defaultColDef = colDef(
                    align = "center"
                   ),
                  columns = list(
                    county = colDef(
                      name = "County"
                    ),
                    Obese = colDef(
                      name = "Obese",
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Obese)) / (max(dashboard_risk2()$Obese) - min(dashboard_risk2()$Obese))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Overall.Cancer.Incidence = colDef(
                      name = "Overall Incidence",
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Overall.Cancer.Incidence)) / (max(dashboard_risk2()$Overall.Cancer.Incidence) - min(dashboard_risk2()$Overall.Cancer.Incidence))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Overall.Cancer.Mortality = colDef(
                      name = "Overall Mortality",
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Overall.Cancer.Mortality)) / (max(dashboard_risk2()$Overall.Cancer.Mortality) - min(dashboard_risk2()$Overall.Cancer.Mortality))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Current.Smoker = colDef(
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Current.Smoker)) / (max(dashboard_risk2()$Current.Smoker) - min(dashboard_risk2()$Current.Smoker))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Binge.Drinking = colDef(
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Binge.Drinking)) / (max(dashboard_risk2()$Binge.Drinking) - min(dashboard_risk2()$Binge.Drinking))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Mammography = colDef(
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Mammography)) / (max(dashboard_risk2()$Mammography) - min(dashboard_risk2()$Mammography))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Pap.Smear = colDef(
                      name = "Pap Smear",
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Pap.Smear)) / (max(dashboard_risk2()$Pap.Smear) - min(dashboard_risk2()$Pap.Smear))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Colorectal = colDef(
                      name = "CRC Screen",
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Colorectal)) / (max(dashboard_risk2()$Colorectal) - min(dashboard_risk2()$Colorectal))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    PSA.Test = colDef(
                      name = "PSA Test",
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$PSA.Test)) / (max(dashboard_risk2()$PSA.Test) - min(dashboard_risk2()$PSA.Test))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Breast.Cancer = colDef(
                      name = "Breast Cancer",
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Breast.Cancer)) / (max(dashboard_risk2()$Breast.Cancer) - min(dashboard_risk2()$Breast.Cancer))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Prostate.Cancer = colDef(
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Prostate.Cancer)) / (max(dashboard_risk2()$Prostate.Cancer) - min(dashboard_risk2()$Prostate.Cancer))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Lung.Cancer = colDef(
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Lung.Cancer)) / (max(dashboard_risk2()$Lung.Cancer) - min(dashboard_risk2()$Lung.Cancer))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Bladder.Cancer = colDef(
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Bladder.Cancer)) / (max(dashboard_risk2()$Bladder.Cancer) - min(dashboard_risk2()$Bladder.Cancer))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Uterine.Cancer = colDef(
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Uterine.Cancer)) / (max(dashboard_risk2()$Uterine.Cancer) - min(dashboard_risk2()$Uterine.Cancer))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Thyroid.Cancer = colDef(
                      name = "Thyroid Cancer",
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Thyroid.Cancer)) / (max(dashboard_risk2()$Thyroid.Cancer) - min(dashboard_risk2()$Thyroid.Cancer))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Kidney.Cancer = colDef(
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Kidney.Cancer)) / (max(dashboard_risk2()$Kidney.Cancer) - min(dashboard_risk2()$Kidney.Cancer))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Melanoma = colDef(
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Melanoma)) / (max(dashboard_risk2()$Melanoma) - min(dashboard_risk2()$Melanoma))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Esophageal.Cancer = colDef(
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Esophageal.Cancer)) / (max(dashboard_risk2()$Esophageal.Cancer) - min(dashboard_risk2()$Esophageal.Cancer))
                        color <- good_color(normalized)
                        list(background = color)
                      }),
                    Liver.Cancer = colDef(
                      name = "Liver Cancer",
                      style = function(value) {
                        value
                        normalized <- (value - min(dashboard_risk2()$Liver.Cancer)) / (max(dashboard_risk2()$Liver.Cancer) - min(dashboard_risk2()$Liver.Cancer))
                        color <- good_color(normalized)
                        list(background = color)
                      })
                    )
                  )
    })
    
    output$county_radar <- renderPlot({
    
      dash_risk <- reactive({dashboard_risk %>% 
          
          select(county, Prostate.Cancer, Breast.Cancer, Lung.Cancer, Colorectal.Cancer, Bladder.Cancer, Kidney.Cancer, Bladder.Cancer, Melanoma, NH.Lymphoma, Uterine.Cancer, Kidney.Cancer, Leukemia, Pancreatic.Cancer, Thyroid.Cancer) %>%
          mutate_at(vars(-county), rescale) %>% 
          filter(county %in% input$county_select)
                 
      })
        input$county_select
        ggradar(dash_risk()) +
                  theme(
                    plot.title = element_text("Top 12 Cancers in New Jersey - County Incidence Relative to State"))
    })

    ### Tab 4: Analytic Cases
    output$disease_site <- renderPlot({
        
        registry_rwj <- reactive({master_report %>% 
                filter(Disease.Site != "", Gender %in% c("Male", "Female")) %>%
                group_by(RWJBH.Site, Disease.Site, Gender, Year) %>% 
                count() %>%
                filter(RWJBH.Site %in% input$rwj_site, Year %in% input$report_year)
        })
        
        ggplot(registry_rwj(), aes(x = reorder_within(Disease.Site, n, Gender), y = n)) +
            facet_wrap(~Gender, scales = "free_y") +
            geom_col(aes(), color = "black", fill = "firebrick3", position = "stack") +
            coord_flip() +
            scale_x_reordered() +
            scale_y_continuous(limits = c(0, 450), breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450)) +
            stat_summary(fun = sum, aes(label = ..y.., group = Disease.Site), geom = "text", hjust = -0.3) +
            scale_fill_gradient(low = "firebrick4", high = "firebrick2") +
            theme(
                strip.text = element_text(size = 14, face = "bold", color = "white"),
                axis.text = element_text(size = 12, color = "black"),
                plot.title = element_text(size = 18),
                strip.background = element_rect(fill = "dodgerblue4", color = "White"),
                axis.title = element_blank(),
                legend.position = "none"
            ) +
            ggtitle("Analytic Cases at RWJBH Sites") +
            labs(caption = "RWJBH Tumor Registry Reports, 2019-2020")
    }, height = 800, width = 1200)
    
 vals <- reactiveValues()
    
    registry_rwj2 <- reactive({
        master_report %>% 
                filter(Gender %in% c("Male", "Female")) %>%
                filter(RWJBH.Site %in% input$rwj_site2, Year %in% input$report_year2, Gender %in% input$gender2, Age %inrange% input$age_range2, Race.Ethnicity %in% input$race)
    })
    
    output$disease_site2 <- renderPlot({ 
        
        case_plot <- ggplot(registry_rwj2(), mapping = aes(x = fct_rev(fct_infreq(Disease.Site)))) +
                geom_histogram(stat = "count", color = "black", fill = "firebrick3") +
                coord_flip() +
                stat_count(binwidth=1, geom="text", aes(label=..count..), hjust =-0.3) +
                scale_y_continuous() +
                scale_fill_gradient() +
                theme(
                    strip.text = element_text(size = 14, face = "bold", color = "white"),
                    axis.text = element_text(size = 12, color = "black"),
                    panel.background = element_rect(fill = "aliceblue"),
                    plot.title = element_text(size = 18),
                    strip.background = element_rect(fill = "dodgerblue4", color = "White"),
                    axis.title = element_blank(),
                    legend.position = "none"
                ) +
                ggtitle(paste("Analytic Cases at RWJBH Sites -", input$rwj_site2), subtitle = paste(c("[Year:",input$report_year2, "] - [Gender:",input$gender2, "] - [Race/Ethnicity:",input$race, "] - [Age range:",input$age_range2, "]"), collapse = " ", sep = "")) +
                labs(caption = "RWJBH Tumor Registry Reports, 2019-2020")
      
        vals$case_plot <- case_plot
        
        print(case_plot)
      }, height = 1000, width = 1000)
  
  output$case_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste("analytic_report.pdf", sep = '')},
    
    content = function(file){
      pdf(file, width = 12, height = 10)
      print(vals$case_plot)
      dev.off()
    })


  
  box1 <- reactive({master_report %>% 
      filter(!Race.Ethnicity %in% c("NA", "Unknown/Other", "Other/Unknown", "Native American"),
             RWJBH.Site %in% input$registry1,
             Disease.Site %in% input$dis1)
  })
  
  box2 <- reactive({master_report %>% 
      filter(!Race.Ethnicity %in% c("NA", "Unknown/Other", "Other/Unknown", "Native American"),
             RWJBH.Site %in% input$registry2,
             Disease.Site %in% input$dis2)
  })
  
  output$boxplot1 <- renderPlot({
  
  box1() %>% 
    ggplot() +
    geom_boxplot(aes(x = factor(Race.Ethnicity, levels = c("White", "Black", "Hispanic/Latino", "Asian")), y = Age, fill = Race.Ethnicity, group = Race.Ethnicity), shape = 21, position = "dodge", color = "black", size = 1, outlier.shape = 21, outlier.size = 3,  inherit.aes = TRUE, fatten = 1) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
    scale_fill_viridis(discrete = TRUE, option = "C", begin = 0, end = 0.8) +
    labs(x = "Race/Ethnicity", y = "Age at Diagnosis", caption = "RWJBH Tumor Registry Reports (2019 and 2020 Q1)") +
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 18),
      panel.background = element_rect(color = "black"),
      strip.background = element_rect(color = "black"),
      strip.text = element_text(size = 18, face = "bold"),
      legend.position = "none",
      plot.title = element_text(size = 18, face = c("bold"))
    ) +
    ggtitle("Age at Diagnosis by Race/Ethnicity" , subtitle = "RWJBarnabas Analytic Cases (2019 and Q1 2020)")
    
  }, height = 800, width = 1000)
  
  
  output$boxplot2 <- renderPlot({
    
    box2() %>% 
      ggplot() +
      geom_boxplot(aes(x = factor(Race.Ethnicity, levels = c("White", "Black", "Hispanic/Latino", "Asian")), y = Age, fill = Race.Ethnicity, group = Race.Ethnicity), shape = 21, position = "dodge", color = "black", size = 1, outlier.shape = 21, outlier.size = 3,  inherit.aes = TRUE, fatten = 1) +
      scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
      scale_fill_viridis(discrete = TRUE, option = "C", begin = 0, end = 0.8) +
      labs(x = "Race/Ethnicity", y = "Age at Diagnosis", caption = "RWJBH Tumor Registry Reports (2019 and 2020 Q1)") +
      theme(
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        panel.background = element_rect(color = "black"),
        strip.background = element_rect(color = "black"),
        strip.text = element_text(size = 18, face = "bold"),
        legend.position = "none",
        plot.title = element_text(size = 18, face = c("bold"))
      ) +
      ggtitle("Age at Diagnosis by Race/Ethnicity" , subtitle = "RWJBarnabas Analytic Cases (2019 and Q1 2020)")
    
  }, height = 800, width = 1000)
  
    ### Tab 5: Maps   
    output$countymap1 = renderTmap({
      tm_basemap("OpenStreetMap") +
       tm_shape(county_risk)+ 
            tm_borders("black", lwd = 0.9, alpha = 0.9) +
            tm_fill(col = c("Obese", "Current.Smoker", "Binge.Drinking", "Mammography"), alpha = 0.7, palette = "Reds", style = "quantile", id = "NAMELSAD", popup.vars = c("county", "Obese", "Current.Smoker", "Binge.Drinking", "Mammography")) +
            tm_facets(as.layers = TRUE)
 
    })
    
    output$air_risk = renderTmap({
        tm_shape(nj_tracts)+ 
            tm_borders("black", lwd = 0.5, alpha = 0.9) +
            tm_fill(col = c("Acetaldehyde", "Benzene", "Formaldehyde", "Naphthalene"), alpha = 0.7, palette = "-plasma", style = "quantile", id = "NAMELSAD10", popup.vars = c("Population", "County", "Acetaldehyde", "Benzene", "Formaldehyde", "Naphthalene")) +
            tm_facets(as.layers = TRUE) +
            tm_shape(nj_counties) +
            tm_borders("black", lwd = 0.85, alpha = 0.9) 
        
    })
   
################################# BRF Leaflet Output  
    decision <- reactive({
      if(input$county_vars == "Obese") return(county_risk2$Obese)
      if(input$county_vars == "Overall.Cancer.Incidence") return(county_risk2$Overall.Cancer.Incidence)
      if(input$county_vars == "Overall.Cancer Mortality") return(county_risk2$Overall.Cancer.Mortality)
      if(input$county_vars == "Binge.Drinking") return(county_risk2$Binge.Drinking)
      if(input$county_vars == "Current.Smoker") return(county_risk2$Current.Smoker)
      if(input$county_vars == "PSA.Test") return(county_risk2$PSA.Test)
      if(input$county_vars == "Mammography") return(county_risk2$Mammography)
      if(input$county_vars == "Colorectal") return(county_risk2$Colorectal)
      if(input$county_vars == "Pap.Smear") return(county_risk2$Pap.Smear)
      if(input$county_vars == "Colorectal.Cancer") return(county_risk2$Colorectal.Cancer)
      if(input$county_vars == "Breast.Cancer") return(county_risk2$Breast.Cancer)
      if(input$county_vars == "Lung.Cancer") return(county_risk2$Lung.Cancer)
      if(input$county_vars == "Prostate.Cancer") return(county_risk2$Prostate.Cancer)
      if(input$county_vars == "Bladder.Cancer") return(county_risk2$Bladder.Cancer)
      if(input$county_vars == "Melanoma") return(county_risk2$Melanoma)
      if(input$county_vars == "Leukemia") return(county_risk2$Leukemia)
      if(input$county_vars == "NH.Lymphoma") return(county_risk2$NH.Lymphoma)
      if(input$county_vars == "Esophageal.Cancer") return(county_risk2$Esophageal.Cancer)
      if(input$county_vars == "Thyroid.Cancer") return(county_risk2$Thyroid.Cancer)
      if(input$county_vars == "Uterine.Cancer") return(county_risk2$Uterine.Cancer)
      if(input$county_vars == "Liver.Cancer") return(county_risk2$Liver.Cancer)
      if(input$county_vars == "Pancreatic.Cancer") return(county_risk2$Pancreatic.Cancer)
      if(input$county_vars == "Kidney.Cancer") return(county_risk2$Kidney.Cancer)
      
    })
    
    newpal <- 
      colorQuantile("Reds", n = 5, domain = NULL)
    
    
    output$countymap2 = renderLeaflet({
      req(input$county_vars)
      leaflet(county_risk2) %>% 
        addTiles() %>% 
        setView(lat = 40.0583,
                lng = -74.4057,
                zoom = 8) %>% 
        addPolygons(data = county_risk2,
                    color = "black",
                    weight = 1.2,
                    fillColor = NULL,
                    fillOpacity = 0)
    })
    
    observeEvent(input$county_vars, {
      leafletProxy("countymap2") %>%
        clearShapes() %>% 
        addPolygons(data = county_risk2, 
                    fillColor = ~newpal(decision()),
                    color = "black",
                    opacity = 1,
                    fillOpacity = 0.6,
                    weight = 1,
                    dashArray = 2,
                    highlightOptions = highlightOptions(
                      color = "black",
                      weight = 2,
                      dashArray = "",
                      bringToFront = TRUE
                    )) %>% 
        addLegend("bottomright", 
                  pal = newpal,
                  values = decision(),
                  title = input$county_vars,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0(cuts[-n], " &ndash; ", cuts[-1])
                  })
                  
    })
  ################################################## Air Pollution Leaflet Output
    decision2 <- reactive({
      if(input$air_risk2 == "X1.3.Butadiene") return(nj_tracts$X1.3.Butadiene)
      if(input$air_risk2 == "Acetaldehyde") return(nj_tracts$Acetaldehyde)
      if(input$air_risk2 == "Aniline") return(nj_tracts$Aniline)
      if(input$air_risk2 == "Benzene") return(nj_tracts$Benzene)
      if(input$air_risk2 == "Ethylene.Oxide") return(nj_tracts$Ethylene.Oxide)
      if(input$air_risk2 == "Formaldehyde") return(nj_tracts$Formaldehyde)
      if(input$air_risk2 == "Naphthalene") return(nj_tracts$Naphthalene)

    })
    
    newpal2 <- 
      colorQuantile("plasma", n = 5, domain = NULL, reverse = TRUE)
    
    
    output$pollution = renderLeaflet({
      req(input$air_risk2)
      leaflet(nj_tracts) %>% 
        addTiles() %>% 
        setView(lat = 40.0583,
                lng = -74.4057,
                zoom = 8) %>% 
        addPolygons(data = county_risk2,
                    color = "black",
                    weight = 1.2,
                    fillColor = NULL,
                    fillOpacity = 0)
    })
    
    observeEvent(input$air_risk2, {
      leafletProxy("pollution") %>%
        clearShapes() %>%
        addPolygons(data = nj_tracts, 
                    fillColor = ~newpal2(decision2()),
                    color = "black",
                    opacity = 1,
                    fillOpacity = 0.7,
                    weight = 0.4,
                    dashArray = 3,
                    group = "Air Pollutant Risk",
                    highlightOptions = highlightOptions(
                      color = "black",
                      weight = 1.5,
                      dashArray = "",
                      bringToFront = FALSE),
                    popup=paste(nj_tracts$NAMELSAD, "<br>",
                                "County", nj_tracts$County, "<br>",
                                "Population:", nj_tracts$Population, "<br>",
                                "1,3-Butadiene", nj_tracts$X1.3.Butadiene, "<br>",
                                "Acetaldehyde:", nj_tracts$Acetaldehyde, "<br>",
                                "Aniline:", nj_tracts$Aniline, "<br>",
                                "Benzene:", nj_tracts$Benzene, "<br>",
                                "Ethlyene Oxide:", nj_tracts$Ethylene.Oxide, "<br>",
                                "Formaldehyde:", nj_tracts$Formaldehyde, "<br>",
                                "Naphthalene:", nj_tracts$Naphthalene)
                    ) %>%
        addPolylines(data = county_risk2,
                    color = "black",
                    weight = 1,
                    fillOpacity = 0) %>% 
        addCircleMarkers(data = npl_sites,
                         color = "blue",
                         stroke = FALSE,
                         fillOpacity = 0.5,
                         group = "NPL Superfund Sites",
                         popup=paste("Site Name:", npl_sites$SITE.NAME, "<br>",
                                     "Address:", npl_sites$ADDRESS, "<br>",
                                     "Federal Facility (Y/N):", npl_sites$FEDERAL.FACILITY)
        ) %>% 
        addCircleMarkers(data = pp_sites,
                         color = "red",
                         stroke = FALSE,
                         fillOpacity = 0.5,
                         group = "Power Plant Sites",
                         radius = pp_sites$TOTAL_MW / 50, 
                         popup=paste("Plant Name:", pp_sites$PLANT_NAME, "<br>",
                                     "Operator:", pp_sites$X.1, "<br>",
                                     "City:", pp_sites$CITY, "<br>",
                                     "Plant Type:", pp_sites$PRIMSOURCE, "<br>",
                                     "Megawatts:", pp_sites$TOTAL_MW)
                                     ) %>% 
        addLayersControl(baseGroups = "Air Pollutant Risk",
                         overlayGroups = c("Power Plant Sites", "NPL Superfund Sites"),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        addLegend("bottomright", 
                  pal = newpal2, 
                  opacity = 1, 
                  title = "Estimated Cancer Risk (per 1M)",
                  values = decision2(),
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0(cuts[-n], " &ndash; ", cuts[-1])
                  })
    })
}
##################################
shinyApp(ui = ui, server = server)

