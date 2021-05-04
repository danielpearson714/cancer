source("supporting/plots.R")
source("supporting/tables.R")

lapply(c("shinyWidgets", "bslib", "reactable", "tidyr", "data.table", "dplyr", "forcats", "viridis", "ggplot2", "shinydashboard",
         "ggrepel", "sf", "tidycensus", "ggradar", "scales", "tidytext", "tmap", "leaflet", "shinyBS", "plotly"), require, character.only = TRUE)

# map settings
options(bitmapType="cairo")
options(tigris_use_cache = TRUE)
census_api_key("81cc090027aa172987dc489efcbb5576416671ba")

## global variables and functions
data_path <- "data"

read_csv_file <- function(filename) {
    read.csv(file.path(data_path, filename))
}

ui_tooltip <- function(id, label = "", text = "") {
    if (is.null(text) || is.na(text) || (text == "")) {
        warning("ui_tooltip() called without tooltip text.")
    }
    result <- shiny::span(
        class = "input-label-with-tt",
        label,
        shiny::img(id = id,
                   src =    "tooltip.png",
                   height = "16px",
                   width =  "16px"),
        shinyBS::bsTooltip(id = id, text, placement = "top"))
    return(result)
}

# UI functions and variables
createPickerInput <- function(inputId, label, choices, selected = NULL) {
    pickerInput(inputId  = inputId,
                label    = ui_tooltip(paste0(inputId, 'Tooltip'), label, label),
                choices  = choices,
                selected = selected,
                multiple = TRUE,
                options  = list('actions-box' = TRUE))
}

createCheckboxInput <- function(inputId, label, value = FALSE) {
  checkboxInput(inputId  = inputId,
                label    = ui_tooltip(paste0(inputId, 'Tooltip'), label, label),
                value = value,
                width = "60%")
}

createCheckboxGroupInput <- function(inputId, label, choices, selected = choices) {
    checkboxGroupInput(inputId  = inputId,
                       label    = ui_tooltip(paste0(inputId, 'Tooltip'), label, label),
                       choices  = choices,
                       selected = selected)
}

createVarSelectInput <- function(inputId, label, data, selected) {
    varSelectizeInput(inputId  = inputId,
                      label    = ui_tooltip(paste0(inputId, 'Tooltip'), label, label),
                      data     = data,
                      selected = selected)
}
  
createSelectInput <- function(inputId, label, choices, selected = choices) {
    selectizeInput(inputId  = inputId,
                   label    = ui_tooltip(paste0(inputId, 'Tooltip'), label, label),
                   choices  = choices,
                   selected = selected)
}

createSliderInput <- function(inputId, label, min, max, value) {
    sliderInput(inputId = inputId,
                label   = ui_tooltip(paste0(inputId, 'Tooltip'), label, label),
                min     = min,
                max     = max,
                value   = value)
}

app_title <- "Catchment Area Research Dashboard"
all_years <- c("2016" = "2016",
               "2017" = "2017",
               "2018" = "2018",
               "2019" = "2019",
               "2020" = "2020")

recent_years <- c("2019" = "2019",
                  "2020" = "2020")
all_sex <- c("Female" = "Female",
             "Male" = "Male")

all_races <- c("White" = "White",
               "Black/African American" = "Black/African American",
               "Asian/Pacific Islander" = "Asian/Pacific Islander",
               "Hispanic/Latino" = "Hispanic/Latino",
               "American Indian or Alaska Native" = "American Indian or Alaska Native",
               "Other/Unknown" = "Other/Unknown")

all_races2 <- c("White" = "White",
                "Black" = "Black",
                "Asian" = "Asian",
                "Hispanic/Latino" = "Hispanic/Latino",
                "Other/Unknown" = "Other/Unknown")

specimen_types <- c("Biomarker",
                    "Blood",
                    "Body Fluid",
                    "Bone Marrow",
                    "Glass Slides",
                    "Outside Paraffin Blocks",
                    "Tissue",
                    "Urine")
sidebar_width <- 250
plot_height   <- "85vh"

get_custom_html <- function() {
  HTML('.bodyTitle { 
      font-size: 20px;
      line-height: 50px;
      text-align: left;
      font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
      padding-left: 25%;
      overflow: hidden;
      color: white;
      }
      /* main sidebar */
      .skin-blue .main-sidebar {
                            background-color: gray;
                            }

      /* active selected tab in the sidebarmenu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: gray;
                            }

      /* other links in the sidebarmenu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: gray;
                            color: #000000;
                            }

      /* other links in the sidebarmenu when hovered */
       .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: gray;
                            }
      /* toggle button when hovered  */                    
       .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: gray;
                            }
      ')
}

# Color Pallette for Risk Factor reactable (Tab 3)
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
good_color <- make_color_pal(c("#FFCDD2FF", "#EF9A9AFF", "#E57373FF", "#F44336FF", "#D32F2FFF"), bias = 2)

newpal  <- colorQuantile("Reds", n = 5, domain = NULL)
newpal2 <- colorQuantile("plasma", n = 5, domain = NULL, reverse = TRUE)

# bootstrap custom theme
my_theme <- bs_theme(
  bg = "white", fg = "midnightblue", primary = "darkred",
  base_font = font_google("Roboto")
)

# Data loading
nj_counties <- tigris::counties("NJ", class = "sf")
dashboard_risk <- read_csv_file("dashboard_risk.csv")
county_risk <- nj_counties %>% 
    left_join(dashboard_risk, by = c("GEOID" = "County_ID")) %>% 
    st_transform(4326)

nj_tracts <- tigris::tracts("NJ", class = "sf", )
risk <- read_csv_file("pollutantrisk.csv") %>% 
    mutate_at(5, as.character)
nj_tracts <- nj_tracts %>% 
    left_join(risk, by = c("GEOID" = "Tract")) %>% 
    st_transform(4326)

registry_new  <- read_csv_file("All Sites Cleaned - 2019.csv")
master_report <- read_csv_file("Mock Data (master_report).csv")
cinj2         <- read_csv_file("cinj2.csv")
new_trials    <- read_csv_file(("Mock Data (new_trials).csv")) %>% 
  mutate_at(c(2,7), as.numeric) %>%
  filter(Age >= 0)

brs <- read_csv_file("Mock Data (BRS).csv") %>% 
      rename(Race_Ethnicity = X)

brs2 <- brs %>% 
      group_by(SEQ., SPECIMEN_TYPE) %>% 
      #group_by(SPECIMEN_TYPE) %>% 
      slice_head(n = 1)
    
npl_sites <- read_csv_file("NPLSuperfunds.csv")
npl_sites <- st_as_sf(npl_sites, coords = c("LONGITUDE", "LATITUDE"))   
st_crs(npl_sites) <- 4326
st_crs(npl_sites)

pp_sites <- read_csv_file("powerplantSHP.csv")
pp_sites <- st_as_sf(pp_sites, coords = c("LONGITUDE", "LATITUDE"))
st_crs(pp_sites) <- 4326
st_crs(pp_sites)

# input choice lists
site_list  <- as.list(sort(unique(new_trials$Disease.Site)))
rwj_list   <- as.list(sort(unique(master_report$RWJBH.Site)))
proto_list <- as.list(sort(unique(new_trials$Protocol.Type)))
phase_list <- as.list(sort(unique(new_trials$Phase)))
tsg_list   <- as.list(sort(unique(new_trials$Subject.Tumor.Study.Group)))
data4_list <- as.list(sort(unique(new_trials$Data.Table.4.Report.Type)))
clin_list  <- as.list(sort(unique(master_report$Clin_Stage)))
path_list  <- as.list(sort(unique(master_report$Path_Stage)))
trial_list <- as.list(sort(unique(new_trials$X)))
dis_list   <- as.list(sort(unique(master_report$Disease.Site)))
risk       <- dashboard_risk %>% select(-county, -NAME, -County_ID)
risk_list  <- list(
  'Risk Factor (%)'         = c(`Obese` = 'Obese', `Current Smoker` = 'Current.Smoker', `Binge Drinking` = 'Binge.Drinking'),
  'Cancer Screening (%)'    = c(`Mammography` = 'Mammography', `Pap smear` = 'Pap.Smear', `Colorectal` = 'Colorectal', `PSA Test` = 'PSA.Test'),
  'Cancer Incidence (Rate)' = c(`Overall Cancer Incidence` = 'Overall.Cancer.Incidence', `Breast Cancer` = 'Breast.Cancer', `Prostate Cancer` = 'Prostate.Cancer',
                                `Lung Cancer` = 'Lung.Cancer', `Liver Cancer` = 'Liver.Cancer', `Thyroid Cancer` = 'Thyroid.Cancer', 
                                `Kidney Cancer` = 'Kidney.Cancer', `Colorectal Cancer` = 'Colorectal.Cancer', `Esophageal Cancer` = 'Esophageal.Cancer', 
                                `Uterine Cancer` = 'Uterine.Cancer', `Pancreatic Cancer` = 'Pancreatic.Cancer', `NH Lymphoma` = 'NH.Lymphoma', 
                                `Leukemia` = 'Leukemia', `Melanoma` = 'Melanoma', `Bladder Cancer` = 'Bladder.Cancer'),
  'Cancer Mortality (Rate)' = c(`Overall Cancer Mortality` = 'Overall.Cancer.Mortality'))

county_list2 <- as.list(sort(unique(dashboard_risk$county)))
county_risk2 <- county_risk %>% 
  select(18:43, -NAME.y) 
