lapply(c("shinyWidgets", "bslib", "reactable", "tidyr", "data.table", "dplyr", "forcats", "shinydashboard", "viridis", "ggplot2", 
         "ggrepel", "sf", "tidycensus", "ggradar", "scales", "tidytext", "tmap", "leaflet"), require, character.only = TRUE)

# map settings
options(bitmapType="cairo")
options(tigris_use_cache = TRUE)
census_api_key("81cc090027aa172987dc489efcbb5576416671ba")

## global variables and functions
data_path <- "data"

read_csv_file <- function(filename) {
    read.csv(file.path(data_path, filename))
}

# UI functions and variables
createPickerInput <- function(inputId, label, choices, selected = NULL) {
    pickerInput(inputId  = inputId,
                label    = label,
                choices  = choices,
                selected = selected,
                multiple = TRUE,
                options  = list('actions-box' = TRUE))
}

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

# leaflet
create_basic_leaflet <- function(data) {
    leaflet(data) %>% 
            addTiles() %>% 
            setView(lat = 40.0583,
                    lng = -74.4057,
                    zoom = 8)
}

create_leaflet <- function(data, color_data = NULL) {
    result <- create_basic_leaflet(data)
    if (is.null(color_data)) {
        result %>% 
            addPolygons(data = data,
                        color = "black",
                        weight = 1.2,
                        fillColor = NULL,
                        fillOpacity = 0)
    } else {
        result %>% 
            addPolygons(data = data, 
                        fillColor = ~newpal(color_data))
    }
}

create_coldef <- function(col_value, col_name = NULL) {
    colDef(name = col_name,
           style = function(value) {
            value
            normalized <- (value - min(col_value)) / (max(col_value) - min(col_value))
            color <- good_color(normalized)
            list(background = color)
          })
}

create_coldef_sum <- function(colname = NULL) {
    if (is.null(colname)) {
        colDef(aggregate = "sum",
               footer = JS("function(colInfo) {
                            var total = 0
                            colInfo.data.forEach(function(row) {
                              total += row[colInfo.column.id]
                            })
                            return '' + total.toFixed(0)
                          }")
        )   
    } else {
        colDef(aggregate = "sum",
               name = colname,
               footer = JS("function(colInfo) {
                            var total = 0
                            colInfo.data.forEach(function(row) {
                              total += row[colInfo.column.id]
                            })
                            return '' + total.toFixed(0)
                          }")
        )   
    }
}

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

registry_new <- read_csv_file("All Sites Cleaned - 2019.csv")
master_report <- read_csv_file("Mock Data (master_report).csv")
cinj2 <- read_csv_file("cinj2.csv")
new_trials <- read_csv_file(("Mock Data (new_trials).csv"))
new_trials <- new_trials %>% 
  mutate_at(c(2,7), as.numeric)

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
