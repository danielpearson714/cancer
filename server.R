server <- function(input, output) {

    ## Sidebar
    output$sidebarItems <- renderUI({
      result <- tagList()
      tabset <- input$tabset
      if (tabset == "clinical_trials") {
        result <- tagList(createPickerInput("disease_site", "Disease Site", site_list),
                          createPickerInput("data4", "Data Table 4 Type", data4_list, selected = "Interventional"),
                          createPickerInput("protocol", "Protocol Type", proto_list, selected = "Treatment"),
                          createPickerInput("phase", "Phase", phase_list, selected = phase_list),
                          createPickerInput("tsg", "Subject Tumor Study Group", tsg_list, selected = tsg_list))
      } else if (tabset == "cancer_risk_factors") {  
        result <- tagList(createSelectInput(inputId = "x_axis", 
                                            label = "Choose x-axis", 
                                            choices = risk_list, 
                                            selected = "Overall.Cancer.Incidence"),
                          createSelectInput(inputId = "y_axis", 
                                            label = "Choose y-axis", 
                                            choices = risk_list, 
                                            selected = "Overall.Cancer.Mortality"))
      } else if (tabset == "total_samples") {  
        result <- tagList(tags$div(class = "tabDescription", tags$em("Available Biospecimen Samples by Race/Ethnicity & Gender - Protocol 001006 (Total Patient Samples as of 11/24/2020)")))
      } else if (tabset == "unique_samples") {  
        result <- tagList(tags$div(class = "tabDescription", tags$em("Available Biospecimen Samples by Race/Ethnicity & Gender - Protocol 001006 (Unique Samples as of 11/24/2020)")))
      } else if (tabset == "top12_cancers") {  
        result <- tagList(createPickerInput("county_select", "Choose NJ county", county_list2, selected = "ATLANTIC"),
                          tags$div(class = "tabDescription", tags$em("This radar chart identifies cancer incidence ranks within each county in New jersey, ranked from 1 (best) to 21 (worst). Low-incidence (1) begins in the center of the circle, and gets higher (21) as points approach the circle's outer edge. This plot may be useful to compare cancer incidence rates across regions and selected counties.")))
      } else if (tabset == "analytic_cases") {
        result <- tagList(createSelectInput(inputId = "rwj_site",
                                            label = "Choose RWJBH Registry",
                                            choices = rwj_list,
                                            selected = "New Brunswick"),
                          createYearInput(inputId  = "report_year", selected = recent_years[1]),
                          createSliderInput(inputId = "age_range",
                                            label = "Select age range:",
                                            min = 0,
                                            max = 100,
                                            value = c(0, 100)))
      } else if (tabset == "analytic_cases_v2"){
        result <- tagList(createPickerInput("rwj_site2", "Choose RWJBH Registry", rwj_list, selected = "New Brunswick"),
                                      createYearInput(inputId  = "report_year2"),
                                      createCheckboxGroupInput(inputId  = "gender2",
                                                               label    =  "Select Gender(s)",
                                                               choices  = all_sex),
                                      createCheckboxGroupInput(inputId = "race",
                                                               label = "Select Race/Ethnicity",
                                                               choices = all_races2),
                                      createSliderInput(inputId = "age_range2",
                                                        label = "Select age range:",
                                                        min = 0,
                                                        max = 100,
                                                        value = c(0, 100)),
                          downloadBttn("case_report", label = "Generate Report", size = "sm"))
      } else if (tabset == "age_distribution") {  
        result <- tagList(HTML("<h4><center>Plot1</center></h4>"),
                          createPickerInput("registry1", "Choose registry site", rwj_list, selected = rwj_list),
                          createPickerInput("dis1", "Choose disease site", dis_list, selected = dis_list),
                          createYearInput(inputId  = "age_distribution_year1"),
                          tags$hr(),
                          HTML("<h4><center>Plot2</center></h4>"),
                          createPickerInput("registry2", "Choose registry site", rwj_list, selected = rwj_list),
                          createPickerInput("dis2", "Choose disease site", dis_list, selected = dis_list),
                          createYearInput(inputId  = "age_distribution_year2"))
      } else if (tabset == "county_map") {  
        result <- tagList(tags$div(class = "tabDescription", tags$em("Cancer-Related Risk Factors by County (2017). Risk factor and screening data: NJSHAD Behavioral Risk Factor Survey. Cancer rates: New Jersey State Cancer Registry.")))
      } else if (tabset == "county_map_v2") {
        result <- tagList(createSelectInput("county_vars", "Choose variable", choices = risk_list, selected = "Obese"),
                          tags$div(class = "tabDescription", tags$em("Cancer-Related Risk Factors by County. Risk factor and screening data: NJSHAD Behavioral Risk Factor Survey. Cancer rates: New Jersey State Cancer Registry.")))
      } else if (tabset == "cancer_trends") {
        result <- tagList(createPickerInput2(inputId = "disease_trend",
                                            label = "Disease Site:",
                                            choices = cpc_site_list,
                                            selected = "All Cancer Sites Combined"),
                          createPickerInput2(inputId = "state_trend",
                                            label = "State:",
                                            choices = cpc_state_list,
                                            selected = "New Jersey"),
                          createPickerInput2(inputId = "gender_trend",
                                            label = "Gender:",
                                            choices = cpc_sex_list,
                                            selected = "Male and Female"),
                          tags$div(class = "tabDescription", tags$em("Cancer Incidence & Mortality Trends by Race/Ethnicity (1999-2017). Data: CDC United States Cancer Statistics (USCS)")))
        
      } else if (tabset == "county_girafe") {
        result <- tagList(createPickerInput2(inputId = "state_girafe",
                                             label = "State:",
                                             choices = c("New Jersey", "New York"),
                                             selected = "New Jersey"),
                          createPickerInput2(inputId = "disease_site_girafe",
                                             label = "Disease Site:",
                                             choices = girafe_site_list,
                                             selected = "All Cancer Sites Combined"),
                          createPickerInput2(inputId = "event_girafe",
                                             label = "Rate Type:",
                                             choices = girafe_event_list,
                                             selected = "Incidence"),
                          createPickerInput2(inputId = "gender_girafe",
                                             label = "Gender:",
                                             choices = girafe_gender_list,
                                             selected = "Male and Female"),
                          createPickerInput2(inputId = "race_girafe",
                                             label = "Race/Ethnicity:",
                                             choices = girafe_race_list,
                                             selected = "All Races"),
                          tags$div(class = "tabDescription", tags$em("Age-Adjusted Cancer Incidence and Mortality Rates, by County (2014-2018)")))
        
      } else if (tabset == "county_brfs") {
        result <- tagList(createPickerInput2(inputId = "risk_brfs",
                                             label = "Risk Factor:",
                                             choices = brfs_risk_list,
                                             selected = "Obese"),
                          createPickerInput2(inputId = "gender_brfs",
                                             label = "Gender:",
                                             choices = brfs_gender_list,
                                             selected = "Male and Female"),
                          createPickerInput2(inputId = "race_brfs",
                                             label = "Race/Ethnicity:",
                                             choices = brfs_race_list,
                                             selected = "All Races"),
                          tags$div(class = "tabDescription", tags$em("Age-Adjusted Percentages - Behavioral or Modifiable Risk Factors, New Jersey by County (2013-2017)")))
      
      } else if (tabset == "air_pollutant_map") {  
        result <- tagList(tags$div(class = "tabDescription", tags$em("Estimated Cancer Risk per 1M Residents, by Air Toxin (2014 National Air Toxics Assessment). Esitmated cancer risk can be defined as the probablity of contracting cancer of the course of a lifetime, assuming continuous exposure to pollutant. Air toxics are pollutants known to cause or suspected of causing cancer or other serious health effects.")))
      } else if (tabset == "air_pollutant_map_v2") {  
        result <- tagList(createSelectInput(inputId = "air_risk2",
                                            label = "Choose air pollutant",
                                            choices = c("X1.3.Butadiene" = "X1.3.Butadiene",
                                                        "Acetaldehyde" = "Acetaldehyde",
                                                        "Benzene" = "Benzene",
                                                        "Ethylene.Oxide" = "Ethylene.Oxide",
                                                        "Formaldehyde" = "Formaldehyde",
                                                        "Naphthalene" = "Naphthalene"),
                                            selected = "Acetaldehyde"),
                          tags$div(class = "tabDescription", tags$em("Estimated Cancer Risk per 1M Residents, by Air Toxin (2014 National Air Toxics Assessment). Esitmated cancer risk can be defined as the probablity of contracting cancer of the course of a lifetime, assuming continuous exposure to pollutant. Air toxics are pollutants known to cause or suspected of causing cancer or other serious health effects.")))
      }
      result
    })
  
    
    ### Tab 1: Clinical Trials Accrual
    accrual_data <- reactive({
        new_trials %>% 
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
    
    output$accrual <- renderPlotly({
      req(nrow(accrual_data()) > 0)
      
      create_accrual_plot(accrual_data(), input$disease_site, input$axis_type)
    })
    
    ### Tab 2: Biospecimen Tables
    output$brs <- renderReactable({
      create_biospecimen_table(brs)
    })
    
    output$brs2 <- renderReactable({
      create_biospecimen_table(brs2)
    })
    
    ### Tab 3: Cancer & Risk Factors
    output$cancer_risk <- renderPlotly({
        req(c(input$x_axis, input$y_axis))
      
        create_cancer_risk_plot(dashboard_risk, input$x_axis, input$y_axis)
    })
    
    # risk table
    output$risk_react <- renderReactable({
      dashboard_risk %>% 
        select(county, input$x_axis, input$y_axis) %>% 
        create_risk_table()
    })
    
    cpc1 <- reactive({
      cpc_area %>% 
        filter(SITE %in% c(input$disease_trend),
               SEX %in% c(input$gender_trend),
               AREA %in% c(input$state_trend))
    })
    
    output$trend_plot1 <- renderGirafe({
      cpc1 <- cpc1() %>% 
        filter(RACE %in% c("White", "Black", "Hispanic", "Asian/Pacific Islander"),
               YEAR != "2014-2018") %>% 
        mutate(YEAR = as.numeric(YEAR)) %>% 
        mutate(AGE_ADJUSTED_RATE = as.numeric(AGE_ADJUSTED_RATE))
      
        girafe_trend <- ggplot(cpc1) +
        facet_wrap(~EVENT_TYPE) +
        geom_line(aes(x = YEAR, y = AGE_ADJUSTED_RATE, color = RACE), size = 1) +
        geom_point_interactive(aes(x = YEAR, y = AGE_ADJUSTED_RATE, color = RACE, tooltip = AGE_ADJUSTED_RATE, data_id = AGE_ADJUSTED_RATE), shape = 19, size = 3.5) +
        scale_x_continuous(limits = c(1999, 2018), breaks = c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2018)) +
        scale_color_viridis(NULL, discrete = TRUE, option = "C", begin = 0, end = 0.9) +
        labs(x = "Year", y = "Age-Adjusted Rate") +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          axis.title = element_text(size = 17, color = "black"),
          axis.text.y = element_text(size = 16, color = "black"),
          axis.text.x = element_text(size = 13, color = "black"),
          plot.title = element_text(size = 18, color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          strip.background = element_rect(color = "black"),
          strip.text = element_text(size = 18)
        ) +
        ggtitle("Age-Adjusted Incidence and Mortality Rates (1999-2018)", subtitle = paste(input$disease_trend, "|", input$gender_trend, "|", input$state_trend))
        
        girafe(ggobj = girafe_trend, width = 10)
        
    })
    
    #########Girafe incidence plot############
    
    county_cancer_rates <- reactive({
      county_geom %>% 
        filter(STATE %in% c(input$state_girafe),
               SITE %in% c(input$disease_site_girafe),
               SEX %in% c(input$gender_girafe),
               RACE %in% c(input$race_girafe),
               EVENT_TYPE %in% c(input$event_girafe))
    })
    
    output$girafe_plot1 <- renderGirafe({
     county_cancer_rates <- county_cancer_rates()
      
      county_girafe_plot <- county_cancer_rates %>% 
        ggplot(aes(x = AGE_ADJUSTED_RATE, y = reorder(NAME, AGE_ADJUSTED_RATE), 
                   fill = AGE_ADJUSTED_RATE)) +
        geom_errorbarh(aes(xmin = AGE_ADJUSTED_CI_LOWER, xmax = AGE_ADJUSTED_CI_UPPER)) +
        geom_point_interactive(color = "black", size = 3.5, shape = 21,
                               aes(data_id = NAME, tooltip = AGE_ADJUSTED_RATE)) +
        scale_fill_distiller(palette = "Reds", direction = 1) + 
        labs(y = "County",
             x = "Age-Adjusted Rate",
             fill = "Age-Adjusted Rate (2014-2018)") + 
        theme_minimal() +
        ggtitle("Age-Adjusted Incidence and Mortality Rates (per 100k residents)", subtitle = paste(input$event_girafe, "|",input$disease_site_girafe, "|", input$gender_girafe, "|", input$race_girafe))
      
      county_girafe_map <- county_cancer_rates %>% 
        ggplot(aes(fill = AGE_ADJUSTED_RATE)) + 
        geom_sf_interactive(aes(data_id = NAME, tooltip = NAME),  color = "black") + 
        scale_fill_distiller(palette = "Reds",
                             direction = 1, 
                             guide = FALSE) + 
        theme_void()
      
      girafe(ggobj = county_girafe_map + county_girafe_plot, width_svg = 10, height_svg = 4,
             options = list(
               opts_selection(
                 type = "multiple", only_shiny = FALSE,
                 css = "fill:goldenrod;stroke:black;"))) %>%
        girafe_options(opts_hover(css = "fill:goldenrod;"))
      
      
    })
    
    
    #########BRFS Girafe rate plot############
    
    county_brfs_rates <- reactive({
      county_geom2 %>% 
        filter(risk_factor %in% c(input$risk_brfs),
               SEX %in% c(input$gender_brfs),
               RACE %in% c(input$race_brfs))
    })
    
    output$girafe_plot2 <- renderGirafe({
      county_brfs_rates <- county_brfs_rates()
      
      county_brfs_plot <- county_brfs_rates %>% 
        ggplot(aes(x = AGE_ADJUSTED_RATE, y = reorder(NAME, AGE_ADJUSTED_RATE), 
                   fill = AGE_ADJUSTED_RATE)) +
        geom_errorbarh(aes(xmin = AGE_ADJUSTED_CI_LOWER, xmax = AGE_ADJUSTED_CI_UPPER)) +
        geom_point_interactive(color = "black", size = 3.5, shape = 21,
                               aes(data_id = NAME, tooltip = AGE_ADJUSTED_RATE)) +
        scale_fill_distiller(palette = "Blues", direction = 1) + 
        labs(y = "County",
             x = paste(input$risk_brfs, "Rate"),
             fill = "Age-Adjusted Rate (2014-2018)") + 
        theme_minimal() +
        ggtitle("Percent of Residents - Behavioral Risk Factors (NJBRFS 2013-2017)", subtitle = paste(input$risk_brfs, "|", input$race_brfs, "|", input$gender_brfs))
      
      county_brfs_map <- county_brfs_rates %>% 
        ggplot(aes(fill = AGE_ADJUSTED_RATE)) + 
        geom_sf_interactive(aes(data_id = NAME, tooltip = NAME),  color = "black") + 
        scale_fill_distiller(palette = "Blues",
                             direction = 1, 
                             guide = FALSE) + 
        theme_void()
      
      girafe(ggobj = county_brfs_map + county_brfs_plot, width_svg = 10, height_svg = 4,
             options = list(
               opts_selection(
                 type = "multiple", only_shiny = FALSE,
                 css = "fill:goldenrod;stroke:black;"))) %>%
        girafe_options(opts_hover(css = "fill:goldenrod;"))
      
      
    })
    
    ########radar plot########
    
    output$county_radar <- renderPlot({
        req(input$county_select)
      
        data <- dashboard_risk %>% 
          select(county, Prostate.Cancer, Breast.Cancer, Lung.Cancer, Colorectal.Cancer, Bladder.Cancer, Kidney.Cancer, Bladder.Cancer, Melanoma, NH.Lymphoma, Uterine.Cancer, Kidney.Cancer, Leukemia, Pancreatic.Cancer, Thyroid.Cancer) %>%
          mutate_at(vars(-county), rescale) %>% 
          filter(county %in% input$county_select)
      
        create_radar_plot(data)
    })

    ### Tab 4: Analytic Cases
    output$disease_site <- renderPlot({
        req(c(input$rwj_site, input$report_year))
      
        data <- master_report %>% 
            filter(Disease.Site != "", Gender %in% c("Male", "Female")) %>%
            group_by(RWJBH.Site, Disease.Site, Gender, Year) %>% 
            count() %>%
            filter(RWJBH.Site %in% input$rwj_site, Year %in% input$report_year)
        
        create_cases_plot(data)
    })
 
    registry_rwj_v2_plot <- reactive({
        req(c(input$rwj_site2, input$report_year2, input$gender2, input$race, input$age_range2))
      
        df <- master_report %>% 
                filter(Gender %in% c("Male", "Female")) %>%
                filter(RWJBH.Site %in% input$rwj_site2, Year %in% input$report_year2, Gender %in% input$gender2, Age %inrange% input$age_range2, Race.Ethnicity %in% input$race)
        
        create_cases2_plot(df, input$rwj_site2, input$report_year2, input$gender2, input$race, input$age_range2, input$axis_type)
    })
    
    output$disease_site2 <- renderPlotly({ 
        registry_rwj_v2_plot()
      })
  
    output$case_report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function(){paste("analytic_report.pdf", sep = '')},
      
      content = function(file){
        pdf(file, width = 12, height = 10)
        print(registry_rwj_v2_plot())
        dev.off()
    })
    
    box1 <- reactive({
      req(c(input$registry1, input$dis1, input$age_distribution_year1))
      filter_age_distribution_data(master_report,
                                   input$registry1, 
                                   input$dis1, 
                                   input$age_distribution_year1)
    })
  
    box2 <- reactive({
      req(c(input$registry2, input$dis2, input$age_distribution_year2))
      filter_age_distribution_data(master_report,
                                   input$registry2, 
                                   input$dis2, 
                                   input$age_distribution_year2)
    })

    output$boxplot1 <- renderPlot({
      create_diagnosis_boxplot(box1(), input$age_distribution_year1)
    })
  
    output$boxplot2 <- renderPlot({
      create_diagnosis_boxplot(box2(), input$age_distribution_year2)
    })
  
    ### Tab 5: Maps  
    output$countymap1 = renderTmap({
      create_openstreetmap(county_risk)
    })
    
    output$air_risk = renderTmap({
        create_air_risk_map(nj_tracts, nj_counties)
    })
   
    # BRF Leaflet Output  
    decision <- reactive({
      county_risk2 %>% pull(input$county_vars)
    })
    
    output$countymap2 = renderLeaflet({
        req(input$county_vars)
        create_leaflet(county_risk2, decision())
    })
    
    get_county_labels <- function(county_var) {
      lab_df <- county_risk2 %>% select(c("county", all_of(county_var)))
      st_geometry(lab_df) <- NULL
      lapply(seq(nrow(lab_df)), function(i) {
        paste0("<b>County</b>: ", lab_df[i, "county"], "<br>", 
               "<b>", county_var, "</b>: ", lab_df[i, county_var]) 
      })
    }
    
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
                    ),
                    popup = lapply(get_county_labels(input$county_vars), HTML)) %>% 
        addLegend("bottomright", 
                  pal = newpal,
                  values = decision(),
                  title = as.character(input$county_vars),
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0(cuts[-n], " &ndash; ", cuts[-1])
                  })
                  
    })
    
    # Air Pollution Leaflet Output
    decision2 <- reactive({
      nj_tracts %>% pull(input$air_risk2)
    })
    
    output$pollution = renderLeaflet({
      create_leaflet(nj_tracts)
    })
    
    get_air_risk_county_labels <- function() {
      lab_df <- nj_tracts
      st_geometry(lab_df) <- NULL
      lapply(seq(nrow(lab_df)), function(i) { 
        paste("<b>Census Tract:</b>", lab_df[i, "NAME"], "<br>",
              "<b>County:</b>", lab_df[i, "County"], "<br>",
              "<b>Population:</b>", lab_df[i, "Population"], "<br>",
              "<b>1,3-Butadiene:</b>", lab_df[i, "X1.3.Butadiene"], "<br>",
              "<b>Acetaldehyde:</b>", lab_df[i, "Acetaldehyde"], "<br>",
              "<b>Aniline:</b>", lab_df[i, "Aniline"], "<br>",
              "<b>Benzene:</b>", lab_df[i, "Benzene"], "<br>",
              "<b>Ethlyene Oxide:</b>", lab_df[i, "Ethylene.Oxide"], "<br>",
              "<b>Formaldehyde:</b>", lab_df[i, "Formaldehyde"], "<br>",
              "<b>Naphthalene:</b>", lab_df[i, "Naphthalene"])
      })
    }
    
    get_air_risk_site_labels <- function() {
      lab_df <- npl_sites
      st_geometry(lab_df) <- NULL
      lapply(seq(nrow(lab_df)), function(i) { 
        paste("<b>Site Name:</b>", lab_df[i, "SITE.NAME"], "<br>",
              "<b>Address:</b>", lab_df[i, "ADDRESS"], "<br>",
              "<b>Federal Facility (Y/N)</b>", lab_df[i, "FEDERAL.FACILITY"])
      })
    }
    
    get_air_risk_plant_labels <- function() {
      lab_df <- pp_sites
      st_geometry(lab_df) <- NULL
      lapply(seq(nrow(lab_df)), function(i) { 
        paste("<b>Plant Name:</b>", lab_df[i, "PLANT_NAME"], "<br>",
              "<b>Operator:</b>", lab_df[i, "X.1"], "<br>",
              "<b>City:</b>", lab_df[i, "CITY"], "<br>",
              "<b>Plant Type:</b>", lab_df[i, "PRIMSOURCE"], "<br>",
              "<b>Megawatts:</b>", lab_df[i, "TOTAL_MW"])
      })
    }
    
    observeEvent(input$air_risk2, {
      leafletProxy("pollution") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
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
                    popup = lapply(get_air_risk_county_labels(), HTML)) %>%
        addPolylines(data = nj_counties,
                    color = "black",
                    weight = 1,
                    fillOpacity = 0) %>%
        addCircleMarkers(data = npl_sites,
                         color = "blue",
                         stroke = FALSE,
                         fillOpacity = 0.5,
                         group = "NPL Superfund Sites",
                         popup = lapply(get_air_risk_site_labels(), HTML)) %>%
        addCircleMarkers(data = pp_sites,
                         color = "red",
                         stroke = FALSE,
                         fillOpacity = 0.5,
                         group = "Power Plant Sites",
                         radius = pp_sites$TOTAL_MW / 50,
                         popup = lapply(get_air_risk_plant_labels(), HTML)) %>%
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