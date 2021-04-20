server <- function(input, output) {
    
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
    
    output$accrual <- renderPlot({
        create_accrual_plot(accrual_data(), input$disease_site)
    }, height = 600, width = 1000)
    
    output$n_value <- renderText({
      paste("n =", accrual_data() %>% n_distinct())
    })
    
    ### Tab 2: Biospecimen Tables
    output$brs <- renderReactable({
      create_biospecimen_table(brs)
    })
    
    output$brs2 <- renderReactable({
      create_biospecimen_table(brs2)
    })
    
    ### Tab 3: Cancer & Risk Factors
    output$cancer_risk <- renderPlot({
        create_cancer_risk_plot(dashboard_risk, input$x_axis, input$y_axis)
    })
    
    # risk table
    output$risk_react <- renderReactable({
      dashboard_risk %>% 
        select(county, input$x_axis, input$y_axis) %>% 
        create_risk_table()
    })
    
    output$county_radar <- renderPlot({
        input$county_select
        data <- dashboard_risk %>% 
          select(county, Prostate.Cancer, Breast.Cancer, Lung.Cancer, Colorectal.Cancer, Bladder.Cancer, Kidney.Cancer, Bladder.Cancer, Melanoma, NH.Lymphoma, Uterine.Cancer, Kidney.Cancer, Leukemia, Pancreatic.Cancer, Thyroid.Cancer) %>%
          mutate_at(vars(-county), rescale) %>% 
          filter(county %in% input$county_select)
      
        create_radar_plot(data)
    })

    ### Tab 4: Analytic Cases
    output$disease_site <- renderPlot({
        data <- master_report %>% 
            filter(Disease.Site != "", Gender %in% c("Male", "Female")) %>%
            group_by(RWJBH.Site, Disease.Site, Gender, Year) %>% 
            count() %>%
            filter(RWJBH.Site %in% input$rwj_site, Year %in% input$report_year)
        
        create_cases_plot(data)
    }, height = 800, width = 1200)
 
    registry_rwj_v2_plot <- reactive({
        df <- master_report %>% 
                filter(Gender %in% c("Male", "Female")) %>%
                filter(RWJBH.Site %in% input$rwj_site2, Year %in% input$report_year2, Gender %in% input$gender2, Age %inrange% input$age_range2, Race.Ethnicity %in% input$race)
        
        create_cases2_plot(df, input$rwj_site2, input$report_year2, input$gender2, input$race, input$age_range2)
    })
    
    output$disease_site2 <- renderPlot({ 
        registry_rwj_v2_plot()
      }, height = 1000, width = 1000)
  
    output$case_report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function(){paste("analytic_report.pdf", sep = '')},
      
      content = function(file){
        pdf(file, width = 12, height = 10)
        registry_rwj_v2_plot()
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
      create_diagnosis_boxplot(box1())
    }, height = 800, width = 1000)
  
    output$boxplot2 <- renderPlot({
      create_diagnosis_boxplot(box2())
    }, height = 800, width = 1000)
  
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
        create_leaflet(county_risk2, decision())
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
    
    # Air Pollution Leaflet Output
    decision2 <- reactive({
      nj_tracts %>% pull(input$air_risk2)
    })
    
    output$pollution = renderLeaflet({
      create_leaflet(nj_tracts)
    })
    
    observeEvent(input$air_risk2, {
      leafletProxy("pollution") %>%
        clearShapes() %>%
        clearMarkers() %>%
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
                                     "Federal Facility (Y/N):", npl_sites$FEDERAL.FACILITY)) %>%
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