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
        ggplot(accrual_data()) +
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
      paste("n =", accrual_data() %>% n_distinct())
    })
    
  ### Tab 2: Biospecimen Tables
  output$brs <- renderReactable({
      brs %>% 
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
                    Blood = create_coldef_sum(),
                    `Outside Paraffin Blocks` = create_coldef_sum(),
                    Tissue = create_coldef_sum("Tumor Tissue"),
                    Urine = create_coldef_sum(),
                    `Glass Slides` = create_coldef_sum(),
                    `Body Fluid` = create_coldef_sum(),
                    `Bone Marrow` = create_coldef_sum(),
                    Biomarker = create_coldef_sum()
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
      })
      
    
    output$brs2 <- renderReactable({
      brs2 %>% 
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
                    Blood = create_coldef_sum(),
                    `Outside Paraffin Blocks` = create_coldef_sum(),
                    Tissue = create_coldef_sum("Tumor Tissue"),
                    Urine = create_coldef_sum(),
                    `Glass Slides` = create_coldef_sum(),
                    `Body Fluid` = create_coldef_sum(),
                    `Bone Marrow` = create_coldef_sum(),
                    Biomarker = create_coldef_sum()
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
      })
    
    ### Tab 3: Cancer & Risk Factors
    output$cancer_risk <- renderPlot({
        ggplot(data = dashboard_risk, 
               aes_string(x = input$x_axis, y = input$y_axis)) +
            geom_point(shape = 21, fill = "firebrick3", color = "black", alpha = 0.85, size = 7) +
            geom_label_repel(aes(label = county), size = 3) +
            ggtitle("Cancer Incidence Rates and Behavioral Risk Factors") +
            theme(plot.title = element_text(size = 20),
                  axis.text = element_text(size = 16, color = "black"),
                  axis.title = element_text(size = 17, color = "black"),
                  panel.background = element_rect(fill = "aliceblue"))
    })
    
    # TODO create function to create table
    output$risk_react <- renderReactable({
      dashboard_risk %>% 
        select(county, input$x_axis, input$y_axis) %>% 
        reactable(pagination = FALSE,
                  compact = TRUE,
                  theme = reactableTheme(borderWidth = "4px"),
                  defaultColDef = colDef(align = "center"),
                  columns = list(
                    county = colDef(name = "County"),
                    Obese = create_coldef(col_value = dashboard_risk$Obese, col_name = "Obese"),
                    Overall.Cancer.Incidence = create_coldef(col_value = dashboard_risk$Overall.Cancer.Incidence, col_name = "Overall Incidence"),
                    Overall.Cancer.Mortality = create_coldef(col_value = dashboard_risk$Overall.Cancer.Mortality, col_name = "Overall Mortality"),
                    Current.Smoker = create_coldef(col_value = dashboard_risk$Current.Smoker),
                    Binge.Drinking = create_coldef(col_value = dashboard_risk$Binge.Drinking),
                    Mammography = create_coldef(col_value = dashboard_risk$Mammography),
                    Pap.Smear = create_coldef(col_value = dashboard_risk$Pap.Smear, col_name = "Pap Smear"),
                    Colorectal = create_coldef(col_value = dashboard_risk$Colorectal, col_name = "CRC Screen"),
                    PSA.Test = create_coldef(col_value = dashboard_risk$PSA.Test, col_name = "PSA Test"),
                    Breast.Cancer = create_coldef(col_value = dashboard_risk$Breast.Cancer, col_name = "Breast Cancer"),
                    Prostate.Cancer = create_coldef(col_value = dashboard_risk$Prostate.Cancer),
                    Lung.Cancer = create_coldef(col_value = dashboard_risk$Lung.Cancer),
                    Bladder.Cancer = create_coldef(col_value = dashboard_risk$Bladder.Cancer),
                    Uterine.Cancer = create_coldef(col_value = dashboard_risk$Uterine.Cancer),
                    Thyroid.Cancer = create_coldef(col_value = dashboard_risk$Thyroid.Cancer),
                    Kidney.Cancer = create_coldef(col_value = dashboard_risk$Kidney.Cancer),
                    Melanoma = create_coldef(col_value = dashboard_risk$Melanoma),
                    Esophageal.Cancer = create_coldef(col_value = dashboard_risk$Esophageal.Cancer),
                    Liver.Cancer = create_coldef(col_value = dashboard_risk$Liver.Cancer, col_name = "Liver Cancer"),
                  )
        )
    })
    
    dash_risk <- reactive({
        dashboard_risk %>% 
          select(county, Prostate.Cancer, Breast.Cancer, Lung.Cancer, Colorectal.Cancer, Bladder.Cancer, Kidney.Cancer, Bladder.Cancer, Melanoma, NH.Lymphoma, Uterine.Cancer, Kidney.Cancer, Leukemia, Pancreatic.Cancer, Thyroid.Cancer) %>%
          mutate_at(vars(-county), rescale) %>% 
          filter(county %in% input$county_select)
      })
    
    output$county_radar <- renderPlot({
        input$county_select
        ggradar(dash_risk()) +
                theme(plot.title = element_text("Top 12 Cancers in New Jersey - County Incidence Relative to State"))
    })

    ### Tab 4: Analytic Cases
    registry_rwj <- reactive({
        master_report %>% 
            filter(Disease.Site != "", Gender %in% c("Male", "Female")) %>%
            group_by(RWJBH.Site, Disease.Site, Gender, Year) %>% 
            count() %>%
            filter(RWJBH.Site %in% input$rwj_site, Year %in% input$report_year)
    })
    
    output$disease_site <- renderPlot({
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
 
    registry_rwj_v2_plot <- reactive({
        df <- master_report %>% 
                filter(Gender %in% c("Male", "Female")) %>%
                filter(RWJBH.Site %in% input$rwj_site2, Year %in% input$report_year2, Gender %in% input$gender2, Age %inrange% input$age_range2, Race.Ethnicity %in% input$race)
        
        ggplot(df, mapping = aes(x = fct_rev(fct_infreq(Disease.Site)))) +
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
        tm_shape(nj_tracts) + 
            tm_borders("black", lwd = 0.5, alpha = 0.9) +
            tm_fill(col = c("Acetaldehyde", "Benzene", "Formaldehyde", "Naphthalene"), alpha = 0.7, palette = "-plasma", style = "quantile", id = "NAMELSAD10", popup.vars = c("Population", "County", "Acetaldehyde", "Benzene", "Formaldehyde", "Naphthalene")) +
            tm_facets(as.layers = TRUE) +
            tm_shape(nj_counties) +
            tm_borders("black", lwd = 0.85, alpha = 0.9) 
    })
   
    # BRF Leaflet Output  
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
    
    output$countymap2 = renderLeaflet({
        req(input$county_vars)
        
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
      if(input$air_risk2 == "X1.3.Butadiene") return(nj_tracts$X1.3.Butadiene)
      if(input$air_risk2 == "Acetaldehyde") return(nj_tracts$Acetaldehyde)
      if(input$air_risk2 == "Aniline") return(nj_tracts$Aniline)
      if(input$air_risk2 == "Benzene") return(nj_tracts$Benzene)
      if(input$air_risk2 == "Ethylene.Oxide") return(nj_tracts$Ethylene.Oxide)
      if(input$air_risk2 == "Formaldehyde") return(nj_tracts$Formaldehyde)
      if(input$air_risk2 == "Naphthalene") return(nj_tracts$Naphthalene)
    })
    
    output$pollution = renderLeaflet({
      req(input$air_risk2)

      create_leaflet(nj_tracts)
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