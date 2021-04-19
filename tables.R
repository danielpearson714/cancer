# contains all tables

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

create_risk_table <- function(data) {
  reactable(data,
            pagination = FALSE,
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
              Liver.Cancer = create_coldef(col_value = dashboard_risk$Liver.Cancer, col_name = "Liver Cancer")
            )
        )
}

create_biospecimen_table <- function(data) {
    data %>%
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
                           columns = specimen_types)),
                
                bordered = TRUE,
                highlight = TRUE,
                striped = TRUE)
}