# contains all plots

create_accrual_plot <- function(data, disease_site) {
  ggplot(data) +
     geom_histogram(aes(x = Age), fill = "firebrick3", color = "black", binwidth = 1) +
     scale_x_continuous("Patient Age at Enrollment", limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
     scale_y_continuous(NULL) +
     ggtitle(paste("Clinical Trials Enrollment (by Disease Site) - ", disease_site)) +
     labs(caption = "OnCore Subject Search: 1/5/2021 (Does not include studies where Disease Site is not captured)") +
     theme(plot.title = element_text(size = 20),
           axis.text = element_text(size = 16, color = "black"),
           axis.title = element_text(size = 17, color = "black"),
           panel.background = element_rect(fill = "aliceblue"))
}

create_cancer_risk_plot <- function(data, x_val, y_val) {
  ggplot(data = data, aes_string(x = x_val, y = y_val)) +
         geom_point(shape = 21, fill = "firebrick3", color = "black", alpha = 0.85, size = 7) +
         geom_label_repel(aes(label = county), size = 3) +
         ggtitle("Cancer Incidence Rates and Behavioral Risk Factors") +
         theme(plot.title = element_text(size = 20),
               axis.text = element_text(size = 16, color = "black"),
               axis.title = element_text(size = 17, color = "black"),
               panel.background = element_rect(fill = "aliceblue"))
}

create_diagnosis_boxplot <- function(data) {
  data %>% 
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
}

create_radar_plot <- function(data) {
  ggradar(data) + 
    theme(plot.title = element_text("Top 12 Cancers in New Jersey - County Incidence Relative to State"))
}

create_cases_plot <- function(data) {
  ggplot(data, aes(x = reorder_within(Disease.Site, n, Gender), y = n)) +
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
}

create_cases2_plot <- function(data, rwj_site, report_year, gender, race, age_range) {
  ggplot(data, mapping = aes(x = fct_rev(fct_infreq(Disease.Site)))) +
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
         ggtitle(paste("Analytic Cases at RWJBH Sites -", rwj_site), subtitle = paste(c("[Year:", report_year, "] - [Gender:", gender, "] - [Race/Ethnicity:", race, "] - [Age range:", age_range, "]"), collapse = " ", sep = "")) +
         labs(caption = "RWJBH Tumor Registry Reports, 2019-2020")
}

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

create_openstreetmap <- function(data) {
  tm_basemap("OpenStreetMap") +
       tm_shape(data)+ 
            tm_borders("black", lwd = 0.9, alpha = 0.9) +
            tm_fill(col = c("Obese", "Current.Smoker", "Binge.Drinking", "Mammography"), alpha = 0.7, palette = "Reds", style = "quantile", id = "NAMELSAD", 
                    popup.vars = c("county", "Obese", "Current.Smoker", "Binge.Drinking", "Mammography")) +
            tm_facets(as.layers = TRUE)
}

create_air_risk_map <- function(data, counties) {
  tm_shape(data) + 
      tm_borders("black", lwd = 0.5, alpha = 0.9) +
      tm_fill(col = c("Acetaldehyde", "Benzene", "Formaldehyde", "Naphthalene"), alpha = 0.7, palette = "-plasma", style = "quantile", id = "NAMELSAD10", 
              popup.vars = c("Population", "County", "Acetaldehyde", "Benzene", "Formaldehyde", "Naphthalene")) +
      tm_facets(as.layers = TRUE) +
      tm_shape(counties) +
      tm_borders("black", lwd = 0.85, alpha = 0.9) 
}