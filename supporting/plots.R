# contains all plots

title_font <- list(
  family = "sans serif",
  size = 16,
  color = 'black')

get_annotations <- function(source_text, x_pos = 1, y_pos = -0.16) {
  list(x = x_pos, y = y_pos, text = source_text, showarrow = F, xref='paper', yref='paper', xanchor = 'right', 
       yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9, color = "black"))
}

add_plot_config <- function(plot) {
  plot %>%
    config(displayModeBar = T, displaylogo = FALSE, collaborate = FALSE,
           modeBarButtonsToRemove = c("hoverCompareCartesian", "hoverClosestCartesian", "resetScale2d", "autoScale2d", 
                                      "toggleSpikelines", "pan2d", "zoom2d", "select2d", "lasso2d"))
}

# to remove warning at the console
create_plotly_output <- function(plot) {
  plot$elementId <- NULL
  plot
}

add_plot_properties <- function(plot, title, subtitle = '', xaxis_title = '', yaxis_title = '', width = NULL, height = NULL, 
                                source_text = "", source_xpos = 1, source_ypos = -0.16, x_tick_angle = 0, log_scale = FALSE) {
  yaxis_type <- ifelse(isTRUE(log_scale), "log", "linear")
  plot %>%
    layout(title = paste0(title,
                          '<br>',
                          '<sup>',
                          subtitle,
                          '</sup>'),
           titlefont = title_font,
           xaxis = list(title = xaxis_title,
                        tickvals = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                        tickangle = x_tick_angle),
           yaxis = list(type = yaxis_type,
                        title = yaxis_title,
                        rangemode = "tozero"),
           margin = list(t=40, b=100),
           annotations = get_annotations(source_text, x_pos = source_xpos, y_pos = source_ypos)) %>%
    add_plot_config() %>% 
    create_plotly_output()
}

create_histogram <- function(data, var, title, subtitle, name = '', xaxis_title = '', yaxis_title = '', source_text = '', color,
                             showlegend = TRUE, source_xpos = 1, source_ypos = -0.16, bar_gap = 0.1, nbin = 100,  log_scale = FALSE) {
  plot_ly(data, x = as.formula(paste0('~', var)), type = "histogram", name = name, marker = list(color = color), nbinsx = nbin) %>%
    add_plot_properties(title = title, subtitle = subtitle, xaxis_title = xaxis_title, yaxis_title = yaxis_title, 
                        source_text = source_text, source_xpos = source_xpos, source_ypos = source_ypos, log_scale = log_scale) %>%
    layout(bargap = bar_gap)
}

create_scatter_plot <- function(data, x_val, y_val, title, subtitle = '', xaxis_title = x_val, yaxis_title = y_val, color = c("#cd2626")) {
  plot_ly(data = data, x = as.formula(paste0('~', x_val)), y = as.formula(paste0('~', y_val)), 
          type = "scatter", mode = "markers", marker = list(color = color, size = 20, line = list(color = 'black', width = 1)),
          text = ~paste("County: ", county)) %>%
    layout(title = title,
           titlefont = title_font,
           margin = list(t=40)) %>% 
    add_plot_config() %>%
    create_plotly_output()
}

create_accrual_plot <- function(data, disease_site, log_scale) {
  title <- ifelse(length(disease_site) == 1, paste("Clinical Trials Enrollment - ", disease_site), "Clinical Trials Enrollment (by Disease Site)")
  create_histogram(data, var = "Age", title = title, subtitle = paste("N =", data %>% n_distinct()),
                   xaxis_title = "Patient Age at Enrollment", yaxis_title = "Count",
                   source_text = "OnCore Subject Search: 1/5/2021 (Does not include studies where Disease Site is not captured)",
                   color = c("#cd2626"), log_scale = log_scale)
}

create_cancer_risk_plot <- function(data, x_val, y_val) {
  create_scatter_plot(data, x_val, y_val, title = "Cancer Incidence Rates and Behavioral Risk Factors")
}

create_diagnosis_boxplot <- function(data) {
  data %>% 
    ggplot() +
    geom_boxplot(aes(x = factor(Race.Ethnicity, levels = c("White", "Black", "Hispanic/Latino", "Asian")), y = Age, fill = Race.Ethnicity, group = Race.Ethnicity), shape = 21, position = "dodge", color = "black", size = 1, outlier.shape = 21, outlier.size = 3,  inherit.aes = TRUE, fatten = 1) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
    scale_fill_viridis(discrete = TRUE, option = "C", begin = 0, end = 0.8) +
    labs(x = "Race/Ethnicity", y = "Age at Diagnosis", caption = "RWJBH Tumor Registry Reports (2019 and 2020 Q1)") +
    ggtitle("Age at Diagnosis by Race/Ethnicity" , subtitle = "RWJBarnabas Analytic Cases (2019 and Q1 2020)") +
    theme(
      panel.background = element_rect(fill = "white", colour = "white", size = 2, linetype = "solid"),
      panel.grid.major = element_line(size = 0.10, linetype = 'solid', colour = "black"),
      panel.grid.minor = element_line(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 16),
      strip.background = element_rect(color = "black"),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "none",
      plot.subtitle = element_text(hjust = 0.5),
      plot.title = element_text(hjust = 0.5, size = 18, face = c("bold")))
}

create_radar_plot <- function(data) {
  ggradar(data) + 
    theme(plot.title = element_text("Top 12 Cancers in New Jersey - County Incidence Relative to State"))
}

cases_theme <- function() {
  theme(
    panel.background = element_rect(fill = "white", colour = "white", size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.10, linetype = 'solid', colour = "black"), 
    panel.grid.minor = element_line(),
    strip.text = element_text(size = 14, face = "bold", color = "white"),
    axis.text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.background = element_rect(fill = "dodgerblue4", color = "White"),
    axis.title = element_blank(),
    legend.position = "none")
}

create_cases_plot <- function(data) {
  ggplot(data, aes(x = reorder_within(Disease.Site, n, Gender), y = n)) +
         facet_wrap(~Gender, scales = "free_y") +
         geom_col(aes(), color = "black", fill = "firebrick3", position = "stack") +
         coord_flip() +
         scale_x_reordered() +
         scale_y_continuous(limits = c(0, 450), breaks = seq(0, 450, 50), expand = c(0, 0)) +
         stat_summary(fun = sum, aes(label = ..y.., group = Disease.Site), geom = "text", hjust = -0.3) +
         scale_fill_gradient(low = "firebrick4", high = "firebrick2") +
         ggtitle("Analytic Cases at RWJBH Sites") +
         labs(caption = "RWJBH Tumor Registry Reports, 2019-2020") +
         cases_theme()
}

create_cases2_plot <- function(data, rwj_site, report_year, gender, race, age_range) {
  ggplot(data, mapping = aes(x = fct_rev(fct_infreq(Disease.Site)))) +
         geom_histogram(stat = "count", color = "black", fill = "firebrick3") +
         coord_flip() +
         stat_count(binwidth=1, geom="text", aes(label=..count..), hjust =-0.3) +
         scale_y_continuous(expand = c(0,0)) +
         scale_fill_gradient() +
         ggtitle(paste("Analytic Cases at RWJBH Sites -", rwj_site), subtitle = paste(c("[Year:", report_year, "] - [Gender:", gender, "] - [Race/Ethnicity:", race, "] - [Age range:", age_range, "]"), collapse = " ", sep = "")) +
         labs(caption = "RWJBH Tumor Registry Reports, 2019-2020") +
         cases_theme()
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
       tm_shape(data) + 
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