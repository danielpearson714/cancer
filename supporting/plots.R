# contains all plots

title_font <- list(
  family = "sans serif",
  size = 16,
  color = 'black')

get_annotations <- function(source_text, y_pos = -0.12) {
  list(x = 1, y = y_pos, text = source_text, showarrow = F, xref='paper', yref='paper',
      xanchor = 'right', yanchor = 'auto', xshift = 10, yshift = 0, font = list(size = 12, color = "black"))
}

add_plot_config <- function(plot) {
  # http://svgicons.sparkk.fr/
  icon_svg_path = "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"
  dl_button <- list(
      name = "Download data",
      icon = list(
          path = icon_svg_path,
          transform = "scale(0.84) translate(-1, -1)"
          ),
      click = htmlwidgets::JS("
            function(gd) {
              var text = '';
              console.log(gd.data);
              console.log(gd.data[0].x)
              for(var i = 0; i < gd.data.length; i++){
                if (gd.data[i].type == 'histogram') {
                  text += gd.data[i].name + '\\n';
                  for (var j = 0; j < gd.data[i].x.length; j++) {
                    text += gd.data[i].x[j] + '\\n';
                  }
                } else if (gd.data[i].type == 'box') {
                  var names = gd.data[i].name.split(';');
                  text += names[0] + ',' + names[1] + '\\n';
                  for (var j = 0; j < gd.data[i].x.length; j++) {
                    text += gd.data[i].x[j] + ',' + gd.data[i].y[j] + '\\n';
                  }
                } else if (gd.data[i].type == 'bar') {
                  var names = gd.data[i].name.split(';');
                  text += names[0] + ',' + gd.data[i].x + '\\n';
                  if (!names[1]) { names[1] = names[0]; }
                  text += names[1] + ',' + gd.data[i].y + '\\n';
                } else {
                  text += gd.data[i].name + ',' + gd.data[i].x + '\\n';
                  text += gd.data[i].name + ',' + gd.data[i].y + '\\n';
                }
              };
              var blob = new Blob([text], {type: 'text/csv'}); /* text/csv */
              var a = document.createElement('a');
              const object_URL = URL.createObjectURL(blob);
              a.href = object_URL;
              a.type = 'csv';
              a.download = 'data.csv';
              document.body.appendChild(a);
              a.click();
              URL.revokeObjectURL(object_URL);
            }
     ")
  )

  plot %>%
    config(displayModeBar = T, displaylogo = FALSE,
           modeBarButtonsToRemove = c("hoverCompareCartesian", "hoverClosestCartesian", "resetScale2d",
                                      "autoScale2d", "toggleSpikelines", "pan2d", "zoom2d", "select2d", "lasso2d"),
           modeBarButtonsToAdd = list(dl_button))
}

add_plot_properties <- function(plot, title, subtitle = '', xaxis_title = '', yaxis_title = '', width = NULL, height = NULL, source_text = "", source_ypos = -0.1, x_tick_angle = 0) {
  plot %>%
    layout(title = list(text = paste0(title,
                                      '<br>',
                                      '<sup>',
                                      subtitle,
                                      '</sup>'),
                        font = title_font,
                        y = 0.96),
           xaxis = list(title = xaxis_title,
                        tickvals = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                        tickangle = x_tick_angle),
           yaxis = list(title = yaxis_title),
           annotations = get_annotations(source_text, y_pos = source_ypos)) %>%
    add_plot_config()
}

create_histogram <- function(data, var, name = '', title = '', subtitle = '', xaxis_title = '', yaxis_title = '', source_text = '', color,
                             showlegend = TRUE, source_ypos = -0.1, bar_gap = 0.1, nbin = 100) {
  values <- data %>% pull(var)
  plot_ly(x = ~values, type = "histogram", name = name, marker = list(color = color), nbinsx = nbin) %>%
    add_plot_properties(title = title, subtitle = subtitle, xaxis_title = xaxis_title, yaxis_title = yaxis_title, source_text = source_text, source_ypos = source_ypos) %>%
    layout(bargap = bar_gap)
}

create_accrual_plot <- function(data, disease_site) {
  # p <- ggplot(data) +
  #    geom_histogram(aes(x = Age), fill = "firebrick3", color = "black", binwidth = 1) +
  #    scale_x_continuous("Patient Age at Enrollment", limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  #    scale_y_continuous(NULL) +
  #    #ggtitle(paste("Clinical Trials Enrollment (by Disease Site) - ", disease_site)) +
  #    labs(caption = "OnCore Subject Search: 1/5/2021 (Does not include studies where Disease Site is not captured)") +
  #    theme(plot.title = element_text(hjust = 0.5),
  #          axis.text = element_text(size = 16, color = "black"),
  #          axis.title = element_text(size = 17, color = "black"),
  #          panel.background = element_rect(fill = "aliceblue")) +
  #    theme_bw()
  # ggplotly(p) %>% 
  #   layout(title = paste("Clinical Trials Enrollment (by Disease Site) - ", disease_site))
  #plot_ly(data, y=y, x=x, histfunc='sum', type = "histogram")
  title <- ifelse(length(disease_site) == 1, paste("Clinical Trials Enrollment - ", disease_site), "Clinical Trials Enrollment (by Disease Site)")
  create_histogram(data, var = "Age", title = title, subtitle = paste("N =", data %>% n_distinct()),
                   xaxis_title = "Patient Age at Enrollment", yaxis_title = "Count",
                   source_text = "OnCore Subject Search: 1/5/2021 (Does not include studies where Disease Site is not captured)",
                   color = c("#cd2626"))
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