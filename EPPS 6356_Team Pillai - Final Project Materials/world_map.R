library(ggplot2)
library(dplyr)
library(maps)
library(tibble)


#Load Dataset

trade_data <- read.csv("Merged_redux.csv")
head(trade_data)

# Coordinates


region_mapping <- tibble(
  Region = c("North America", "North America", "North America", "South and Central America", "Europe & Eurasia", "Asia", "Asia",
             "South and Central America", "Asia", "Asia", "Africa", "Europe & Eurasia", "Africa", "Middle East",  "Europe & Eurasia", "Middle East"),
  Country = c("United States", "China", "Russia", "United States", "China", "Russia",
              "United States", "China", "Russia", "United States", "China", "Russia",
              "United States", "China", "Russia", "United States"),
  Partner = c("Afghanistan", "Belarus", "Bangladesh", "India", "Iran", "Bangladesh", "India", "Iran",
               "Iraq", "Israel", "Japan", "Myanmar", "Pakistan", "Qatar", "Saudi Arabia", "South Korea"),
  Latitude = c(37.0902, 56.130366, 23.634501, -14.235004, 51.165691, 35.8617, 20.5937,
               -1.8312, 23.6850, 30.3753,9.0820, 52.1326, -1.2864, 15.552727, 55.378051,23.885942),
  Longitude = c(-95.7129,  -106.346771, -102.552784, -51.92528, 10.451526, 104.1954, 78.96288,
                -78.1834, 90.3563, 69.3451, 8.6753, 5.2913,37.9062, 48.5164,-3.4359, 45.0792))

trade_data_coords <- trade_data %>%
  left_join(region_mapping, by = c("Supplier" = "Country")) %>%
  rename(Export_Lat = Latitude, Export_Lon = Longitude, Region_Export = Supplier) %>%
  left_join(region_mapping, by = c("Recipient" = "Partner")) %>%
  rename(Import_Lat = Latitude, Import_Lon = Longitude, Region_Import = Recipient)


head(trade_data_coords)

trade_data_clean = trade_data_coords %>%
  filter(!is.na(Region_Export), !is.na(Region_Import)) %>%
  mutate(Route = paste(Region_Export, "to", Region_Import)) %>%
  filter(!is.na(Export_Lon), !is.na(Export_Lat), !is.na(Import_Lon), !is.na(Import_Lat))  %>%
  filter(!(Export_Lon == Import_Lon & Export_Lat == Import_Lat))


head(trade_data_clean)


# create map
world <- map_data("world")

world_regions <- world %>%
  mutate(Region = case_when(
    region %in% c("USA", "Canada", "Mexico") ~ "North America",
    region %in% c("Brazil", "Argentina", "Ecuador") ~ "South and Central America",
    region %in% c("Germany", "Russia", "France", "Netherlands", "UK") ~ "Europe & Eurasia",
    region %in% c("China", "India", "Bangladesh", "Pakistan") ~ "Asia",
    region %in% c("Japan", "Australia") ~ "Asia Pacific",
    region %in% c("Nigeria", "South Africa", "Kenya") ~ "Africa",
    region %in% c("Saudi Arabia", "Yemen") ~ "Middle East",
    TRUE ~ "Other"
  ))
# Color palette for regions
region_colors <- c(
  "North America" = "mistyrose1",
  "South and Central America" = "tomato",
  "Europe & Eurasia" = "lightskyblue3",
  "Asia" = "antiquewhite",
  "Asia Pacific" = "sandybrown",
  "Middle East" = "pink",
  "Africa" = "aquamarine",
  "Other" = "gray90")

summary(trade_data_clean)
head(trade_data_clean)

# Create base map
ggplot() +
  geom_polygon(data = world_regions, aes(x = long, y = lat, group = group, fill = Region), color = "black") +
  geom_curve(data = trade_data_clean, aes(x = Export_Lon, y = Export_Lat,
                                          xend = Import_Lon, yend = Import_Lat, color = Route, size = Amount), 
             size = 0.3 ,curvature = 0.1, arrow = arrow(length = unit(0.1, "cm"))) +
  scale_fill_manual(values = region_colors, name = "Region") +  
  coord_fixed(1.3) +
  labs(title = "Routes of Arms Partners",
       x = "Longitude",
       y = "Latitude",
       subtitle = "A Global 'Network' View",
       caption = "Sources: WITS and SIPRI") +
  theme_economist() +
  theme(text = element_text(family = "Optima")) +
  theme(legend.position = "right")


# !NOTE!: The remainder of the script was unused in our project:


# create plotly map
install.packages("plotly")
library(plotly)
library(dplyr)
library(tidyr)

trade_data_clean = trade_data_coords %>%
  filter(!is.na(Region_Export), !is.na(Region_Import)) %>%
  mutate(Route = paste(Region_Export, "to", Region_Import)) %>%
  filter(!is.na(Export_Lon), !is.na(Export_Lat), !is.na(Import_Lon), !is.na(Import_Lat))  %>%
  filter(!(Export_Lon == Import_Lon & Export_Lat == Import_Lat))

head(trade_data_clean)


trade_data_plot <- trade_data_clean %>% 
  rowwise() %>%
  mutate(
    lon = list(c(Export_Lon, Import_Lon, NA)),
    lat = list(c(Export_Lat, Import_Lat, NA))
  ) %>%
  unnest(c(lon, lat))

View(trade_data_plot)

plot <- plot_geo() %>%
  add_trace(
    type = "scattergeo",
    mode = "lines",
    lon = ~lon,
    lat = ~lat,
    split = ~Route,
    line = list(width = 2.5, color = "steelblue"),
    color = ~Route,
    colors = "viridis",
    data = trade_data_plot
  ) %>%
  add_trace(
    type = "scattergeo",
    mode = "markers",
    lon = ~Export_Lon,
    lat = ~Export_Lat,
    text = ~paste("Country", Country),
    marker = list(size = 5, color = "hotpink2"),
    name = "Export"
  ) %>%
  add_trace(
    type = "scattergeo",
    mode = "markers",
    lon = ~Import_Lon,
    lat = ~Import_Lat,
    text = ~paste("Country", Partner.Name),
    marker = list(size = 5, color = "red"),
    name = "Import"
  ) %>%
  layout(
    title = "Interactive US and China Trade Route",
    geo = list(
      showland = TRUE,
      landcolor = "darkseagreen",
      showcoastlines = "black",
      projection = list(type = "equirectagular")
    )
  )

#show interactive map

plot



#animated interactive map

plot_animate <- plot_geo() %>%
  add_trace(
    type = "scattergeo",
    mode = "lines",
    lon = ~lon,
    lat = ~lat,
    split = ~Route,
    line = list(width = 2.5, color = "steelblue"),
    color = ~Route,
    colors = "viridis",
    frame = ~Year,
    data = trade_data_plot
  ) %>%
  add_trace(
    type = "scattergeo",
    mode = "markers",
    lon = ~Export_Lon,
    lat = ~Export_Lat,
    text = ~paste("Country", Country),
    marker = list(size = 5, color = "hotpink2"),
    name = "Export"
  ) %>%
  add_trace(
    type = "scattergeo",
    mode = "markers",
    lon = ~Import_Lon,
    lat = ~Import_Lat,
    text = ~paste("Country", Partner.Name),
    marker = list(size = 5, color = "red"),
    name = "Import"
  ) %>%
  layout(
    title = "Interactive US and China Trade Route",
    geo = list(
      showland = TRUE,
      landcolor = "darkseagreen4",
      showcoastlines = "black",
      projection = list(type = "equirectagular")
    ),
    updatemenus = list(
      list(
        type = "buttons",
        showactive = FALSE,
        buttons = list(
          list(label = "Play",
               method = "animate",
               args = list(NULL, list(frame = list(duration = 700, redraw = TRUE), fromcurrent = TRUE))),
          list(label = "Play",
               method = "animate",
               args = list(NULL, list(mode = "immediate", frame = list(duration = 0))))
        )
      )
    )
  )

plot_animate

#show interactive map

plot

#animated plotly

library(plotly)
library(dplyr)
library(tidyr)
library(RColorBrewer)



trade_data_clean = trade_data_coords %>%
  filter(!is.na(Region_Export), !is.na(Region_Import)) %>%
  mutate(Route = paste(Region_Export, "to", Region_Import)) %>%
  filter(!is.na(Export_Lon), !is.na(Export_Lat), !is.na(Import_Lon), !is.na(Import_Lat))  %>%
  filter(!(Export_Lon == Import_Lon & Export_Lat == Import_Lat))

head(trade_data_clean)


inter_trade <- trade_data_clean %>% 
  rowwise() %>%
  mutate(
    interpolated = list(
      tibble(
    lon = seq(Export_Lon, Import_Lon, length.out = 25),
    lat = seq(Export_Lat, Import_Lat, length.out = 25),
    step = 1:25
      )
    )
    )%>%
  unnest(interpolated)

head(inter_trade)
unique(inter_trade$step)
custom_colors <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique(inter_trade$Indicator)))



animated_plot <- plot_ly() %>%
  add_trace(
    type = "scattergeo",
    mode = "lines",
    lon = ~lon,
    lat = ~lat,
    frame = ~Year,
    color = ~Indicator,
    line = list(width = 5),
    data = inter_trade) %>%
  layout(
    title = "Animated US, Russia, and China Trade Route",
    geo = list(
      showland = TRUE,
      landcolor = "tan",
      showcoastlines = "black",
      projection = list(type = "equirectagular")
    ),
    updatemenus = list(
      list(
        type = "buttons",
        showactive = FALSE,
        buttons = list(
          list(label = "Play",
               method = "animate",
               args = list(NULL, list(frame = list(duration = 700, redraw = TRUE), fromcurrent = TRUE))),
          list(label = "Play",
               method = "animate",
               args = list(NULL, list(mode = "immediate", frame = list(duration = 0))))
        )
      )
    )
  )

#show interactive map

animated_plot




