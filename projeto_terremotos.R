install.packages("tidyverse")
install.packages("rnaturalearth")
install.packages("sf")
install.packages("rjson")
install.packages("flexdashboard")
install.packages("ggplot2")
install.packages("dplyr")

devtools::install_github("AndySouth/rnaturalearthhires")
install.packages("rnaturalearthhires")

library(flexdashboard)
library(rjson)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(tidyverse)

file_path <- "C:/Users/Gabriel/Desktop/trabalho R/dados/data.json"
data <- fromJSON(file = file_path)

cities <- c("CA", "Alaska", "Montana", "Washington", "Oklahoma", "Hawaii")

city_earthquakes <- lapply(cities, function(city) {
  data$features[sapply(data$features, function(x) 
    !is.null(x$properties$place) && grepl(city, x$properties$place, ignore.case = TRUE))
  ]
})

for (i in seq_along(cities)) {
  num_city_earthquakes <- length(city_earthquakes[[i]])
  cat('Número total de eventos em', cities[i], ':', num_city_earthquakes, '\n')
  
  magnitudes_city <- sapply(city_earthquakes[[i]], function(x) x$properties$mag)
  average_magnitude_city <- mean(magnitudes_city)
  cat('Magnitude média em', cities[i], ':', average_magnitude_city, '\n')
  
  max_magnitude_city_event <- city_earthquakes[[i]][[which.max(magnitudes_city)]]
  cat('Evento de maior magnitude em', cities[i], ':', max_magnitude_city_event$properties$mag, '\n')
  
  min_magnitude_city_event <- city_earthquakes[[i]][[which.min(magnitudes_city)]]
  cat('Evento de menor magnitude em', cities[i], ':', min_magnitude_city_event$properties$mag, '\n\n')
}

city_earthquakes <- lapply(cities, function(city) {
  data$features[sapply(data$features, function(x) 
    !is.null(x$properties$place) && grepl(city, x$properties$place, ignore.case = TRUE))
  ]
})

df_cidades <- data.frame(
  Cidade = c("CA", "Alaska", "Montana", "Washington", "Oklahoma", "Hawaii"),
  NumEventos = sapply(city_earthquakes, length),
  MagnitudeMedia = sapply(city_earthquakes, function(cidade) mean(sapply(cidade, function(x) x$properties$mag)))
)

cores_chamativas <- c("#FF6347", "#1E90FF", "#32CD32", "#FFD700", "#FF69B4", "#8A2BE2")

ggplot(df_cidades, aes(x = Cidade, y = NumEventos, fill = Cidade, label = NumEventos)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 3, color = "white") +
  labs(title = "Número total de eventos sísmicos por cidade", y = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.text.y = element_blank(), 
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = cores_chamativas)

ggplot(df_cidades, aes(x = Cidade, y = MagnitudeMedia, fill = Cidade, label = sprintf("%.2f", MagnitudeMedia))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 3, color = "white") +
  labs(title = "Magnitude média de eventos sísmicos por cidade", y = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.text.y = element_blank(),  
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = cores_chamativas)

