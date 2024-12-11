library(tidyverse)
library(sf)
library(mapview)

match_nome <- read_csv("banco_dados/match_nome.csv")

trechos <- st_read("banco_dados/trechos.gpkg") |> 
  st_transform("epsg:31983") |> 
  filter(tipo_via != "service") |> 
  select(id_osm, geometria_trecho = geom)

sinistros <- read_csv("banco_dados/sinistros.csv") |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform("epsg:31983") |> 
  select(id_sinistro, geometria_ponto = geometry)

distancias <- match_nome |> 
  separate_longer_delim(id_osm, delim = ";") |> 
  left_join(sinistros) |> 
  left_join(trechos) |>
  mutate(distancia_geografica = st_distance(geometria_trecho, geometria_ponto, by_element = TRUE))

match_geografico <- distancias |> 
  select(-geometria_ponto, -geometria_trecho) |> 
  arrange(id_sinistro, distancia_geografica) |> 
  group_by(id_sinistro) |> 
  top_n(-1) |> 
  ungroup()

match_geografico |> 
  write_csv("banco_dados/match_geografico.csv")


match_geografico |> 
  mutate(quantil = ntile(distancia_geografica, 100)) |>
  group_by(quantil) |> 
  summarize(distancia_geografica = median(as.numeric(distancia_geografica)),
            distancia_nome = mean(distancia_nome)) |> 
  ggplot(aes(x = quantil, y = as.numeric(distancia_geografica))) +
  geom_line(aes(colour = distancia_nome), lwd = 3) +
  scale_y_continuous(transform = "log10", labels = scales::number, breaks = 10^(0:6)) +
  scale_colour_gradient(low = "white", high = "darkred", limits = c(0, .2)) +
  theme_minimal()

match_geografico |> 
  ggplot(aes(y = as.numeric(distancia_geografica))) + 
  geom_histogram(bins = 50) +
  scale_y_continuous(transform = "log10", labels = scales::number, breaks = 10^(0:6)) +
  facet_wrap(~distancia_nome) +
  theme_minimal()

match_geografico |> 
  mutate(quantil = ntile(distancia_geografica, 100)) |> 
  ggplot(aes(y = as.numeric(distancia_geografica))) + 
  geom_histogram(aes(after_stat(density)), bins = 50) +
  scale_y_continuous(transform = "log10", labels = scales::number, breaks = 10^(0:6)) +
  facet_wrap(~distancia_nome) +
  theme_minimal()

nomes <- read_csv("banco_dados/sinistros.csv")
nomes.osm <- st_read("banco_dados/trechos.gpkg")
match_geografico |> 
  filter(distancia_geografica > 1000) |> 
  left_join(nomes) |>
  View()

nomes |> 
  filter(id_sinistro == 39714) |> 
  View()
  
match_nome |> 
  filter(id_sinistro == 39714) |> 
  separate_longer_delim(id_osm, delim = ";") |> 
  left_join(nomes.osm) |> 
  View()

tibble(street="78, AVSUMARE",
        city="SÃO PAULO",
        county="SÃO PAULO",
        state="SÃO PAULO",
        country="BRAZIL") |> 
  tidygeocoder::geocode(street=street,
                        city=city,
                        county=county,
                        state=state,
                        country=country,
                        method = 'osm', lat = nova_latitude , long = nova_longitude)
