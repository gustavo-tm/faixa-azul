library(sf)

linha <- st_linestring(matrix(c(-1,1,0,0), 2))
ponto <- st_point(c(0,1))

plot(c(linha, ponto, st_cast(linha, "MULTIPOINT")))

st_distance(linha, ponto)
st_distance(st_cast(linha, "MULTIPOINT"), ponto)
sqrt(2)
