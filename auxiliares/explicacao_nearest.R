library(sf)

linha <- st_linestring(matrix(c(-1,1,0,0), 2))
ponto <- st_point(c(0,1))

plot(c(linha, ponto, st_cast(linha, "MULTIPOINT")))

st_distance(linha, ponto)
st_distance(st_cast(linha, "MULTIPOINT"), ponto)
sqrt(2)

quadrado <- st_polygon(list(
  matrix(c(0,1,
           0,-1,
           -2,-1,
           -2,1,
           0,1), 
         ncol = 2, byrow = T)))

triangulo <- st_polygon(list(
  matrix(c(1,0,
           2,-1,
           2,1,
           1,0), 
         ncol = 2, byrow = T)))

plot(c(quadrado, st_cast(quadrado, "MULTIPOINT"),
       triangulo, st_cast(triangulo, "MULTIPOINT")))

st_distance(quadrado, triangulo)
st_distance(st_cast(quadrado, "MULTIPOINT"), st_cast(triangulo, "MULTIPOINT"))
