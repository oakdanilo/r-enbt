## aula 08
#pacotes necessários
library(rgdal)
library(rgeos)
library(raster)

#lendo shapefile
westeros <- readOGR("./data/GoTRelease/political.shp", encoding = "UTF-8")

#plotando - aqui pode plotar com argumentos secundários tbm

plot(westeros, las = 1, axes = T)
abline(h = 0, lty = 2, col = "tomato") #plotando a linha do 'equador'

#plotando só os territórios dos starks
stark <- westeros[westeros$ClaimedBy == "Stark",]
plot(stark, axes = T, las = 1)

#criando buffer - estudar isso pra mapa da diss
pontos <- spsample(stark, 10, 'random')
plot(stark, axes = T)
points(pontos, pch = "+", col = "tomato", cex = 1.5)
pontos.buffer <- buffer(pontos, width = 200000, dissolve = TRUE) #pontos por metros
plot(stark, axes = T)
plot(pontos.buffer, add = T, col = "navy") #troquei de cor
points(pontos, col = 'red', pch = 16)

#buffer do mapa
stark.buffer <- buffer(stark, width = 2, dissolve = TRUE)
plot(stark.buffer, col = "grey80", axes = T)


#adicionando nova coluna id
westeros$regiao <- c(rep(1:3, each = 4))
westeros

#uso da fx aggregate
westeros_contorno = aggregate(westeros)
plot(westeros_contorno, axes = T)
plot(stark, add = T, col = "lightblue")
new_westeros = aggregate(westeros, by = "regiao")
plot(westeros, axes = T, col = terrain.colors(12))
#usando só 4 cores desse pck
plot(new_westeros, axes = T, col = terrain.colors(4))

#a função writeOGR só exporta objetos do formato SpatialPointsDataFrame, SpatialLinesDataFrame ou SpatialPolygonsDataFrame object
writeOGR(
  westeros_contorno, #nome do objeto a ser salvo
  dsn = "./data/meushape", #diretorio a serem salvos os resultados
  layer = "westeros_contorno", #nome do arquivo
  driver = "ESRI Shapefile" #formato pretendido para exportação
)

#criando um raster em cima do shp
westeros_raster <- raster(westeros_contorno, res = 0.08)
westeros_raster <- rasterize(westeros_contorno, westeros_raster) #deixando com o mesmo extent
plot(westeros_raster)

var1 <- raster("./data/vars/var_1.tif")
var1
plot(var1)

#vários raster usando a fx stack
lista <- list.files("./data", pattern = "tif$", full.names = T)
vars <- stack(lista)
plot(vars)

#salvando
writeRaster(var1, "output.tif")

#fx possíveis pra raster
media <- mean(vars)
plot(media)


#manipulação de raster usando o shp

westeros <- readOGR("./data/GoTRelease/political.shp", encoding = "UTF-8")
stark <- westeros[westeros$ClaimedBy == "Stark",]
plot(westeros, axes = T, las = 1)
plot(stark, add = T, col = "tomato") #passos anteriores

plot(var1)
plot(westeros, add = T)

#cortando o raster
var1_croped <- crop(var1, stark)
var1_croped
plot(var1_croped)
plot(stark, add = T)

#outra possibilidade

var1_masked = mask(var1, stark)
var1_masked
var1.masked2 = mask(crop(var1,stark), stark)
var1.masked2
plot(var1.masked2)
plot(stark, add = T)


var1.aggregated = aggregate(var1, fact = 5, fun = "mean") #unindos rasters pela média e transformando em células de 5
var1.aggregated
plot(var1.aggregated)
