
#...................................................................... #
#...................... Classificacao de imagens ...................... #
#..........................Usando Indices de .......................... #
#............................. Vegetacao .............................. #
#...................................................................... #
#o xenhenhem da viola e o piripimpim da sanfona
# adoro!!!!


# 1 - biblioteca
library(raster)
library(rgdal)
library(rpart) # classificacao por arvore de decisao
library(maptools)
library(dplyr)
library(randomForest)
library(ithir)

# 2 - Areas Mapeadas
# Shapefile polilinha
samples_area = readOGR(choose.files()) #D:\GIS\Guarda_Mor_2019 # Samples_landuse
samples_crs = crs(samples_area)
table_saples = samples_area@data
samples_area = unionSpatialPolygons(samples_area, IDs = table_saples$MC_ID)

# 3 - Carregamento dos indices nos respectivos meses de interesse

# A analise ira contemplar os indices de vegetacao de todas os periodos 
# do ano. Para tal sera utilizado a cada 3 meses uma imagem. Para a clas-
# sificacao inicial sera utilizado os dados de 2019 contemplando oss dados
# de 2018. Assim as imagens tem como preferencia os meses de MARCO, AGOSTO e
# DEZEMBRO.


# Diretorio imagens cortadas e preprocessadas
dir_landsat_process = choose.dir() #D:\GIS\Guarda_Mor_2019\Landsat_process\classification

# Lista de diretorios das imagens
dirs_imagens = list.dirs(dir_landsat_process)

# criando o raster de saida
indice_vegetacao = raster() # vazio

for(k in 2:length(dirs_imagens)){
  arquivos = list.files(dirs_imagens[k])
  
  # Procurar a data
  for(n in 1:length(arquivos)){
    tipo_arquivo =unlist(strsplit(arquivos[n],split = "_"))
    tipo_arquivo = tipo_arquivo[length(tipo_arquivo)]
    if(tipo_arquivo=="MTL.txt"){
      tabela = read.table(paste0(dirs_imagens[k],"\\",arquivos[n]),fill =NA) # le o arquivo txt
      data = as.character(tabela[24,3]) # pega a data
      data = unlist(strsplit(data,split = "-")) # quebra em a m d
      data = data[2] # pega o mes
    }
  } # fim da selecao da data

  for(n in 1: length(arquivos)){
    tipo_arquivo =unlist(strsplit(arquivos[n],split = "_"))
    tipo_arquivo = tipo_arquivo[length(tipo_arquivo)]
    if(tipo_arquivo=="dvi.tif"){
      dvi_map = raster(paste0(dirs_imagens[k],"\\",arquivos[n]))
      dvi_map = ratify(dvi_map) # Cria tabela de atributos no raster
    }else if(tipo_arquivo == "evi.tif"){
      evi_map = raster(paste0(dirs_imagens[k],"\\",arquivos[n]))
      evi_map = ratify(evi_map)
    }else if(tipo_arquivo == "ndvi.tif"){
      ndvi_map = raster(paste0(dirs_imagens[k],"\\",arquivos[n]))
      ndvi_map = ratify(ndvi_map)
    }else if(tipo_arquivo == "nrvi.tif"){
      nrvi_map = raster(paste0(dirs_imagens[k],"\\",arquivos[n]))
      nrvi_map = ratify(nrvi_map)
    }else if(tipo_arquivo == "savi.tif"){
      savi_map = raster(paste0(dirs_imagens[k],"\\",arquivos[n]))
      savi_map = ratify(savi_map)
    }
  } # fim selecao de raster
  
  #criacao do raster de saida
  iv_mes = stack(dvi_map,evi_map,ndvi_map,nrvi_map,savi_map)
  nomes = paste(data, c("dvi_map","evi_map","ndvi_map","nrvi_map","savi_map"))
  names(iv_mes) = nomes
  indice_vegetacao = stack(indice_vegetacao,iv_mes)
  
  }# selecao do multiraster

# 4 - Classificacao

# 4.1 - Amostragem
# Extrair valor dos indices para cada uso
samples_area = spTransform(samples_area, crs(indice_vegetacao))
plot(indice_vegetacao$X12.ndvi_map)

sample_values = extract(indice_vegetacao, samples_area, df = T) # demoraaa!

# 4.2 - Estatistica basica da amostragem
# cria o arquivo 
stat_sample = data.frame(Class_id = c(1,2,3,4,5)) # classes do poligono
nome = names(sample_values) # extrai o nome de caa mapa
aux = matrix(nrow = 5,ncol = 2) # matriz vazia para adicionar espaco na matriz principal

for(j in 2:length(nome)){
  stat_sample = cbind(stat_sample,aux) # adiciona espaco
  colnames(stat_sample)[(2*j-2):(2*j-1)] = paste(c("m_", "dp_"),nome[j]) 
  # Coloca o nome em duas colunas m para a media dp o coeficiente de variacao do desvio padrao
  
  for(i in 1:nrow(stat_sample)){
      #Media
      media = mean(sample_values[sample_values$ID==i,j])
      stat_sample[i,(2*j-2)] = media
      
      # Cv
      coe_var = sd(sample_values[sample_values$ID==i,j])/media
      stat_sample[i,(2*j-1)] = coe_var
    }
}

# 4.3 - Classificacao -  tree decisions

# Criacao Modelo

class_modelo = rpart(data = sample_values, as.factor(ID)~.,method = "class" )
class_modelo$cptable
# Modelo reduzido
#sample_values_selec = sample_values[,c(1,3,4,8,9,13,14)]
#class_modelo = rpart(data = sample_values_selec, as.factor(ID)~.,method = "class",maxcompete=8, xval = 20)

plot(class_modelo, uniform =T, main= "class tree")
text(class_modelo,cex=0.8)

# Predicao do modelo

pred_model = predict(indice_vegetacao,class_modelo,type = "class")
pred_model = ratify(pred_model)
plot(pred_model)

writeRaster(pred_model,choose.files())

# 4.4 - Modelo Random forest

sample_values$ID = as.factor(sample_values$ID)
sample_values = na.exclude(sample_values)
rf_model = randomForest(ID ~ ., data = sample_values, ntree = 500)

rf_pred_map = predict(indice_vegetacao,rf_model,type = "class")
plot(rf_pred_map)

writeRaster(rf_pred_map,choose.files())

# 5 - Teste de Eficiencia

# 5.1 - Selecao 
set.seed(655)
training = sample(nrow(sample_values), 0.75 * nrow(sample_values))
train_rf = randomForest(ID ~ ., data = sample_values[training,], ntree = 500, mtry = 15) # = hv.RF

# Importancia de cada variavel na determinacao
importance(train_rf)

# Predicao dos usos
pred_rf = predict(train_rf, newdata = sample_values[training, ])

# 
a = goofcat(observed = sample_values$ID[training],predicted = pred_rf )
a$confusion_matrix
a$kappa
