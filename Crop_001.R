# .............................. Corte com camada mascara..................................#

# bibliotecas
library(raster)
library(rgdal)
library(sf)
library(maptools)
library(XML)

# diretorios
# Pasta com todas as imagens
dir_landsat = choose.dir() # D:\GIS\Guarda_Mor_2019\Landsat
lista_dir_imagens = list.dirs(dir_landsat)

# pasta de saida
dir_landsat_process = choose.dir()#D:\GIS\Guarda_Mor_2019\Landsat_process

# shapefile
bacia_hidrografica = readOGR(choose.files()) #D:\GIS\Guarda_Mor_2019 #$ bacia_unida
proj_shape = crs(bacia_hidrografica)

# processo de corte
for(k in 2:length(lista_dir_imagens)){
  dir_imagem= lista_dir_imagens[k] # Diretorio das imagens
  lista_imagens = list.files(dir_imagem) # todas as imagens e arquivos
  for(n in 1:length(lista_imagens)){
    teste = unlist(strsplit(lista_imagens[n],split = "[.]"))
    if(teste[2]=="tif"){ # se for tif faz o processo de corte
      # seta a imagem
      imagem = raster(paste0(dir_imagem, "\\", lista_imagens[n]))
      # transforma o Vetor para a coordenada da imagem - mais rapido!
      bacia_hidrografica = spTransform(bacia_hidrografica, crs(imagem))
      
      # corte
      imagem_cortada = crop(x = imagem,y = extent(bacia_hidrografica))
      imagem_cortada = mask(x = imagem_cortada,mask =bacia_hidrografica)
      
      # Reprojetar raster para as coordenadas utm locais representadas no shape
      ext = projectExtent(imagem_cortada,crs = proj_shape)
      imagem_cortada = projectRaster(imagem_cortada, ext)
      
      # selecao da saida
      nome_pasta= unlist(strsplit(lista_dir_imagens[k], split = "/"))
      nome_pasta = nome_pasta[2]
      dir_saida = paste0(dir_landsat_process,"\\",nome_pasta)
      
      if(dir.exists(dir_saida)==F){
        dir.create(dir_saida)
      }
      writeRaster(imagem_cortada,filename = paste0(dir_saida,"\\cr_",lista_imagens[n]))
    }else{
      # selecao da saida
      nome_pasta= unlist(strsplit(lista_dir_imagens[k], split = "/"))
      nome_pasta = nome_pasta[2]
      dir_saida = paste0(dir_landsat_process,"\\",nome_pasta)
      if(dir.exists(dir_saida)==F){
        dir.create(dir_saida)
      }

      # Ler arquivos
      file.copy(from = paste0(dir_imagem, "\\",lista_imagens[n]), to = dir_saida)
    }
    }
  }







