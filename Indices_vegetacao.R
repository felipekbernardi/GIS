# .................................................................................#
# ........................... Indices de vegetacao para ...........................#
# ......................... Cenas Landsat pre-trabalhadas..........................#
# .................................................................................#


# 1 - bibliotecas
library(RStoolbox)
library(raster)

# 2 - Diretorios
dir_landsat_process = choose.dir()#D:\GIS\Guarda_Mor_2019\Landsat_process
lista_dir_imagens = list.dirs(dir_landsat_process)

for(k in 2:length(lista_dir_imagens)){
  # 3 - selecao de Bandas
  dir_cena = lista_dir_imagens[k]
  arquivos = list.files(dir_cena)

  for(n in 1:length(arquivos)){
    teste = unlist(strsplit(arquivos[n],split = "_"))
  if(teste[length(teste)]=="band2.tif"){ # se for a banda 2 grava no blue
    b_blue = raster(paste0(dir_cena,"\\",arquivos[n]))
    b_blue = b_blue*0.0001 # o valor 0.0001 e' o fator escala pra reflectancia ficar entre 0 e 1.
  }else if(teste[length(teste)]=="band4.tif"){ # se for a banda 4 grava no red
    b_red = raster(paste0(dir_cena,"\\",arquivos[n]))
    b_red = b_red*0.0001
  }else if(teste[length(teste)]=="band5.tif"){ # se for a banda 5 grava no nir
    b_NIR = raster(paste0(dir_cena,"\\",arquivos[n]))
    b_NIR =b_NIR*0.0001
  }
  }
  
  ls_imagem = brick(b_blue, b_red, b_NIR)
  names(ls_imagem) = c("b_blue", "b_red", "b_NIR")
  
  #red = 4
  #nir = 5
  #blue = 2
  
  # 4 - calculo e armazenamento dos indices
  
  # EVI - red, blue, nir
  evi_map = spectralIndices(ls_imagem, red = "b_red" , nir = "b_NIR", blue = "b_blue", indices = "EVI")
  writeRaster(evi_map,filename=(paste0(dir_cena,"\\","evi.tif")),overwrite=TRUE)
  
  # EVI2 - red e nir
  evi2_map = spectralIndices(ls_imagem, red = "b_red" , nir = "b_NIR", blue = "b_blue", indices = "EVI2")
  writeRaster(evi2_map,filename=(paste0(dir_cena,"\\","evi2.tif")),overwrite=TRUE)
  
  # NRVI - red e nir
  nrvi_map = spectralIndices(ls_imagem, red = "b_red" , nir = "b_NIR", blue = "b_blue", indices = "NRVI")
  writeRaster(nrvi_map,filename=(paste0(dir_cena,"\\","nrvi.tif")),overwrite=TRUE)
  
  # NDVI - red - nir
  ndvi_map = spectralIndices(ls_imagem, red = "b_red" , nir = "b_NIR", blue = "b_blue", indices = "NDVI")
  writeRaster(ndvi_map,filename=(paste0(dir_cena,"\\","ndvi.tif")),overwrite=TRUE)
  
  # SAVI - red - nir
  savi_map = spectralIndices(ls_imagem, red = "b_red" , nir = "b_NIR", blue = "b_blue", indices = "SAVI")
  writeRaster(savi_map,filename=(paste0(dir_cena,"\\","savi.tif")),overwrite=TRUE)
  
  # DVI - red-nir
  dvi_map = spectralIndices(ls_imagem, red = "b_red" , nir = "b_NIR", blue = "b_blue", indices = "DVI")
  writeRaster(dvi_map,filename=(paste0(dir_cena,"\\","dvi.tif")),overwrite=TRUE)
}

plot(evi_map)
plot(evi2_map)
plot(nrvi_map)
plot(ndvi_map)
plot(savi_map)
plot(dvi_map)

