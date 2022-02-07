# .............................. Masking Rasters in Bach mode..................................#
# 1 - Introdution

# This file was developed to crop rasters with a mask (shapefile). This code will help to cla- #
# ssification process.  It will reduce the area to enhance the calssification process perfor-  #
# mance and reduce the memory needed to storage the data. 

# 1 - Libraries
library(raster) # raster opperations
library(rgdal) # shapefile operations
library(sf) # spatial vectors
library(maptools) # raster and features operators
library(XML) # Access XML filees

# 2 - Directories and GIS files
# Directory with imagery directories.
# Normaly the satellite Scenes is stored in differents directories. This code will handle in  #
# this data storage organization. 
dir_landsat = choose.dir() # Command for Windows
lista_dir_imagens = list.dirs(dir_landsat)

# Output directory - This is the main output directory that will storage different directories #
# with the croped rasters.
dir_landsat_process = choose.dir()

# Choosing Shapefile to crop data
bacia_hidrografica = readOGR(choose.files())
proj_shape = st_crs(bacia_hidrografica)

# 3 - Crop Process
for(k in 2:length(lista_dir_imagens)){
  dir_imagem= lista_dir_imagens[k] # Landsat directories
  lista_imagens = list.files(dir_imagem) # Specific Scene directory
  for(n in 1:length(lista_imagens)){
    teste = unlist(strsplit(lista_imagens[n],split = "[.]")) # the extension must be .tif
    if(teste[2]=="tif"){ # if it is .tif, it will make the crop
      # File seting
      imagem = raster(paste0(dir_imagem, "\\", lista_imagens[n]))
      # Project Vector file to raster coordinate. It is faster than project the raster
      bacia_hidrografica = spTransform(bacia_hidrografica, crs(imagem))
      
      # Cropping
      imagem_cortada = crop(x = imagem,y = extent(bacia_hidrografica)) # with extention
      imagem_cortada = mask(x = imagem_cortada,mask =bacia_hidrografica) # with the shapefile shape
      
      # Project cropped raster to the shapefile CRS(corddinate Refference System)
      ext = projectExtent(imagem_cortada,crs = proj_shape)
      imagem_cortada = projectRaster(imagem_cortada, ext)
      
      # This sequence will create the output directory name
      nome_pasta= unlist(strsplit(lista_dir_imagens[k], split = "/"))
      nome_pasta = nome_pasta[2]
      dir_saida = paste0(dir_landsat_process,"\\",nome_pasta)
      
      if(dir.exists(dir_saida)==F){ # Verifing if directories exists..
        dir.create(dir_saida) # if it don't, create
      }
      writeRaster(imagem_cortada,filename = paste0(dir_saida,"\\cr_",lista_imagens[n])) #Saving croped raster
    }else{ #if the daa isn't tif, it will replace in new directory
      # Ouput selection
      nome_pasta= unlist(strsplit(lista_dir_imagens[k], split = "/"))
      nome_pasta = nome_pasta[2]
      dir_saida = paste0(dir_landsat_process,"\\",nome_pasta)
      if(dir.exists(dir_saida)==F){
        dir.create(dir_saida)
      }

      # Copying files.
      file.copy(from = paste0(dir_imagem, "\\",lista_imagens[n]), to = dir_saida)
    }
    }
  }







