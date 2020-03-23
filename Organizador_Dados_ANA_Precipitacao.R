#.............................................................................................#
#..............................ORGANIZADOR DE DADOS ANA.......................................#
#...................................(Precipitação)............................................#
#...................................FELIPE BERNARDI...........................................#
#............................https://github.com/felipekbernardi...............................#

# APRESENTACAO
#..1. Bibliotecas.............................................................................#
#install.packages("lubridate")
library(lubridate) # pacote para Trabalhar com datas


#.. 2. Periodo de analise.....................................................................#

#. O co'digo visa posicionar os volores de um ou um conjuto de arquivos padroes hidroweb  em- #
# basado em uma data fornecida pelo usuario. Assim, primeiro e' necessario fornecer essas da- #
# tas. Isso e' realizado via prompt de comando.

# 2.1 Entrada
# Ao executar as data inicial e final o usuario pode inserir os valores no console          
data_inicial = readline(prompt = "Escreva a data inicial da conversao (dd/mm/aaaa) >_")
data_final = readline(prompt = "Escreva a data final da conversao (dd/mm/aaaa) >_")

# 2.2 Ajuste da hora

formato = guess_formats(as.character(data_inicial), orders = "dmy") # determina o formato padrao
data_inicial = as.character(data_inicial) # transforma, por seguranca, as entradas em strings
data_inicial = as.POSIXct(data_inicial,format = formato[[2]], origin="1970-01-01 UTC", tz="Etc/GMT-3")
# Transforma a data para o padrao da biblioteca lubridate. 
# OBS: a biblioteca lubridate e' minha opçao por trabalhar com datas de diferentes UTCs. As-#
# sim, e' mais eficiente em unificar datas de outros bancos de dados.

# Setando para as 7. os dados do pluviografo sao acumulados para as 7h da manha. Então se a- #
# tribui o horario das 7 horas para facilitar em uma serie de analises. 
data_inicial = data_inicial+ hours(7)

# Mesma rotina de padronizacao para a data final
formato = guess_formats(as.character(data_final), orders = "dmy")
data_final = as.character(data_final)
data_final = as.POSIXct(data_final,format = formato[[2]], origin="1970-01-01 UTC", tz="Etc/GMT-3")
#setando para as 7
data_final = data_final+ hours(7)

# 3. Diretorios ..............................................................................#
# 3.1 Entradas
##Diretorio de entrada
diretorio_entrada = choose.dir(caption = "Escolha o Diretorio que contem arquivos padrao ANA 
                               com Dados de Precipitacao em mm") 
# diretorio onde esta o arquivo ou o conjunto de arquivos

##Diretorio de saida
diretorio_saida = choose.dir(caption = "Selecione o diretorio de saida - 
                             OBS: deve ser diferente do de entrada")
# diretorio onde vai ser salvo os arquivos processados

# 3.2 Orgqanizacao
arquivos = list.files(diretorio_entrada) #cria uma lista com o nome dos arquivos.

# 3.2.1 Criacao do arquivo de saida com a serie temporal
# A serie temporal e' uma sequencia de datas com valor diarios do valor inicial ao final

# Serie temporal
serieperiodo = interval(data_inicial,data_final)
comprimentoserie <- time_length(serieperiodo,"day")
periodo <- data_inicial + days(seq(0,comprimentoserie))

periodo = data.frame(numerica = as.numeric(periodo), data = periodo)
#  A data numerica, anexadas aqui em um data frame, e' uma maneira facil de realizar calculos#
# com a data pois representa a quantidade de segundos a partir de 01/01/1970. Antes de 1970  #
# os valores sao negativos e apos, positivos. 


# 4. Rotina de distribuicao de dados do hidroweb.............................................#

# O objetivo da rotina e' alocar na saida preferencialmente o dado consistido e na sua ausen-#
# cia alocar o dado nao consistido.

# arquivo de saida
saida1 = data.frame(numerica = periodo$numerica, data= periodo$data, nivel_cons =NA, estacao = NA)
# um arquivo auxiliar para criacao de tabela em cada iteracao
# Assim, os arquivos terao a data em numero, a data no formato padrao, o nivel de consisten- #
# cia(1 para nao consistido, 2 para consistido) e o valor de Prec em mm.

for (k in 1:length(arquivos)){ # for criado para ler todos os arquivos da pasta 
  saida = saida1
  # 4.1 Selecao do arquivo (k)
  tipo_arquivo = unlist(strsplit(arquivos[k], "[.]"))  
  if(tipo_arquivo[2]=="csv"){ # se for CSV abra csv
    dir_tabela = paste0(diretorio_entrada,"/" ,arquivos[k]) # cria o diretorio do arquivo
    tabela = read.csv(dir_tabela,sep=";" ,skip = 13, header = F) # leitura do arquivo
    
    # Dados da estacao
    # Codigo
    codigo = read.csv(dir_tabela,sep=";",skip = 9, nrows = 1, header = F)
    codigo = as.character(codigo$V1[1])
    codigo = unlist(strsplit(codigo,"[:]"))
    codigo = as.numeric(codigo[2])
    
  }else{ # se for txt abra table
    dir_tabela = paste0(diretorio_entrada,"/" ,arquivos[k]) # cria o diretorio do arquivo
    tabela = read.table(dir_tabela,sep=";" ,skip = 13, header = F) # leitura do arquivo
    # Dados da estacao
    # Codigo
    codigo = read.table(dir_tabela,sep=";",skip = 9, nrows = 1, header = F)
    codigo = as.character(codigo$V1[1])
    codigo = unlist(strsplit(codigo,"[:]"))
    codigo = as.numeric(codigo[2])
  }
  
  
  # 4.2 Ajuste da tabela
  tabela = tabela[,-(76:ncol(tabela))]
  
  # Nome das variaveis conforme o arquivo padrão da ANA
  cabecalho = c("EstacaoCodigo","NivelConsistencia","Data","TipoMedicaoChuvas","Maxima","Total",
                "DiaMaxima","NumDiasDeChuva","MaximaStatus","TotalStatus","NumDiasDeChuvaStatus",
                "TotalAnual","TotalAnualStatus","Chuva01","Chuva02","Chuva03","Chuva04","Chuva05",
                "Chuva06","Chuva07","Chuva08","Chuva09","Chuva10","Chuva11","Chuva12","Chuva13",
                "Chuva14","Chuva15","Chuva16","Chuva17","Chuva18","Chuva19","Chuva20","Chuva21",
                "Chuva22","Chuva23","Chuva24","Chuva25","Chuva26","Chuva27","Chuva28","Chuva29",
                "Chuva30","Chuva31","Chuva01Status","Chuva02Status","Chuva03Status","Chuva04Status",
                "Chuva05Status","Chuva06Status","Chuva07Status","Chuva08Status","Chuva09Status",
                "Chuva10Status","Chuva11Status","Chuva12Status","Chuva13Status","Chuva14Status",
                "Chuva15Status","Chuva16Status","Chuva17Status","Chuva18Status","Chuva19Status",
                "Chuva20Status","Chuva21Status","Chuva22Status","Chuva23Status","Chuva24Status",
                "Chuva25Status","Chuva26Status","Chuva27Status","Chuva28Status","Chuva29Status",
                "Chuva30Status","Chuva31Status")
  colnames(tabela)<-cabecalho 
  
  
  # 4.3 Ajuste de dados
  
  # 4.3.1 Selecao por linhas (Mensal)
  # data do inicio dos meses observadas
  formato = guess_formats(as.character(tabela$Data[1]), orders = "dmy")
  data_meses = as.character(tabela$Data)
  data_meses = as.POSIXct(data_meses,format = formato[[2]], origin="1970-01-01 UTC", tz="Etc/GMT-3")
  data_meses = data_meses + hours(7)
  tabela$Data = data_meses
  
  # 4.3.2 Substituir virgula por ponto e transformar em numeroconforme padrao internacional
    for(n in 5:ncol(tabela)){
    # substituir(o que?, pelo que?, onde?)
    tabela[,n]=gsub(",",".",tabela[,n] )
    tabela[,n]=as.numeric(tabela[,n])
    }
  
  # 4.3.3 Selecao de consistidos e nao consistidos (a)
  # Consistidos
  
  tabela_consistidos = subset(tabela,NivelConsistencia==2)
  tabela_naoconsistidos = subset(tabela, NivelConsistencia==1)
    
    # 4.5 Construcao do aquivo de Saida
    # Alocacao dos dados consistidos pela ana
    # Primeiramente se aloca os valores consistidos por comparacao com os valores da data

  for(t in 1:nrow(tabela_consistidos)){ # t e' o contador de linhas de consistidos enao consistidos. 
    # t e' o primeiro dia do mes que esta' sendo analisado. 
      teste_data = is.na(tabela_consistidos$Data[t]) # se ha valor na data da tabela
      n_linhas = nrow(tabela_consistidos) # Numero de linhas
      if(n_linhas==0){ # se nao ha dados consistidos atribui o valor de continuidade T
        teste_data = T
      }
      if(teste_data==F){ # se ha valores, esses sao alocados na serie
        dias_no_mes = as.numeric(days_in_month(tabela_consistidos$Data[t])) # seleciona quantos dias ha no mes
        
        for(c in 14:(14+dias_no_mes-1)){ # a tabela comeca os dados na coluna 14 e termina no valor 14 + dias daquele mes
          # c e' o valor iterativo que varia a coluna do dia 1 ao ultimo dia do mes
          data_tabela = tabela_consistidos$Data[t] + days(c-14) # calculo da posicao deste em relaco ao primeiro dia do mes
          posicao = (as.numeric(days(data_tabela - data_inicial))/86400)+1 # Posicao na tabela de saida
          # Como as datas estao em dia, o calculo e' referente a posicao e 86400 e' conversao dos valores
          # de segundos para dias.
          
          if(posicao>0 & posicao<=nrow(saida)){ # Se a posicao faz parte da tabela
            saida$estacao[posicao] = tabela_consistidos[t,c] # aloca em seu respectivo lugar
            saida$nivel_cons[posicao] = 2 # e atribui a consistencia o valor 2 de consistido
          }
        }
      }
    }
      
    #Alocacao dos dados nao_consistidos pela ana, seguindo mesma rotina do consistidos 
    # verificando, apenas, se ja ha dado consistido no local
  for(t in 1:nrow(tabela_naoconsistidos)){
    teste_data = is.na(tabela_naoconsistidos$Data[t])
    n_linhas = nrow(tabela_naoconsistidos)
    if(n_linhas==0){
      teste_data = T
    }
    if(teste_data==F){
      dias_no_mes = as.numeric(days_in_month(tabela_naoconsistidos$Data[t]))
      for(c in 14:(14+dias_no_mes-1)){
        data_tabela = tabela_naoconsistidos$Data[t] + days(c-14)
        posicao = (as.numeric(days(data_tabela - data_inicial))/86400)+1
        if(posicao>0 & posicao<=nrow(saida)){
          teste = is.na(saida$estacao[posicao])
          if(teste==T){ # se nao ha valor atribuido, e' colocado o nao consistido no lugar
            saida$estacao[posicao] = tabela_naoconsistidos[t,c]
            saida$nivel_cons[posicao] = 1
          }
        }
      }
    } 
  }
  colnames(saida)= c("numerica", "data", "nive_cons", paste0("e_",codigo))
  write.csv(saida,file = paste0(diretorio_saida,"/e_",codigo,".csv"), row.names = F)
}




