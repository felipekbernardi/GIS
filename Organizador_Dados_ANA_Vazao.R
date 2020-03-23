#.............................................................................................#
#..............................ORGANIZADOR DE DADOS ANA.......................................#
#......................................(VAZAO)................................................#
#..................................FELIPE BERNARDI............................................#
#............................https://github.com/felipekbernardi...............................#

# APRESENTACAO

# 1. Bibliotecas .............................................................................#
# install.packages("lubridate")
library(lubridate)

# 2. Periodo de analise ......................................................................#
# 2.1 Entrada
data_inicial = readline(prompt = "Escreva a data inicial da conversao (dd/mm/aaaa) >_")
data_final = readline(prompt = "Escreva a data final da conversao (dd/mm/aaaa) >_")

# 2.2 Ajuste

formato = guess_formats(as.character(data_inicial), orders = "dmy")
data_inicial = as.character(data_inicial)
data_inicial = as.POSIXct(data_inicial,format = formato[[2]], origin="1970-01-01 UTC", tz="Etc/GMT-3")
#setando para as 7
#data_inicial = data_inicial+ hours(7)

formato = guess_formats(as.character(data_final), orders = "dmy")
data_final = as.character(data_final)
data_final = as.POSIXct(data_final,format = formato[[2]], origin="1970-01-01 UTC", tz="Etc/GMT-3")
#setando para as 7
#data_final = data_final+ hours(7)

# 3. Diretorios ..............................................................................#

# 3.1 Entradas
##Diretorio de entrada
diretorio_entrada = choose.dir(caption = "Escolha o Diretorio que contem arquivos padrao ANA 
                               com Dados de vazões")

##Diretorio de saida
diretorio_saida = choose.dir(caption = "Selecione o diretorio de saida")

# 3.2 Orgqanizacao
arquivos = list.files(diretorio_entrada)

# 3.2.1 Criacao do arquivo de saida com a serie temporal

# serie temporal
serieperiodo = interval(data_inicial,data_final)
comprimentoserie <- time_length(serieperiodo,"day")
periodo <- data_inicial + days(seq(0,comprimentoserie))
periodo = data.frame(numerica = as.numeric(periodo), data = periodo)

# Ajuste horario de verao 

# arquivo de saida
saida1 = data.frame(numerica = periodo$numerica, data= periodo$data, nivel_cons =NA, estacao = NA)

# 4. Rotina .................................................................................#

for (k in 1:length(arquivos)){
  saida = saida1
  # 4.1 Selecao do arquivo (k)
  dir_tabela = paste0(diretorio_entrada,"/" ,arquivos[k])
  tabela = read.table(dir_tabela,sep=";" ,skip = 14, header = F)
  
  # 4.2 Ajuste da tabela
  tabela = tabela[,-(48:ncol(tabela))]
  
  # Nome das variaveis

  
cabecalho = c("EstacaoCodigo",	"NivelConsistencia",	"Data",	"Hora",	"MediaDiaria", "MetodoObtencaoVazoes",	
              "Maxima",	"Minima",	"Media",	"DiaMaxima",	"DiaMinima",	"MaximaStatus",	"MinimaStatus",	"MediaStatus",	
              "MediaAnual", "MediaAnualStatus", "Vazao01", "Vazao02",	"Vazao03",	"Vazao04",	"Vazao05",	"Vazao06",	
              "Vazao07",	"Vazao08",	"Vazao09",	"Vazao10",	"Vazao11",	"Vazao12",	"Vazao13",	"Vazao14",	"Vazao15", 
              "Vazao16", 	"Vazao17",	"Vazao18",	"Vazao19",	"Vazao20",	"Vazao21",	"Vazao22",	"Vazao23",
              "Vazao24",	"Vazao25",	"Vazao26",	"Vazao27", "Vazao28", "Vazao29",	"Vazao30",	"Vazao31")
  
  colnames(tabela)<-cabecalho 
  
  # Dados da estacao
  # Codigo
  codigo = read.table(dir_tabela,sep=";",skip = 10, nrows = 1, header = F)
  codigo = as.character(codigo$V1[1])
  codigo = unlist(strsplit(codigo,"[:]"))
  codigo = as.numeric(codigo[2])
  
  # 4.3 Ajuste de dados
  
  # 4.3.1 Selecao por linhas (Mensal)
  # data do inicio dos meses observadas
  formato = guess_formats(as.character(tabela$Data[1]), orders = "dmy")
  data_meses = as.character(tabela$Data)
  data_meses = as.POSIXct(data_meses,format = formato[[2]], origin="1970-01-01 UTC", tz="Etc/GMT-3")
  data_meses = data_meses
  tabela$Data = data_meses
  
  # 4.3.2 Substituir virgula por ponto e transformar em numero
    for(n in 7:ncol(tabela)){
    # substituir(o que?, pelo que?, onde?)
    tabela[,n]=gsub(",",".",tabela[,n] )
    tabela[,n]=as.numeric(tabela[,n])
    }
  # 4.3.3 Selecao de consistidos e nao consistidos (a)
  tabela_consistidos = subset(tabela,NivelConsistencia==2)
  tabela_naoconsistidos = subset(tabela, NivelConsistencia==1)
    
    # 4.5 Construcao do aquivo de Saida
    # Alocacao dos dados consistidos pela ana

    for(t in 1:nrow(tabela_consistidos)){
      teste_data = is.na(tabela_consistidos$Data[t])
      n_linhas = nrow(tabela_consistidos)
      if(n_linhas==0){
        teste_data = T
      }
      if(teste_data==F){
        dias_no_mes = as.numeric(days_in_month(tabela_consistidos$Data[t]))
        
        for(c in 17:(17+(dias_no_mes-1))){
          data_tabela = tabela_consistidos$Data[t] + days(c-17)
          posicao = (as.numeric(days(data_tabela - data_inicial))/86400)+1
          
          if(posicao>0 & posicao<=nrow(saida)){
            saida$estacao[posicao] = tabela_consistidos[t,c]
            saida$nivel_cons[posicao] = 2
          }
        }
      }
    }
      
    #Alocacao dos dados nao_consistidos pela ana
  for(t in 1:nrow(tabela_naoconsistidos)){
    teste_data = is.na(tabela_naoconsistidos$Data[t])
    n_linhas = nrow(tabela_naoconsistidos)
    if(n_linhas==0){
      teste_data = T
    }
    if(teste_data==F){
      dias_no_mes = as.numeric(days_in_month(tabela_naoconsistidos$Data[t]))
      for(c in 17:(17+dias_no_mes-1)){
        data_tabela = tabela_naoconsistidos$Data[t] + days(c-17)
        posicao = (as.numeric(days(data_tabela - data_inicial))/86400)+1
        if(posicao>0 & posicao<=nrow(saida)){
          teste = is.na(saida$estacao[posicao])
          if(teste==T){
            saida$estacao[posicao] = tabela_naoconsistidos[t,c]
            saida$nivel_cons[posicao] = 1
          }
        }
      }
    } 
  }
  colnames(saida)= c("numerica", "data", "nive_cons", paste0("e_",codigo))
  write.csv(saida,file = paste0(diretorio_saida,"/e_",codigo,".csv"))
}


