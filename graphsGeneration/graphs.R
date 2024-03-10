# install.packages('ggplot2')
# install.packages('knitr')
# install.packages('dplyr')
# install.packages('gridExtra')
# install.packages('Kendall')
# install.packages('trend')

library(trend)
library(knitr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Kendall)
setwd("~/projects/dataAnalysisExercises/research")

statisc_tests <- function(x,y,testName,modelName){
  # Realize o teste de Mann-Kendall
  resultado_mann_kendall <- MannKendall(y)
  p_value <- resultado_mann_kendall[["sl"]][1]

  # Obtenha o slope manualmente
  modelo <- lm(y ~ x)
  slope <- coef(modelo)[2]

  ic <- intervalo_confianca_slope(x,y)
  meanY <- mean(y)
  # Exiba os resultados
  print("MK_TEST")
  print(resultado_mann_kendall)
  print(p_value)
  print("SLOPE")
  print(slope)
  print("CONFIDENC_INTERVALO")
  print(ic)
  print("MEAN")
  print(meanY)
  
  novos_dados <- data.frame(
    # TEST = c(testName),
    MODELNAME = c(modelName),
    # P_VALUE = c(p_value),
    SLOPE = c(slope),
    CONFIDENC_INTERVALO = paste(ic[1],ic[2],sep = " "),
    MEAN = c(meanY)
  )
  
  # Nome do arquivo CSV existente
  arquivo_csv <- paste("~/Documents/dados_",testName,".csv",sep = "")
  # Escrever os novos dados no final do arquivo CSV
  write.table(novos_dados, file = arquivo_csv, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
}

kb_to_gb <- function(kilobytes) {
  gigabytes <- kilobytes / 1024^2
  return(gigabytes)
}

bytes_to_gb <- function(bytes) {
  gigabytes <- bytes / 1024^3
  return(gigabytes)
}

intervalo_confianca_slope <- function(x,y) {
  # Ajustar o modelo de regressão linear
  modelo <- lm(y ~ x)
  
  # Coeficiente de correlação
  correlacao <- cor(x, y)
  
  # Desvio padrão de x e y
  desvio_x <- sd(x)
  desvio_y <- sd(y)
  
  # Tamanho da amostra
  n <- length(x)
  
  # Valor crítico da distribuição t
  valor_critico_t <- qt(0.975, df = n - 2)  # Para um intervalo de confiança de 95%
  
  # Calculando o intervalo de confiança do slope
  intervalo_confianca_slope <- summary(modelo)$coefficients[2, 1] +
    c(-1, 1) * valor_critico_t * (desvio_y / desvio_x) * sqrt((1 - correlacao^2) / (n - 2))
  
  print(typeof(intervalo_confianca_slope))
  return(intervalo_confianca_slope)
}

generate_graph <- function(df, xLegend, yLegend,yStart,yEnd,yBy) {
  time_interval <- 0.2
  
  dados_media <- df %>%
    mutate(grupo_tempo = ((time - min(time)) %/% time_interval) * time_interval) %>%
    group_by(grupo_tempo) %>%
    summarise(media_valor = mean(value, na.rm = TRUE))
  
  graph <- ggplot(dados_media, aes(x = grupo_tempo, y = media_valor)) +
    geom_line(color = "blue") +
    labs(x = xLegend, y = yLegend) +
    scale_x_continuous(breaks = seq(0, 48, by = 8)) +
    scale_y_continuous(breaks = seq(yStart, yEnd, by = yBy)) +
    coord_cartesian(xlim = c(0, 48)) +
    coord_cartesian(ylim = c(yStart, yEnd)) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
    )
  return(graph)
}

generate_graph_cache <- function(df, xLegend, yLegend) {
  time_interval <- 0.2

  dados_media <- df %>%
    mutate(grupo_tempo = ((time - min(time)) %/% time_interval) * time_interval) %>%
    group_by(grupo_tempo) %>%
    summarise(media_valor = mean(value, na.rm = TRUE))

  graph <- ggplot(dados_media, aes(x = grupo_tempo, y = media_valor)) +
  # graph <- ggplot(df, aes(x = time, y = value)) +
    geom_line(color = "blue") +
    labs(x = xLegend, y = yLegend) +
    scale_x_continuous(breaks = seq(0, 48, by = 8)) +
    # scale_y_continuous(breaks = seq(yStart, yEnd, by = yBy)) +
    coord_cartesian(xlim = c(0, 48)) +
    # coord_cartesian(ylim = c(yStart, yEnd)) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
    )
  return(graph)
}


growth_Rate <- function(vetor){
  start <- head(vetor, n = 1)
  end <- tail(vetor, n = 1)
  tax <- ((end - start) / start) *100
  return(tax)
}

generateGraphs <- function(modelName, pathFile, testTimePeriod) {
  
  pythiaStart <- c(1,2,1.7)
  pythiaEnd <- c(1.5,2.7,2.6)
  
  optStart <- c(0.4,1.3,1.5) 
  optEnd <- c(1.5,2.7,2.3)
  
  gptNeoStart <- c(1,2,1.7)
  gptNeoEnd <- c(1.6,2.5,2.7)
  
  yStartProcess <- 0
  yEndProcess <- 3
  yStartMemory <- 0
  yEndMemory <- 3
  yStartCached <- 0
  yEndCached <- 3
  yBy <- 0.1
  
  print(modelName)
  
  if (grepl("pythia",modelName)) {
    yStartProcess <- pythiaStart[1]
    yEndProcess <- pythiaEnd[1]
    yStartMemory <- pythiaStart[2]
    yEndMemory <- pythiaEnd[2]
    yStartCached <- pythiaStart[3]
    yEndCached <- pythiaEnd[3]
  } else if(grepl("opt",modelName,)) {
    yStartProcess <- optStart[1]
    yEndProcess <- optEnd[1]
    yStartMemory <- optStart[2]
    yEndMemory <- optEnd[2]
    yStartCached <- optStart[3]
    yEndCached <- optEnd[3]
    yBy <- 0.2
  } else {
    yStartProcess <- gptNeoStart[1]
    yEndProcess <- gptNeoEnd[1]
    yStartMemory <- gptNeoStart[2]
    yEndMemory <- gptNeoEnd[2]
    yStartCached <- gptNeoStart[3]
    yEndCached <- gptNeoEnd[3]
  }
  
  fileNameToProcesses <- paste("processes", modelName, "_clean.csv", sep = "")
  #leitura do arquivo criado apos a limpeza
  df <- read.csv(file = fileNameToProcesses,header = T,strip.white = T,na.strings = "",sep=",")
  # filtrando os processos com memoria maior que 0
  dados_filtrados <- df %>% filter(MEM>0)
  dados_filtrados$COMMAND <- as.factor(dados_filtrados$COMMAND)
  dados_filtrados$CURRENT_TIME <- as.POSIXct(dados_filtrados$CURRENT_TIME, format = "%d/%m/%Y+%H:%M:%S")
  proccessMEM <- dados_filtrados %>% filter( COMMAND == "python3 server.py")
  proccessMEM$CURRENT_TIME <- difftime(proccessMEM$CURRENT_TIME, proccessMEM$CURRENT_TIME[1], units = "hours") 
  proccessMEM <- proccessMEM %>% 
    mutate(timeInHours = as.numeric(CURRENT_TIME, units = "hours")) %>% 
    filter(timeInHours <= 50 )
  
  mktestProccess <- mk.test(proccessMEM$RSS)
  
  dataFrameToPrint <- data.frame(
    time = proccessMEM$timeInHours,
    value = kb_to_gb(proccessMEM$RSS)
  )
  
  print("Proccess Memory test start")  
  statisc_tests(dataFrameToPrint$time, dataFrameToPrint$value, "proccess", modelName)
  print("Proccess Memory test end")
  
  process_graph <- generate_graph(
    df = dataFrameToPrint, 
    xLegend ="Time (hour)", 
    yLegend = "Used Memory (GB)",
    yStart = yStartProcess,
    yEnd = yEndProcess,
    yBy = 0.1
  )
  graph_name <- paste(pathFile,"/",modelName,"_","processMemoryAnalysis.png",sep = "")
  ggsave(graph_name,plot = process_graph, width = 4, height = 3)
  
  #############################################################################
  dados_filtrados$CURRENT_TIME <- difftime(dados_filtrados$CURRENT_TIME, dados_filtrados$CURRENT_TIME[1], units = "hours")
  dados_filtrados <- dados_filtrados %>% filter(CURRENT_TIME <= 48 ) %>% arrange(COMMAND, CURRENT_TIME)
  GROWTH_RATE <- dados_filtrados %>% group_by(COMMAND) %>% summarise(GROWTH_RATE = growth_Rate(RSS)) %>% arrange(desc(GROWTH_RATE)) %>% ungroup() %>% slice_head(n = 5)
  write.csv(GROWTH_RATE, file = paste(pathFile,"/growth_rate.csv",sep = ""),row.names = FALSE)

  fileNameToMonitoring <- paste("monitoring", modelName, ".csv", sep = "")
  df <- read.csv(file = fileNameToMonitoring, header = T, strip.white = T, na.strings = "", sep=",")
  
  # df$currentTime <- as.POSIXct(df$currentTime, format = "%Y-%m-%d %H:%M:%OS")
  # df$currentTime <- difftime(df$currentTime, df$currentTime[1], units = "hours") 
  
  df <- df %>% 
    mutate(usedMemory = usedMemory / 10^9) %>% 
    mutate(usedSwap = usedSwap / 10^9) %>% 
    mutate(currentTime = as.POSIXct(df$currentTime, format = "%Y-%m-%d %H:%M:%OS")) %>%
    mutate(currentTime = difftime(df$currentTime, df$currentTime[1], units = "hours")) %>%
    mutate(timeInHours = as.numeric(currentTime, units = "hours")) %>% 
    filter(timeInHours <= 50 )
  
  dataFrameToPrint <- data.frame(
    time = df$timeInHours,
    value = df$usedMemory
  )
  print("Total Memory test start")
  statisc_tests(dataFrameToPrint$time, dataFrameToPrint$value, "Memory", modelName)
  print("Total Memory test end")
  
  totalMemoryGraph <- generate_graph(
    df = dataFrameToPrint, 
    xLegend ="Time (hour)", 
    yLegend = "Used Memory (GB)",
    yStart = yStartMemory,
    yEnd = yEndMemory,
    yBy = yBy
  )
  
  graph_name <- paste(pathFile,"/",modelName,"_","totalMemoryAnalysis.png",sep = "")
  ggsave(graph_name,plot = totalMemoryGraph, width = 4, height = 3)
  
  #############################################################################
  dataFrameToPrint <- data.frame(
    time = df$timeInHours,
    value = bytes_to_gb(df$cachedMemory)
  )
  
  print("Cached Memory test start")
  statisc_tests(dataFrameToPrint$time, dataFrameToPrint$value, "cached", modelName)
  print("Cached Memory test end")
  
  cachedMemoryGraph <- generate_graph(
    df = dataFrameToPrint, 
    xLegend ="Time (hour)", 
    yLegend = "Cached Memory (GB)",
    yStart = yStartCached,
    yEnd = yEndCached,
    yBy = 0.1
  )
    
  graph_name <- paste(pathFile,"/",modelName,"_","cachedMemoryAnalysis.png",sep = "")
  ggsave(graph_name,plot = cachedMemoryGraph, width = 4, height = 3)
  
}

modelsName <- c("gpt-neo-125m_", "pythia-160m_", "opt-125m_")
folderName <- c("gptNeo125m", "pythia160m", "opt125m")
filesTests <- c("10s","5s","1s")
testTimePeriodV <- c(0, 48)

for (i in 1:3) {
  modelName <- modelsName[i]
  for (j in 1:3) {
    pathFile <- paste("/home/chas/projects/dataAnalysisExercises/research/",folderName[i],"/",filesTests[j], sep = "")
    generateGraphs(paste(modelName,filesTests[j],sep = ""), pathFile, testTimePeriodV)
  }
}


#########################################################################
# df <- read.csv(
#   file = "benchMarkTest5s.csv",
#   header = T,
#   strip.white = T,
#   na.strings = "",
#   sep=",")
# 
# memoryTest  <-  
# cpuTest <-  mk.test(df$totalCpuUsage)
# swapTest <-  mk.test(df$usedSwap)
# 
# subject <- c("Memory", "CPU", "Swap")
# mktests <- c(memoryTest$p.value,
#              cpuTest$p.value,
#              swapTest$p.value)
# 
# tabela <- data.frame(subject, mktests)
# 
# tabela$mktests <- as.character(format(tabela$mktests, scientific = -5))
# 
# colnames(tabela) <- c("Subject", "p-value")
# 
# tabela %>%
#   kable("html") %>%
#   kable_styling(full_width = FALSE, font_size = 25) %>%
#   add_header_above(c("Mann-Kendall Test" = 2),
#                    escape = FALSE,
#                    align = "c",
#                    font_size = 30)
# 
# 
# dados_filtrados <- df %>% filter(MEM>0)
# dados_filtrados$COMMAND <- as.factor(dados_filtrados$COMMAND)
# dados_filtrados$CURRENT_TIME <- as.POSIXct(dados_filtrados$CURRENT_TIME, format = "%d/%m/%Y+%H:%M:%S")
# 
# dados <- dados_filtrados %>%
#   arrange(CURRENT_TIME) %>%
#   group_by(COMMAND) %>%
#   summarise(
#     TAXA = ((last(MEM)-first(MEM))/first(MEM))*100,
#     FIRST = first(MEM),
#     LAST = last(MEM),
#   ) %>%
#   arrange(desc(TAXA))
# 
# head(dados, 5)
# top_5_crescimento <- head(dados, 5)
# top_5_crescimento$COMMAND <- as.character(top_5_crescimento$COMMAND)
# top_5_crescimento$COMMAND #  <- sapply(strsplit(top_5_crescimento$COMMAND, "/"), function(x) tail(x, 1))
# 
# top_5_crescimento$COMMAND
# vetor_cores <- c("red", "blue", "green", "orange", "purple")
# barplot(top_5_crescimento$TAXA ,
#         ylab = "Process growth rate",
#         main = "More Growing Processes",
#         col = vetor_cores)
# text(x =1:5 ,top_5_crescimento$taxa, labels = paste(round(top_5_crescimento$taxa,2), "%"),pos = 3)
# legend("topright", legend = top_5_crescimento$COMMAND, fill = vetor_cores)

# fileCleaning <- function(fileName) {
#   # lendo o arquivo informado
#   processesgpt2 <- readLines(fileName)
#   # separando cada linha do arquivo por espaços em branco
#   processesgpt2 <- gsub("\\s+", " ", processesgpt2)
#   # o comando que gera os arquivos insere um cabeçalho para cada registro, então é necessárido detecatalo através das string 'USER' e retiralos do comando exceto pelo primeiro
#   processesgpt2 <- processesgpt2[-grep(pattern = "USER", processesgpt2)[-1]]
#   
#   # iteração por cada elemento para limpeza de cada linha
#   for (i in 1:length(processesgpt2)) {
#     # listando os elementos separados por espaco em um vetor
#     vetor <- unlist(strsplit(processesgpt2[i], " "))
#     # unindo os seis primeiros elementos que, pois nao sao separados por espaco
#     string1 <- paste(vetor[1:6], collapse = ",")
#     # o ultimo elemento representa o comando utilizado para inicializar o processo, entap é necessário uni-lo em apenas uma string, alem de manter os espacos contidos anteriormente 
#     string2 <- paste(",\"",paste(vetor[7:length(vetor)], collapse = " "),"\"", sep = "")
#     # união das duas strings geradas
#     processesgpt2[i] <- paste(string1,string2,sep="")
#   }
#   
#   # ajustando o cabeacalho
#   processesgpt2[1] <- "CURRENT_TIME,USER,PID,VSZ,RSS,MEM,COMMAND"
#   # removendo a extensao .txt do arquivo
#   fileName <- unlist(strsplit(fileName, ".txt"))
#   # adicionando .csv como extensao do arquivo 
#   file_path <- paste("/home/chas/projects/dataAnalysisExercises/research/",fileName,"_clean.csv", sep="")
#   writeLines(processesgpt2, con = file_path)
# }
# 
# for (i in 1:3) {
#   fileCleaning(paste("processes", modelName, filesTests[i], ".txt", sep = ""))
# }

