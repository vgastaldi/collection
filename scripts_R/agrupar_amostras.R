# Organização de tabelas - Todas as samples, folha e caule
library(readr)
options(stringsAsFactors = FALSE)

# Escolher o diretorio
setwd("C:\\Users\\vinid\\Desktop\\mapeamento/")

# Pegar o padrao
sample_S <- dir(path = getwd(), pattern ="_S")
sample_L <- dir(path = getwd(), pattern ="_L")

amostras_L <- NULL
amostras_L$Contig <- "remover"
as.data.frame(amostras_L) -> amostras_L

amostras_S <- NULL
amostras_S$Contig <- "remover"
as.data.frame(amostras_S) -> amostras_S


for (i in sample_L){
  read_table(i, col_names = FALSE) -> amostra  
  amostra[,c(2,1)] -> amostra
  gsub("_counts_L.txt","",i) -> i
  colnames(amostra) <- c("Contig",i)
  merge(amostras_L,amostra,by.x = 1, by.y =1, all.x=TRUE,all.y=TRUE) -> amostras_L
}
amostras_L[is.na(amostras_L)] <- 0
amostras_L[-1,] -> amostras_L
amostras_L[,c(1,2,7,13,3,4,5,10,11,12,17,18,19,6,8,9,14,15,16)] -> amostras_L
colnames(amostras_L) <- c("Contig","WWL1A","WWL1B","WWL1C","WWL2A","WWL2B","WWL2C","WWL3A","WWL3B","WWL3C","DL1A","DL1B","DL1C","DL2A","DL2B","DL2C","DL3A","DL3B","DL3C")

for (i in sample_S){
  read_table(i, col_names = FALSE) -> amostra  
  amostra[,c(2,1)] -> amostra
  gsub("_counts_S.txt","",i) -> i
  colnames(amostra) <- c("Contig",i)
  merge(amostras_S,amostra,by.x = 1, by.y =1, all.x=TRUE,all.y=TRUE) -> amostras_S
}
amostras_S[is.na(amostras_S)] <- 0
amostras_S[-1,] -> amostras_S
amostras_S[,c(1,17,18,19,5,6,7,11,12,13,2,3,4,8,9,10,14,15,16)] -> amostras_S
colnames(amostras_S) <- c("Contig","WWS1A","WWS1B","WWS1C","WWS2A","WWS2B","WWS2C","WWS3A","WWS3B","WWS3C","DS1A","DS1B","DS1C","DS2A","DS2B","DS2C","DS3A","DS3B","DS3C")


