# Script para calculo do erro padrao da diferenca entre tratamentos + media
# Disponivel em https://github.com/vgastaldi/collection
# Definir opcoes do R para carregar o arquivo
options(stringsAsFactors = FALSE)

# Definir o diretorio - no Windows copiar o caminho da pasta, substituindo \ por \\
setwd("")

# Carregar os pacotes do R que serao utilizados
library(readxl)
library(dplyr)

# Ler o arquivo a ser utilizado. Variacoes podem ser utilizadas com o read.delim
read_excel(".xlsx") -> delta
#read.delim("arquivo.?", sep = ",", header=TRUE, na.strings = c(""," ",".")) -> delta


# Elimine as colunas que nao sao necessarias. O script esta estruturado para tres colunas de identificação e uma coluna com valores, elimine as colunas adicionais
delta[,-5] -> delta

# Obter os valores que serao utilizados para iterar a tabela
unique(delta$X__1) -> subespecie
unique(delta$X__2) -> tratamento

# Criar um objeto vazio para receber os dados
tabela_final <- NULL
tabela_deltas <- NULL

# Loop para percorrer todas as subespecies/coluna 1
for (i in subespecie){
  # Separar em um novo objeto um item da primeira coluna
  delta[grep(paste0("\\b",i,"\\b"), delta$X__1),] -> subespecie_selecao
  # Separar para cada item da segunda coluba
  for (y in tratamento){
    subespecie_selecao[grep(paste0("\\b",y,"\\b"), subespecie_selecao$X__2),] -> subespecie_tratamento
    # Separar e ordenar para cada um dos horarios. Pode ser feito um loop se tiver mais do que 2 horarios.
    # Isso vai envolver outras mudancas nos calculos depois
    subespecie_tratamento[grep("M", subespecie_tratamento$X__3),] -> subespecie_manha
    as.data.frame(subespecie_manha) -> subespecie_manha
    subespecie_manha[order(subespecie_manha[,4], decreasing = TRUE),]  -> subespecie_manha
    subespecie_tratamento[grep("T", subespecie_tratamento$X__3),] -> subespecie_tarde
    as.data.frame(subespecie_tarde) -> subespecie_tarde
    subespecie_tarde[order(subespecie_tarde[,4], decreasing = TRUE),]  -> subespecie_tarde
    # Unir os valores em uma tabela so. Eles ja estao pareados por "valor"
    cbind(subespecie_manha[,c(1,2,4)], subespecie_tarde[,4]) -> subespecie_par_periodo
    # Renomeando as colunas. Pode ser alterado conforme necessidade
    colnames(subespecie_par_periodo) <- c("subespecie","tratamento","manha","tarde")
    # Esse comando apaga qualquer linha que tenha pelo menos uma coluna com NA
    na.omit(subespecie_par_periodo) -> subespecie_par_periodo
    # Calcular a diferenca entre manha e tarde
    subespecie_par_periodo$diferenca <- subespecie_par_periodo[,3] - subespecie_par_periodo[,4]
    # Gravar arquivo para os deltas
    subespecie_par_periodo[,c(1,2,5)] -> subespecie_gravar
    rbind(tabela_deltas, subespecie_gravar) -> tabela_deltas
    # Usar o pacote dplyr para calcular media e erro padrao do conjunto
    subespecie_calculo <- subespecie_par_periodo %>% 
      group_by(subespecie, tratamento) %>% 
      summarise(media = mean(diferenca),
                erro_padrao_manha = sd(manha)/sqrt(length(manha)), erro_padrao_tarde = sd(tarde)/sqrt(length(tarde)))
    # Calculo do erro padrao da diferenca
    subespecie_calculo$erro_padrao_diferenca <- sqrt((subespecie_calculo$erro_padrao_manha^2)+(subespecie_calculo$erro_padrao_tarde^2))
    # Removendo as colunas do erro padrao de cada periodo
    subespecie_calculo[,-c(4,5)] -> subespecie_calculo
    # Juntar em um arquivo so
    rbind(tabela_final, subespecie_calculo) -> tabela_final
    
  }
}

# Ordena a tabela pela primeira coluna
tabela_final[order(tabela_final$subespecie),]  -> tabela_final
tabela_deltas[order(tabela_deltas$subespecie),]  -> tabela_deltas


# Como trabalha com o Excel vamos trocar o ponto pela v?rgula no separador
gsub("\\.",",",tabela_final$media) -> tabela_final$media
gsub("\\.",",",tabela_final$erro_padrao_diferenca) -> tabela_final$erro_padrao_diferenca

# Gravar o arquivo em .csv para leitura no Excel. Sera um .csv que usa tabulacao!
# Ha alternativas melhores, mas elas querem Perl ou Java instalado no computador, essa ? a mais simples
write.table(tabela_final, "delta_R_DW.csv",row.names = FALSE,sep = "\t", quote = FALSE)

# Gravar os deltas individuais
gsub("\\.",",",tabela_deltas$diferenca) -> tabela_deltas$diferenca
write.table(tabela_deltas, "deltas_R.csv",row.names = FALSE,sep = "\t", quote = FALSE)

            