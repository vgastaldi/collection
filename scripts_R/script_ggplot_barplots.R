# Generating barplots with error bar using ggplot2
options(stringsAsFactors = FALSE, scipen = 999)

library(ggplot2)
setwd("C:\\Users\\Renata\\Google Drive\\Doutorado direto\\2018\\PLANT_PHYS\\filtragem_tmm_Vi")
setwd("D:\\Downloads/")

#read.delim("Amostras_normalizadas.csv", sep = "\t", na.strings = c(""," ","."), header = TRUE) -> allcpm
#usando a tabela de means e SDs pra filtrar contigs.
#read.delim("amostras_norm_comDE.txt", sep = "\t", na.strings = c(""," ","."), header = TRUE) -> comDE
read.delim("amostras_norm_meanSD_SE.txt", sep = "\t", na.strings = c(""," ","."), header = TRUE) -> meanSD

#filtrando
read.delim("list.txt",sep="\t",header = FALSE) -> contigs

# Loop
for (i in contigs[,1]){
  meanSD[which(meanSD$Contig == i),] -> contig_values
  gsub("\\|.*","",i) -> i
  contig_values[,grep("mean", colnames(contig_values))] -> contig_mean
  t(contig_mean) -> contig_mean
  contig_values[,grep("se", colnames(contig_values))] -> contig_se
  t(contig_se) -> contig_se
  as.data.frame((cbind(contig_mean,contig_se))) -> contig_T
  colnames(contig_T) <- c("mean","SE")
  row.names(contig_T) -> contig_T$samples
  gsub("mean","",contig_T$samples) -> contig_T$samples
  contig_T[grep("S", contig_T$sample),] -> contig_T_S
  contig_T_S$samples <- factor(contig_T_S$samples, levels = c("CS1","DS1","CS2","DS2","CS3","DS3"))
  contig_T_S$Treatment <- c("Well-watered","Droughted","Well-watered","Droughted","Well-watered","Droughted")
  contig_T[grep("L", contig_T$sample),] -> contig_T_L
  contig_T_L$samples <- factor(contig_T_L$samples, levels = c("CL1","DL1","CL2","DL2","CL3","DL3"))
  contig_T_L$Treatment <- c("Well-watered","Droughted","Well-watered","Droughted","Well-watered","Droughted")
  # With titles
  #temp_plot = ggplot(contig_T_S, aes(x=samples, y=mean,fill=Treatment)) +
  #  geom_bar(stat="identity", position="dodge") +
  #  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width=0.6) +
  #  theme_classic() + xlab("Samples") + ylab("TMM Counts") + ggtitle(paste(i,"stem",sep="_")) + theme(plot.title = element_text(hjust = 0.5, size = 12))
  # Without
  temp_plot = ggplot(contig_T_S, aes(x=samples, y=mean,fill=Treatment)) +
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width=0.6) +
    theme_classic() + theme(
      plot.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(), legend.position="none")
  ggsave(temp_plot, file=paste0(i, "_barplot_stem.tiff", sep = ""), width = 14, height = 10, units = "cm",dpi = 300)
  # With titles
  #temp_plot2 = ggplot(contig_T_L, aes(x=samples, y=mean,fill=Treatment)) +
  #  geom_bar(stat="identity", position="dodge") +
  #  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width=0.6) +
  #  theme_classic() + xlab("Samples") + ylab("TMM Counts") + ggtitle(paste(i,"leaf",sep="_")) + theme(plot.title = element_text(hjust = 0.5, size = 12))
  # Without
  temp_plot2 = ggplot(contig_T_L, aes(x=samples, y=mean,fill=Treatment)) +
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width=0.6) +
    theme_classic() + theme(
      plot.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(), legend.position="none")
    ggsave(temp_plot2, file=paste0(i, "_barplot_leaf.tiff", sep = ""), width = 14, height = 10, units = "cm",dpi = 300)
}