library(tidyr)
library(dplyr)

# Create bar chart to compare three methods
dat <- read.csv("compare3.csv")

dat <- dat[,c(1,2,5,8)]

dat2 <- dat %>% gather(method, value, -pairs)

ggplot(dat2, aes(factor(pairs), value, fill = method)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create Venn diagrams to compare three methods

vennDiagram(dt[,1:2], circle.col=c("turquoise", "salmon"))

lDF <- readRDS("/Users/lindz/bigPint/topGenes_limma.Rds")
dDF <- readRDS("/Users/lindz/BeeVirusDiet/DESeq/topGenes_DEG.Rds")
eDF <- readRDS("/Users/lindz/BeeVirusDiet/edgeR/DEGPairs/listDEG.Rds")
all <- read.delim(file="/Users/lindz/BeeVirusDiet/AllLaneCount.txt", row.names=1, stringsAsFactors = FALSE)
allGenes <- rownames(all)

groups <- c("NC", "NP", "NR", "NS", "VC", "VP", "VR", "VS")

for (i in 1:(length(groups)-1)){
  for (j in (i+1):length(groups)){
    vecL <- vecD <- vecE <- rep(0, length(allGenes))
    degL <- rownames(lDF[[paste0(groups[i], "vs", groups[j])]])
    degD <- dDF[[paste0(groups[i], groups[j])]]$genes
    degE <- rownames(eDF[[paste0(groups[i], groups[j])]])
    vecL[which(allGenes %in% degL)] = 1
    vecD[which(allGenes %in% degD)] = 1
    vecE[which(allGenes %in% degE)] = 1
    df <- data.frame(Genes = allGenes, Limma = vecL, DESeq = vecD, edgeR = vecE)
    png(paste0(groups[i], "_", groups[j], ".png"))
    vennDiagram(df[,c(2:4)], circle.col=c("turquoise", "salmon", "blue"), main = paste(groups[i], "and", groups[j]))
    dev.off()
  }
}

