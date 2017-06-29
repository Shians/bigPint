for (comparison in 2:48){
  outPath <- paste0("/Users/lindz/bigPint/tblshoot/RemSamples/Compare", comparison)
  rmarkdown::render(paste0(outPath, "/limmaTutorialBee.Rmd"))
  rm(list=ls())
}

Compare1 <- readRDS("Compare1/results.Rds")
Compare2 <- readRDS("Compare2/results.Rds")
Compare3 <- readRDS("Compare3/results.Rds")
Compare4 <- readRDS("Compare4/results.Rds")
Compare5 <- readRDS("Compare5/results.Rds")
Compare6 <- readRDS("Compare6/results.Rds")
Compare7 <- readRDS("Compare7/results.Rds")
Compare8 <- readRDS("Compare8/results.Rds")
Compare9 <- readRDS("Compare9/results.Rds")
Compare10 <- readRDS("Compare10/results.Rds")
Compare11 <- readRDS("Compare11/results.Rds")
Compare12 <- readRDS("Compare12/results.Rds")
Compare13 <- readRDS("Compare13/results.Rds")
Compare14 <- readRDS("Compare14/results.Rds")
Compare15 <- readRDS("Compare15/results.Rds")
Compare16 <- readRDS("Compare16/results.Rds")
Compare17 <- readRDS("Compare17/results.Rds")
Compare18 <- readRDS("Compare18/results.Rds")
Compare19 <- readRDS("Compare19/results.Rds")
Compare20 <- readRDS("Compare20/results.Rds")
Compare21 <- readRDS("Compare21/results.Rds")
Compare22 <- readRDS("Compare22/results.Rds")
Compare23 <- readRDS("Compare23/results.Rds")
Compare24 <- readRDS("Compare24/results.Rds")
Compare25 <- readRDS("Compare25/results.Rds")
Compare26 <- readRDS("Compare26/results.Rds")
Compare27 <- readRDS("Compare27/results.Rds")
Compare28 <- readRDS("Compare28/results.Rds")
Compare29 <- readRDS("Compare29/results.Rds")
Compare30 <- readRDS("Compare30/results.Rds")
Compare31 <- readRDS("Compare31/results.Rds")
Compare32 <- readRDS("Compare32/results.Rds")
Compare33 <- readRDS("Compare33/results.Rds")
Compare34 <- readRDS("Compare34/results.Rds")
Compare35 <- readRDS("Compare35/results.Rds")
Compare36 <- readRDS("Compare36/results.Rds")
Compare37 <- readRDS("Compare37/results.Rds")
Compare38 <- readRDS("Compare38/results.Rds")
Compare39 <- readRDS("Compare39/results.Rds")
Compare40 <- readRDS("Compare40/results.Rds")
Compare41 <- readRDS("Compare41/results.Rds")
Compare42 <- readRDS("Compare42/results.Rds")
Compare43 <- readRDS("Compare43/results.Rds")
Compare44 <- readRDS("Compare44/results.Rds")
Compare45 <- readRDS("Compare45/results.Rds")
Compare46 <- readRDS("Compare46/results.Rds")
Compare47 <- readRDS("Compare47/results.Rds")
Compare48 <- readRDS("Compare48/results.Rds")

c1 <- Compare1$allPairs$Freq
c2 <- Compare2$allPairs$Freq
c3 <- Compare3$allPairs$Freq
c4 <- Compare4$allPairs$Freq
c5 <- Compare5$allPairs$Freq
c6 <- Compare6$allPairs$Freq
c7 <- Compare7$allPairs$Freq
c8 <- Compare8$allPairs$Freq
c9 <- Compare9$allPairs$Freq
c10 <- Compare10$allPairs$Freq
c11 <- Compare11$allPairs$Freq
c12 <- Compare12$allPairs$Freq
c13 <- Compare13$allPairs$Freq
c14 <- Compare14$allPairs$Freq
c15 <- Compare15$allPairs$Freq
c16 <- Compare16$allPairs$Freq
c17 <- Compare17$allPairs$Freq
c18 <- Compare18$allPairs$Freq
c19 <- Compare19$allPairs$Freq
c20 <- Compare20$allPairs$Freq
c21 <- Compare21$allPairs$Freq
c22 <- Compare22$allPairs$Freq
c23 <- Compare23$allPairs$Freq
c24 <- Compare24$allPairs$Freq
c25 <- Compare25$allPairs$Freq
c26 <- Compare26$allPairs$Freq
c27 <- Compare27$allPairs$Freq
c28 <- Compare28$allPairs$Freq
c29 <- Compare29$allPairs$Freq
c30 <- Compare30$allPairs$Freq
c31 <- Compare31$allPairs$Freq
c32 <- Compare32$allPairs$Freq
c33 <- Compare33$allPairs$Freq
c34 <- Compare34$allPairs$Freq
c35 <- Compare35$allPairs$Freq
c36 <- Compare36$allPairs$Freq
c37 <- Compare37$allPairs$Freq
c38 <- Compare38$allPairs$Freq
c39 <- Compare39$allPairs$Freq
c40 <- Compare40$allPairs$Freq
c41 <- Compare41$allPairs$Freq
c42 <- Compare42$allPairs$Freq
c43 <- Compare43$allPairs$Freq
c44 <- Compare44$allPairs$Freq
c45 <- Compare45$allPairs$Freq
c46 <- Compare46$allPairs$Freq
c47 <- Compare47$allPairs$Freq
c48 <- Compare48$allPairs$Freq

all <- data.frame(Compare1$allPairs$Var2, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30, c31, c32, c33, c34, c35, c36, c37, c38, c39, c40, c41, c42, c43, c44, c45, c46, c47, c48)
colnames(all) <- c("Pair", paste0("NC.",seq(1:6)), paste0("NP.",seq(1:6)), paste0("NR.",seq(1:6)), paste0("NS.",seq(1:6)), paste0("VC.",seq(1:6)), paste0("VP.",seq(1:6)), paste0("VR.",seq(1:6)), paste0("VS.",seq(1:6)))

RemSampleComp <- all
saveRDS(RemSampleComp, "/Users/lindz/bigPint/tblshoot/RemSamples/RemSampleComp.rds")

myMatrix <- data.matrix(RemSampleComp[,2:ncol(RemSampleComp)])

png('Heatmap.png', width = 2000, height = 600)
heatmap(myMatrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10), labRow = as.character(RemSampleComp$Pair))
dev.off()


