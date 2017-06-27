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

colnames(Compare2$allPairs)[2] <- "Compare2"
colnames(Compare3$allPairs)[2] <- "Compare3"
colnames(Compare4$allPairs)[2] <- "Compare4"
colnames(Compare5$allPairs)[2] <- "Compare5"
colnames(Compare6$allPairs)[2] <- "Compare6"
colnames(Compare7$allPairs)[2] <- "Compare7"
colnames(Compare8$allPairs)[2] <- "Compare8"
colnames(Compare9$allPairs)[2] <- "Compare9"
colnames(Compare10$allPairs)[2] <- "Compare10"
colnames(Compare11$allPairs)[2] <- "Compare11"
colnames(Compare12$allPairs)[2] <- "Compare12"
colnames(Compare13$allPairs)[2] <- "Compare13"

all <- data.frame(Compare2$allPairs$Var2, Compare2$allPairs$Compare2, Compare3$allPairs$Compare3, Compare4$allPairs$Compare4, Compare5$allPairs$Compare5, Compare6$allPairs$Compare6, Compare7$allPairs$Compare7, Compare8$allPairs$Compare8, Compare9$allPairs$Compare9, Compare10$allPairs$Compare10, Compare11$allPairs$Compare11, Compare12$allPairs$Compare12, Compare13$allPairs$Compare13)
colnames(all) <- c("Pair", "2","3","4","5","6","7","8","9","10","11","12","13")

filterComp <- all
saveRDS(filterComp, "/Users/lindz/bigPint/tblshoot/Filters/filterComp.rds")
