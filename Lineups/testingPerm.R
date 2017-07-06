
countTable <- readRDS("/Users/lindz/bigPint/BeeCountOrig.Rds")
group1="NP"; group2="NS"; nRep=6; nPerm=20

groupNames = unlist(lapply(colnames(countTable), function (x) unlist(strsplit(x, "[.]"))[1]))
listCond = groupNames[c(which(unlist(lapply(colnames(countTable), function (x) unlist(strsplit(x, "[.]"))[1]))==group1), which(unlist(lapply(colnames(countTable), function (x) unlist(strsplit(x, "[.]"))[1]))==group2))]
permCols = colnames(countTable)[which(groupNames %in% c(group1, group2))]
allComb <- getPerms(length(listCond))

allComb[allComb==1]<-1.53253
allComb[allComb==2]<-18.453263
allComb[allComb==3]<-37.46364
allComb[allComb==4]<-38.6346
allComb[allComb==5]<-47.3547
allComb[allComb==6]<-57.6373
allComb[allComb==7]<-78.275474
allComb[allComb==8]<-89.2373
allComb[allComb==9]<-100.273734
allComb[allComb==10]<-200737
allComb[allComb==11]<-300.53234
allComb[allComb==12]<-432.44754

test <- apply(allComb,1,function(x) mean(x[1:6]))
test2 <- apply(allComb,1,function(x) mean(x[7:12]))
testDiff <- abs(test-test2)
length(unique(testDiff))
