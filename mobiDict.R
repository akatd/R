library(data.table)
setwd("~/")

##### DATA PREP #####

moeDict <- fread("moeDict.csv")
setDT(moeDict)
moeDict <- moeDict[,c(3,7,11)]
names(moeDict) <- c("word","prn","def")

# JUST FOR TESTING!!!
moeDict <- moeDict[1:10,]

##### WRITE MOBI #####

pb <- txtProgressBar(min = 0, max = nrow(moeDict), style = 3)

for (i in seq(nrow(moeDict))) {

word <- moeDict[[i,"word"]]
pronunciation <- moeDict[[i,"prn"]]
definition <- moeDict[[i,"def"]]

dictxt <-  
sprintf("<idx:entry><b><idx:orth>\n %s
        </idx:orth></b><i>\n %s
        </i><br/>\n %s
        </idx:entry>\n
        <br/><hr/>",
        word, pronunciation, definition)

cat(dictxt,file = "outfile.mobi",append = T)

setTxtProgressBar(pb,i)
  
}

close(pb)

paste0("<html><body>",moeDict,"</body></html>")



