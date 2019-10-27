library(tidyr)
library(dplyr)
library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL
library(rmarkdown)
library(class)
library(caret)
library(e1071)
library(data.table)




beers = read.csv("/Users/Izzino/Documents/MDS-6306-Doing-Data-Science-Fall-2019/Unit 8 and 9 Case Study 1/Beers.csv")  # read csv file 
breweries = read.csv("/Users/Izzino/Documents/MDS-6306-Doing-Data-Science-Fall-2019/Unit 8 and 9 Case Study 1/Breweries.csv")  # read csv file 



makecodebook

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

summary(beers)
summary(breweries)

total <- merge(beers,breweries,by="ID")

countbystate <- breweries %>% group_by(State) %>%summarise(no_rows = length(State))
print(as_tibble(countbystate), n = 100)

bb <- merge(beers, breweries, by.beers=c(“brewery_id”),
                    by.breweries=c(“brew_id”))


bb <- merge(beers, breweries, by.beers = "Brewery_id", by.breweries = "Brew_ID")


bb <- merge.data.frame(beers, breweries, by.x = "Brewery_id", by.y = "Brew_ID")



bbibu <- completeFun(bb, "IBU")
bbabv <- completeFun(bb, "ABV")

bba <- completeFun(bbabv,"IBU")

summary(bbabv)

head(bb,n=6)
tail(bb,n=6)
bb[1:6,]

install.packages('/Users/Izzino/Downloads/doBy_4.6-2.tar.gz', repos = NULL, type="source")
install.packages('/Users/Izzino/Downloads/dataMaid_1.3.2.tar.gz', repos = NULL, type="source")
library(doBy)

bbmabv <- summaryBy(ABV ~ State, data = bba, 
          FUN = list(mean, max, min, median, sd))
bbmibu <- summaryBy(IBU ~ State, data = bba, 
          FUN = list(mean, max, min, median, sd))


bbstat <- merge(bbmabv,bbmibu,by="State")
bbstat1 <- bbstat
bbstat$ABV.MedianX100 <- bbstat$ABV.Median * 100
ggplot(fifa ,aes(y = position ,fill = position))+geom_bar()

summary(bbstat)

library(reshape2)
bbstat_long <- melt(bbstat, id.var = "State")



ggplot(subset(bbstat_long,variable %in% c("ABV.median","IBU.median")) , aes(x = State, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") 

ggplot(bbstat,aes(y = ABV.median, x  = State, fill = State))+geom_bar(stat = "identity")
ggplot(bbstat,aes(y = IBU.median, x  = State, fill = State))+geom_bar(stat = "identity")

ggplot(bbibu, aes(x= ABV, fill = ABV , color = ABV)) + geom_histogram()

bbuIPA <- subset(bbibu, (IPA =='yes'))

ggplot(bbuIPA, aes(x= ABV, fill = IBU , color = Style)) + geom_histogram()


ggplot(bbstat, aes(x, y = value, color = variable)) + 
  geom_point(aes(y = y1, col = "y1")) + 
  geom_point(aes(y = y2, col = "y2"))

ggplot(data = bba, mapping = aes(x = ABV, y = IBU)) +
  geom_point()+
  geom_smooth(method = lm)




breweries %>% count(State)



summary(bbibu)

IPA <- select(bbibu,contains("IPA"))

mtcars[rownames(mtcars) %like% "Merc", ]


bbibu$IPA <- ifelse(grepl("IPA", bbibu$Style), "yes", "no")
bbibu$Ale <- ifelse(grepl("Ale", bbibu$Style), "yes", "no")


bbibu1 <-subset(bbibu, (IPA =='yes')  || (Ale =='yes'))


bbAle <- bbAle[- grep("IPA", bbAle$Style),]


trainIndices = sample(1:dim(bbibu1)[1],round(.20 * dim(bbibu1)[1]))
train = bbibu1[trainIndices,]
test = bbibu1[-trainIndices,]

classifications = knn(train[,c("ABV","IBU")],test[,c("ABV","IBU")],train$IPA, prob = TRUE, k = 6)
table = table(classifications,test$IPA)
confusionMatrix(table)


trainIndices = sample(1:dim(bbibu1)[1],round(.20 * dim(bbibu1)[1]))
train = bbibu1[trainIndices,]
test = bbibu1[-trainIndices,]

classifications = knn(train[,c("ABV","IBU")],test[,c("ABV","IBU")],train$IPA, prob = TRUE, k = 6)
table = table(classifications,test$IPA)
confusionMatrix(table)


ggplot(data = bbibu1, mapping = aes(x = ABV, y = IBU,color = IPA)) +
  geom_point()+
  geom_smooth(method = lm)


bbibu[which.max(bbibu$ABV),]
bbibu[which.max(bbibu$IBU),]





summary(bbAle)
