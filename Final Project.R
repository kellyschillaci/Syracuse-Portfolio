
set.seed(1337)
library(dplyr)
library(data.table)
library(ggplot2)
library(arules)
library(arulesViz)
library(cluster)
library(factoextra)
library(caret)
library(mclust)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(Cairo)
library(randomForest)
library(reshape2)
library(proxy)
library(dendextend)

athleteEvents <- read.csv('athlete_events.csv',header = T, stringsAsFactors = F)
noc <- read.csv('noc_regions.csv',header=T, stringsAsFactors = F)

rawData <- athleteEvents %>% left_join(noc, by='NOC')
colnames(rawData) <- tolower(colnames(rawData))

head(rawData)
str(rawData)

testID <- rawData[,1:2]
by_id <- testID %>% group_by(id, name)
by_id <- unique(by_id)
dim(by_id)[1]
dim(by_id)[1] == length(unique(by_id$id))
dim(by_id)[1] == length(unique(by_id$name))
length(unique(by_id$name))
dupeID <- unique(by_id[duplicated(by_id$name, nmax=1),])
head(dupeID)

dataClean <- rawData[-which(rawData$name %in% dupeID$name),]

unique(dataClean$sex)

length(which(is.na(dataClean$age)))
dataClean <- dataClean[-which(is.na(dataClean$age)),]
min(dataClean$age)
max(dataClean$age) #according to research, oldest olympian to compete was 73 http://www.oldest.org/sports/olympians/
dataClean[which(dataClean$age > 73),]

dataClean <- dataClean[-which(dataClean$age > 73),]

length(which(is.na(dataClean$height)))
dataClean <- dataClean[-which(is.na(dataClean$height)),]

min(dataClean$height)
max(dataClean$height) #Yes, Yao Ming really is that tall

length(which(is.na(dataClean$weight)))
dataClean <- dataClean[-which(is.na(dataClean$weight)),]

min(dataClean$weight) #55 lbs seems low
max(dataClean$weight) #and 471 seems high

dataClean[which(dataClean$weight == 25),] #But apparently this is real https://www.sports-reference.com/olympics/athletes/ch/choi-myong-hui-1.html
dataClean[which(dataClean$weight == 214),] #https://www.sports-reference.com/olympics/athletes/bl/ricardo-blas-jr-1.html

length(which(is.na(dataClean$team)))
length(which(is.na(dataClean$noc)))

testNOC <- data.frame(dataClean[,7:8])
by_noc <- testNOC %>% group_by(noc, team)
by_noc <- unique(by_noc)
by_team <- testNOC %>% group_by(team, noc)
by_team <- unique(by_team)
dim(by_noc)[1]
dim(by_team)[1]
dim(by_noc)[1] == length(unique(by_noc$noc))
dim(by_noc)[1] == length(unique(by_noc$team))
length(unique(by_noc$team))
dupeNOC <- unique(by_noc[duplicated(by_noc$team, nmax=1),])
head(dupeNOC)
unique(dataClean[which(dataClean$noc=='FIN'),]$team) #Multiple iterations of the same country name, we'll use 'NOC' as a proxy.

dim(dupeTeam <- unique(by_team[duplicated(by_team, nmax=1),]))[1] #no team has multiple NOC values.

sort(unique(dataClean$sport))

unique(dataClean$medal)
#NAs are okay here, it just means an athlete didn't medal.

length(which(is.na(dataClean$region)))
unique(dataClean[which(is.na(dataClean$region)),c(7,8,16,17)])
#for now, set NA region to be noc value
dataClean[which(is.na(dataClean$region)),]$region <- dataClean[which(is.na(dataClean$region)),]$noc

testNOCR <- data.frame(dataClean[,c(8,16)])
testNOCR <- unique(testNOCR)
by_nocr <- testNOCR %>% group_by(noc, region)
dupeNOCR <- setorder(unique(by_nocr[duplicated(by_nocr$region, nmax=1),]),'region') #18 dupes, we'll fix by hand

dupeNOCR[1,2]
unique(dataClean[which(dataClean$region == 'Australia'),]$noc)
dataClean[which(dataClean$region == 'Australia'),]$noc <- 'AUS'

dupeNOCR[2,2]
unique(dataClean[which(dataClean$region =='Canada'),]$noc)
dataClean[which(dataClean$region == 'Canada'),]$noc <- 'CAN'

dupeNOCR[3,2]
unique(dataClean[which(dataClean$region =='China'),]$noc)
dataClean[which(dataClean$region == 'Canada'),]$noc <- 'CHN'

dupeNOCR[4,2]
unique(dataClean[which(dataClean$region =='Czech Republic'),]$noc)
dataClean[which(dataClean$region == 'Czech Republic'),]$noc <- 'CZE'

dupeNOCR[5:7,2]
unique(dataClean[which(dataClean$region =='Germany'),]$noc)
dataClean[which(dataClean$region == 'Germany'),]$noc <- 'GER'

dupeNOCR[8,2]
unique(dataClean[which(dataClean$region =='Malaysia'),]$noc)
dataClean[which(dataClean$region == 'Malaysia'),]$noc <- 'MAL'

dupeNOCR[9:10,2]
unique(dataClean[which(dataClean$region =='Russia'),]$noc)
dataClean[which(dataClean$region == 'Malaysia'),]$noc <- 'RUS'

dupeNOCR[11:12,2]
unique(dataClean[which(dataClean$region =='Serbia'),]$noc)
dataClean[which(dataClean$region == 'Serbia'),]$noc <- 'SRB'

dupeNOCR[13,2]
unique(dataClean[which(dataClean$region =='Syria'),]$noc)
dataClean[which(dataClean$region == 'Syria'),]$noc <- 'SYR'

dupeNOCR[14,2]
unique(dataClean[which(dataClean$region =='Trinidad'),]$noc)
dataClean[which(dataClean$region == 'Trinidad'),]$noc <- 'TTO'

dupeNOCR[15,2]
unique(dataClean[which(dataClean$region =='Vietnam'),]$noc)
dataClean[which(dataClean$region == 'Vietnam'),]$noc <- 'VNM'

dupeNOCR[16:17,2]
unique(dataClean[which(dataClean$region =='Yemen'),]$noc)
dataClean[which(dataClean$region == 'Yemen'),]$noc <- 'YEM'

dupeNOCR[18,2]
unique(dataClean[which(dataClean$region =='Zimbabwe'),]$noc)
dataClean[which(dataClean$region == 'Zimbabwe'),]$noc <- 'RHO'


dataClass <- data
#limit to Swimming
dataClass <- dataClass[which(dataClass$sport == 'Swimming' & dataClass$sex == 'M'),]
dataClass$medal <- ifelse(is.na(dataClass$medal), 0, 1)
table(dataClass$medal)
#balance the data
dataClass_m <- dataClass[which(dataClass$medal == 1),]
dataClass_nm <- dataClass[which(dataClass$medal == 0),]
nmSamp <- sample(1:nrow(dataClass_nm), nrow(dataClass_m))
dataClass <- rbind(dataClass_m, dataClass_nm[nmSamp,])


athFreq <- data %>% dplyr::count(name, sex)
ggplot(athFreq, aes(x=n)) + geom_histogram(color='blue', fill = 'white', binwidth=1) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Histogram of Athlete Appearances') +ylab('Athlete Count') + xlab('# Appearances')
#max(athFreq$n)

data[which(data$name == athFreq[which.max(athFreq$n),1][[1]]),]

ggplot(athFreq, aes(x=n, fill=sex, color=sex)) + geom_histogram(position='dodge', alpha=0.5, binwidth = 1) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Appearances by Gender')

athStats <- unique(data[,c(2,3,4,5,6)])

athWt <- athStats %>% dplyr::count(weight, sex)

ggplot(athWt, aes(x=weight, fill=sex, color=sex)) + geom_histogram(position='dodge', alpha=0.5, binwidth = 10) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Weight by Gender')

athHt <- athStats %>% dplyr::count(height, sex)

ggplot(athHt, aes(x=height, fill=sex, color=sex)) + geom_histogram(position='dodge', alpha=0.5, binwidth = 5) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Height by Gender')

athAge <- athStats %>% dplyr::count(age, sex)

ggplot(athAge, aes(x=age, fill=sex, color=sex)) + geom_histogram(position='dodge', alpha=0.5, binwidth = 5) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Age by Gender')

sportStats <- unique(data[,c(3,4,5,6,11,13)])
sportF <- sportStats[which(sportStats$sex == 'F' & sportStats$season == 'Winter'),]

sportWt <- sportF %>% dplyr::count(weight, sport)

ggplot(sportWt, aes(x=weight, fill=sport, color=sport)) + geom_density(alpha=.1) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Weight Distribution by Sport')

sportWt2 <- sportWt[which(sportWt$sport %in% c('Bobsleigh','Ski Jumping')),]

ggplot(sportWt2, aes(x=weight, fill=sport, color=sport)) + geom_density(alpha=.1) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Weight Distribution by Sport \n Bobsleigh & Speed Skating')

counts <- data %>% 
  dplyr::group_by(year, season) %>%
  dplyr::summarize(
    athletes = length(unique(id)),
    nations = length(unique(noc)),
    events = length(unique(event))
  )

ggplot(counts, aes(x=year, y=athletes, group=season, color=season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("dark blue","dodgerblue")) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Athlete Count by Season')

ggplot(counts, aes(x=year, y=nations, group=season, color=season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("dark blue","dodgerblue")) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Nation Count by Season')

ggplot(counts, aes(x=year, y=events, group=season, color=season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("dark blue","dodgerblue")) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Event Count by Season')

dataAR <- data
dataAR$medal[is.na(dataAR$medal)] <- 0

dataAR$Medal1 <- gsub("Gold", "1", dataAR$medal)
dataAR$Medal1 <- gsub("Silver", "1", dataAR$Medal1)
dataAR$Medal1 <- gsub("Bronze", "1", dataAR$Medal1)

MedaldataAR <- cbind(dataAR$age, dataAR$height, dataAR$weight, dataAR$noc, dataAR$medal,dataAR$Medal1)
colnames(MedaldataAR) <- c("Age", "Height", "Weight", "Team", "Medal", "Medal1")

MD <- as.data.frame(MedaldataAR[complete.cases(MedaldataAR),])
MD$Medal=factor(MD$Medal)
MD$Team=factor(MD$Team)

MD1 <- MD[,-6]
n <- round(nrow(MD1)/5)
s <- sample(1:nrow(MD1), n)
## The test set is the sample
MD1Train <- MD1[s,]
## The training set is the not sample
MD1Test <- MD1[-s,]

MD1Test_no_labels <- MD1Test[,-5]
MD1Test_labels <- MD1Test[,5]
MD1Train_no_labels <- MD1Train[,-5]
MD1Train_labels <- MD1Train[,5]

MD2 <- MD[,-5]
n2 <- round(nrow(MD2)/5)
s2 <- sample(1:nrow(MD2), n2)
## The test set is the sample
MD2Train <- MD2[s2,]
## The trainng set is the not sample
MD2Test <- MD2[-s2,]

MD2Test_no_labels <- MD2Test[,-5]
MD2Test_labels <- MD2Test[,5]
MD2Train_no_labels <- MD2Train[,-5]
MD2Train_labels <- MD2Train[,5]

rules=arules::apriori(data=MD2, parameter = list(supp=.001, conf=.005, minlen=2),
                      appearance = list(default="rhs", lhs=c("Medal1=1")),
                      control=list(verbose=FALSE))
arules::inspect(rules)

NoMedals <- MD1[,-5]
NoMedals$Age<-as.character(NoMedals$Age)
NoMedals$Age<-as.numeric(NoMedals$Age)
NoMedals$Height<-as.character(NoMedals$Height)
NoMedals$Height<-as.numeric(NoMedals$Height)
NoMedals$Weight<- as.character(NoMedals$Weight)
NoMedals$Weight<- as.numeric(NoMedals$Weight)
x<-as.data.frame(NoMedals)
x<- x[,-4]

ggplot(x, aes(y=x$Age)) + geom_boxplot() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Boxplot of Age') +ylab('Age')
ggplot(x, aes(y=x$Height)) + geom_boxplot() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Boxplot of Height') +ylab('Height')
ggplot(x, aes(y=x$Weight)) + geom_boxplot() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Boxplot of Weight') +ylab('Age')

x$WeightNorm <-(x$Weight-mean(x$Weight))/sd(x$Weight)
x$HeightNorm <-(x$Height-mean(x$Height))/sd(x$Height)

xkm <- kmeans(x[4:5], 
              centers= 4)

## Find cluster size
clus.size <-xkm$size
clus <- data.frame(table(xkm$cluster))
## Plot a Bar chart for cluster size
names(clus) <- c("Cluster","Size")

dataClust <- data[,c(3,4,5,6,13)]
dataClust <- dataClust[which(dataClust$sport %in% c('Basketball','Speed Skating')),]
dataClust <- dataClust[which(dataClust$sex == 'F'),]
#Normalize values
dataClust$age <- (dataClust$age - min(dataClust$age))/(max(dataClust$age) - min(dataClust$age))
dataClust$height <- (dataClust$height - min(dataClust$height))/(max(dataClust$height) - min(dataClust$height))
dataClust$weight <- (dataClust$weight - min(dataClust$weight))/(max(dataClust$weight) - min(dataClust$weight))
rownames(dataClust) <- paste(rownames(dataClust),dataClust$sport)
dataClust <- dataClust[sample(1:nrow(dataClust), 0.025*nrow(dataClust)),]
dataClust <- as.matrix(dataClust[,c(-1,-5)])

distE <- dist(dataClust,method='euclidean')
distC <- dist(dataClust,method='cosine')

groupsE <- hclust(distE,method='ward.D')
plot(groupsE,cex=.5,pin=c(10,10))
rect.hclust(groupsE,2)

groupsC <- hclust(distC,method='ward.D')
plot(groupsC,cex=.5,pin=c(10,10))
rect.hclust(groupsE,2)

library(caret)
ctrl <- trainControl(method='cv',number=3)
grid <- expand.grid(cp = seq(0,1,by=.1))

dataDT <- dataClass[,c(4,5,6,15)]
dataDT$medal <- as.factor(dataDT$medal)

fitDT <- train(medal~., data=dataDT, trControl=ctrl, tuneGrid = grid, method='rpart')
fitDT

ctrl <- trainControl(method='cv',number=3)
grid <- expand.grid(mtry = 1:5)

dataRF <- dataClass[,c(4,5,6,15)]
dataRF$medal <- as.factor(dataRF$medal)

fitRF <- train(medal~., data = dataRF, method='rf', trControl=ctrl, tuneGrid = grid)
fitRF

ctrl <- trainControl(method='cv', number=3)
grid <- expand.grid(fL = c(0,0.001,0.01,0.1,1), usekernel = c(TRUE,FALSE), adjust = 0:5)

dataNB <- dataClass[,c(4,5,6,15)]
dataNB$medal <- as.factor(dataRF$medal)

fitNB <- train(medal~.,data=dataNB,method='nb',trControl=ctrl, tuneGrid = grid)
fitNB

dataSVM <- dataClass[,c(3,4,5,6,8,15)]

for (i in (1:3)){
  n = i+1
  max = max(dataSVM[,n])
  min = min(dataSVM[,n])
  dataSVM[,n] <- (dataSVM[,n] - min)/(max-min)
}

dataSVM <- dataSVM[,-1]
dummy <- dummyVars('~.', data=dataSVM)
dataSVM_ohe <- data.frame(predict(dummy, newdata=dataSVM))

dataSVM_ohe$medal <- as.factor(dataSVM_ohe$medal)

ctrl <- trainControl(method='cv',number=3)
grid <- expand.grid(C = c(0.1, 0.25, 0.5, 0.75, 1))

SVMlin <- train(medal~., data = dataSVM_ohe, method = 'svmLinear', trControl = ctrl, tuneGrid=grid)

SVMlin

ctrl <- trainControl(method='cv',number=3)
grid <- expand.grid(degree = c(2,3,4), scale = c(.001, .01, .1), C = c(0.1, 0.25, 0.5, 0.75, 1))

SVMpoly <- train(medal~., data = dataSVM_ohe, method = 'svmPoly', trControl = ctrl, tuneGrid = grid)

SVMpoly

ctrl <- trainControl(method='cv',number=3)
grid <- expand.grid(sigma = c(0.1, 0.25, 0.5, 0.75, 1), C = c(0.1, 0.25, 0.5, 0.75, 1))

SVMrad <- train(medal~., data = dataSVM_ohe, method = 'svmRadial', trControl = ctrl, tuneGrid = grid)

SVMrad

plot(rules[1:20],method="graph")

cp <-barplot(height=clus$Size ,
             names.arg=clus$Cluster,
             xlab="Cluster",
             ylab="Object Counts",
             main="Cluster Size",
             col=c("darkorchid1","firebrick2","darkseagreen2","goldenrod1"),
             ylim= c(0,max(clus$Size)+500 ) ,
             border=NA 
)

plot(clus, what = "classification")

dendE <- as.dendrogram(groupsE)
dendC <- as.dendrogram(groupsC)
tanglegram(dendE,dendC,k_labels=2,sort=TRUE,k_branches=2,lab.cex=1,columns_width = c(3,3,3),dLeaf=-0.01,margin_inner=10,main_left = "Euclidean Distance",main_right = "Cosine Similarity",cex_main_left = 1,lwd=1.5)

entanglement(dendlist(dendE,dendC))

preds <- predict(fitDT, dataDT[,-4], type='raw')
table(preds, dataDT$medal)

modelAcc <- data.frame(modelName = 'Decision Tree',Accuracy = max(fitDT$results$Accuracy))

preds <- predict(fitRF, dataRF[,-4], type='raw')
table(preds, dataRF$medal)

acc <- data.frame(modelName = 'Random Forest',Accuracy = max(fitRF$results$Accuracy))
modelAcc <- rbind(modelAcc, acc)

preds <- predict(fitNB, dataNB[,-4], type='raw')
table(preds, dataNB$medal)

acc <- data.frame(modelName = 'Naive Bayes',Accuracy = max(na.omit(fitNB$results$Accuracy)))
modelAcc <- rbind(modelAcc, acc)

preds <- predict(SVMlin, dataSVM_ohe[,-126], type='raw')
table(preds, dataSVM_ohe$medal)

acc <- data.frame(modelName = 'SVM (Linear Kernel)',Accuracy = max(SVMlin$results$Accuracy))
modelAcc <- rbind(modelAcc, acc)

preds <- predict(SVMpoly, dataSVM_ohe[,-126], type='raw')
table(preds, dataSVM_ohe$medal)

acc <- data.frame(modelName = 'SVM (Polynomial Kernel)',Accuracy = max(SVMpoly$results$Accuracy))
modelAcc <- rbind(modelAcc, acc)

preds <- predict(SVMrad, dataSVM_ohe[,-126], type='raw')
table(preds, dataSVM_ohe$medal)

acc <- data.frame(modelName = 'SVM (Radial Kernel)',Accuracy = max(SVMrad$results$Accuracy))
modelAcc <- rbind(modelAcc, acc)

ggplot(modelAcc, aes(x=modelName, y=Accuracy, fill=Accuracy)) + geom_bar(stat='identity') + coord_flip() + scale_x_discrete(limits=rev(modelAcc$modelName)) + scale_fill_gradientn(colors=rainbow(4))
