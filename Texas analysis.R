library(readr)
TexasMerge <- read_csv("~/MAR 653/TexasMerge.csv")
colnames(TexasMerge)

rbind( table(is.na(TexasMerge)) , round(prop.table(table(is.na(TexasMerge)))*100,1) )
str(TexasMerge)

MSP <- aggregate(ZHVI_AllHomes ~Year, TexasMerge, mean)

library(ggplot2)
#install.packages("wesanderson")
names(wesanderson::wes_palettes)
library(wesanderson)
gbp1<-wes_palette("GrandBudapest2")[1]
sl1 <-ggplot(MSP, aes(x=as.factor(Year), y=ZHVI_AllHomes))+
  geom_line(color=gbp1, aes(group=1), size=1.5)+
  geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="Texas Growth of Sale Prices by year", x=NULL, y="Sale Price")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$ZHVI_AllHomes)/MSP[-nrow(MSP),]$ZHVI_AllHomes)

sl2 <-ggplot(MSP, aes(x=as.factor(Year), y=rate))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  #geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
  labs(title="Change rate of Sale Price", x="Year", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()
library(gridExtra)
grid.arrange(sl1,sl2)


Year_county<-aggregate(ZHVI_AllHomes ~Year+County, TexasMerge,mean)

library(RColorBrewer)
pal<-rep(brewer.pal(10, "BrBG"),5)

ggplot(Year_county, aes(group = County ))+
  geom_line(aes(x=Year,y=ZHVI_AllHomes,color=County), alpha=0.5, show.legend=F)+
  labs(title="The Trend of House Price in Texas by County", x=NULL)+
  theme(panel.background=element_rect(fill = "white"),
        plot.title=element_text(vjust=3, size=15),
        panel.grid.major=element_line(color = "gray90"))

summary(TexasMerge)


library(Rcmdr)
RegModel.1 <- 
  lm(ZHVI_AllHomes~per.capita.personal.income+personal.income+population, 
     data=Dataset)
summary(RegModel.1)

RegModel.2 <- 
  lm(ZHVI_AllHomes~per.capita.personal.income+personal.income+population+RegionName,
     data=Dataset)
summary(RegModel.2)

RegModel.3 <- 
  lm(ZHVI_AllHomes~per.capita.personal.income+personal.income+population+Year,
     data=Dataset)
summary(RegModel.3)

RegModel.4 <- 
  lm(ZHVI_AllHomes~per.capita.personal.income+personal.income+population+RegionName+Year,
     data=Dataset)
summary(RegModel.4)
cluster <-  KMeans(model.matrix(~-1 + per.capita.personal.income + 
                                  personal.income + population, Dataset), centers = 5, iter.max = 10, 
                   num.seeds = 10)

