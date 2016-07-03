#Unit 7
WHO <- read.csv("WHO.csv")
str(WHO)
plot(WHO$GNI,WHO$FertilityRate)

library("ggplot2")
scatterplot <- ggplot(WHO,aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()
scatterplot + geom_point(color = "blue", size = 3, shape = 17)
scatterplot + geom_point(color = "red", size = 3, shape = 8)
scatterplot + geom_point(color = "red", size = 3, shape = 8) + ggtitle("Fertilty Rate vs. Gross National Income")
fertilityGNIplot <- scatterplot + geom_point(color = "red", size = 3, shape = 8) + ggtitle("Fertilty Rate vs. Gross National Income")
pdf("MyPlot.pdf")
print(fertilityGNIplot)
dev.off()

scatterplot + geom_point(color = "blue", size = 3, shape = 15)


ggplot(WHO,aes(x = GNI, y = FertilityRate, color = Region)) + geom_point(size = 5)
ggplot(WHO,aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point(size = 5)
ggplot(WHO,aes(x = FertilityRate, y = Under15)) + geom_point(size = 5)
ggplot(WHO,aes(x = log(FertilityRate), y = Under15)) + geom_point(size = 5)     
model <- lm(Under15 ~ log(FertilityRate), WHO)
summary(model)
ggplot(WHO,aes(x = log(FertilityRate), y = Under15)) + geom_point(size = 5) + stat_smooth(method = "lm")
ggplot(WHO,aes(x = log(FertilityRate), y = Under15)) + geom_point(size = 5) + stat_smooth(method = "lm", level = 0.99)
ggplot(WHO,aes(x = log(FertilityRate), y = Under15)) + geom_point(size = 5) + stat_smooth(method = "lm", se = FALSE)
ggplot(WHO,aes(x = log(FertilityRate), y = Under15)) + geom_point(size = 5) + stat_smooth(method = "lm", se = FALSE, color = "orange")

ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() + scale_color_brewer(palette="Dark2")


mvt <- read.csv("mvt.csv", stringsAsFactors = FALSE)
str(mvt)

mvt$Date <- strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt$Weekday <- weekdays(mvt$Date)
mvt$Hour <- mvt$Date$hour
str(mvt)

table(mvt$Weekday)
weekdayCounts <- as.data.frame(table(mvt$Weekday))
str(weekdayCounts)

ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1))
weekdayCounts$Var1 <- factor(weekdayCounts$Var1, ordered = TRUE, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1)) +
  xlab("Day of the week") + ylab("Total Motor Vehicle Theft")

ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), linetype = 2) +
  xlab("Day of the week") + ylab("Total Motor Vehicle Theft")

ggplot(weekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), linetype = 2, alpha = 0.3) +
  xlab("Day of the week") + ylab("Total Motor Vehicle Theft")

table(mvt$Weekday,mvt$Hour)

DayHourCounts <- as.data.frame(table(mvt$Weekday,mvt$Hour))
str(DayHourCounts)
DayHourCounts$Hour <- as.numeric(as.character(DayHourCounts$Var2))
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group = Var1))
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(color = Var1), size = 2)

DayHourCounts$Var1 <- factor(DayHourCounts$Var1, ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name = "Total MV Thefts", low = "white", high = "red") + theme(axis.title.y = element_blank())
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name = "Total MV Thefts", low = "white", high = "black") + theme(axis.title.y = element_blank())

#crime rate i chicago
install.packages("maps")
install.packages("ggmap")
library("maps")
library("ggmap")

chicago <- get_map(location = "chicago", zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))
LatLonCounts <- as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))
str(LatLonCounts)
LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))

ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size = Freq)) +
  scale_color_gradient(low = "yellow", high = "red")

ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill = "red") 

LatLonCounts2 <- subset(LatLonCounts, LatLonCounts$Freq!=0)
nrow(LatLonCounts) - nrow(LatLonCounts2)

#murders in the United States
murders <- read.csv("murders.csv")
str(murders)

statesMap <- map_data("state")
str(statesMap)
ggplot(statesMap, aes(x = long, y = lat,group = group)) + geom_polygon(fill = "white", color = "black")
murders$region <- tolower(murders$State)
murderMap <- merge(statesMap, murders, by = "region")
str(murderMap)

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + 
  scale_fill_gradient(low="black", high="red", guide = "legend")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + 
  scale_fill_gradient(low="black", high="red", guide = "legend")

murderMap$MurderRate <- murderMap$Murders/murderMap$Population*100000

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + 
  scale_fill_gradient(low="black", high="red", guide = "legend")

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + 
  scale_fill_gradient(low="black", high="red", guide = "legend", limits = c(0,10))

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color = "black") + 
  scale_fill_gradient(low="black", high="red", guide = "legend")

#Recitation - The Good, the Bad and the Ugly: Visualization Recitation
library(ggplot2)
intl <- read.csv("intl.csv")
ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = "identity") + geom_text(aes(label = PercentOfIntl))
intl <- transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)
intl$PercentOfIntl <- intl$PercentOfIntl*100
ggplot(intl, aes(x = Region, y = PercentOfIntl)) + 
  geom_bar(stat = "identity", fill = "dark blue") + 
  geom_text(aes(label = PercentOfIntl), vjust = -0.4) +
  ylab("Percent of International Students") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
  
library(ggmap)
intlall <- read.csv("intlall.csv", stringsAsFactors = FALSE)
str(intlall)
intlall[is.na(intlall)] <- 0
head(intlall)
world_map <- map_data("world")
str(world_map)

world_map <- merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
str(world_map)

ggplot(world_map, aes(x=long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") + coord_map("mercator")

#fix 1
world_map <- world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") + coord_map("mercator")

#fix 2
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)" ] = "China"
world_map <- world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y = lat, group = group)) + 
  geom_polygon(aes(fill = Total), color = "black") + 
  coord_map("mercator")

ggplot(world_map, aes(x=long, y = lat, group = group)) + 
  geom_polygon(aes(fill = Total), color = "black") + 
  coord_map("ortho", orientation = c(20,30,0))

ggplot(world_map, aes(x=long, y = lat, group = group)) + 
  geom_polygon(aes(fill = Total), color = "black") + 
  coord_map("ortho", orientation = c(-37,175,0))


library(ggplot2)
household <- read.csv("households.csv")
str(household)
library(reshape2)
households <- melt(household, id = "Year")
head(households,20)

ggplot(households, aes(x = Year, y = value, color = variable)) +
  geom_line(size = 2) + 
  geom_point(size = 5) +
  ylab("Percentage of households")
