## Course Project 2 for Reproducible Research
## Will produce an R Markdown file published on rpubs.com

## Load R packages
library(plyr)
library(lattice)

## Download the data and read the CSV file
if(!file.exists('repdata-data-StormData.csv.bz2')){
        download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', destfile='repdata-data-StormData.csv.bz2', method='curl')
}
if(!exists('storm_data')){
        storm_data <- read.csv('repdata-data-StormData.csv.bz2')  
}

#----------------------------------------------------------------------------------------
## Across the United States, which types of events are most harmful to population health?

# Create the health data frame by summing the fatalities and injuries for each event type
healthDF <- ddply(storm_data, .(EVTYPE), summarize, Fatalities=sum(FATALITIES), Injuries=sum(INJURIES))

# Order the health data frame by decreasing number of fatalities and injuries
healthDF1 <- healthDF[order(healthDF$Fatalities, healthDF$Injuries, decreasing=TRUE),]
healthDF2 <- healthDF[order(healthDF$Injuries, healthDF$Fatalities, decreasing=TRUE),]

# Take only the 20 most harmful event types
healthDF1 <- healthDF1[1:20,]
healthDF2 <- healthDF2[1:20,]

# Create x-axis labels based on the row order in the health data frames (i.e. plot most harmful -> least harmful)
labels1 <- factor(healthDF1$EVTYPE, levels = unique(healthDF1$EVTYPE))
labels2 <- factor(healthDF2$EVTYPE, levels = unique(healthDF2$EVTYPE))

# Plot 1 - Fatalities vs. Event Type
png('Fatalities.png', width=800, height=600)
print(xyplot(healthDF1$Fatalities ~ labels1, pch=19, scales=list(x=list(rot=45)), 
             main='Fatalities by Event Type', xlab='Event Type', ylab='Number of Fatalities',
       panel=function(x, y, ...) {
               panel.xyplot(x, y, ...);
               ltext(x=x, y=y, labels=healthDF1$Fatalities, pos=3, offset=1, cex=0.8)
       }))
dev.off()

# Plot 2 - Injuries vs. Event Type
png('Injuries.png', width=800, height=600)
print(xyplot(healthDF2$Injuries ~ labels2, pch=19, scales=list(x=list(rot=45)), 
             main='Injuries by Event Type', xlab='Event Type', ylab='Number of Injuries',
       panel=function(x, y, ...) {
                     panel.xyplot(x, y, ...);
                     ltext(x=x, y=y, labels=healthDF2$Injuries, pos=3, offset=1, cex=0.8)
             }))
dev.off()

#----------------------------------------------------------------------------------------
## Across the United States, which types of events have the greatest economic consequences?
#  Note that the columns PROPDMGEXP and CROPDMGEXP indicate the unit (thousands [K], millions [M], billions [B] of $)

#  Convert the thousands, millions of $ to billions of $, create new columns with the converted data called 'Property' and 'Crops'
data_conv <- within(storm_data, Property <- PROPDMG*(PROPDMGEXP=='K')/1000000)
data_conv <- within(data_conv,  Property <- ifelse(PROPDMGEXP=='M', PROPDMG/1000, Property))
data_conv <- within(data_conv,  Property <- ifelse(PROPDMGEXP=='B', PROPDMG, Property))
data_conv <- within(data_conv,  Crops <- CROPDMG*(CROPDMG=='K')/1000000)
data_conv <- within(data_conv,  Crops <- ifelse(CROPDMGEXP=='M', CROPDMG/1000, Crops))
data_conv <- within(data_conv,  Crops <- ifelse(CROPDMGEXP=='B', CROPDMG, Crops))

econDF <- ddply(data_conv, .(EVTYPE), summarize, Prop=sum(Property), Crop=sum(Crops))

econDF1 <- econDF[order(econDF$Prop, econDF$Crop, decreasing=TRUE),]
econDF2 <- econDF[order(econDF$Crop, econDF$Prop, decreasing=TRUE),]

econDF1 <- econDF1[1:15,]
econDF2 <- econDF2[1:15,]

labels1 <- factor(econDF1$EVTYPE, levels = unique(econDF1$EVTYPE))
labels2 <- factor(econDF2$EVTYPE, levels = unique(econDF2$EVTYPE))

png('Damage.png', width=1000, height=550)
par(mfrow=c(1,2), mar=c(9,5,2,1))
plot(labels1, econDF1$Prop, xaxt='n', ylab='Property damage (billions of $)', main='Property Damage (billions of $) by Event Type')
points(labels1, econDF1$Prop, pch=19, col='red')
text(labels1, econDF1$Prop, labels=round(econDF1$Prop,2), cex= 0.8, pos=3)
axis <- axis(1, las=3, at=labels1, labels=tolower(labels1))

plot(labels2, econDF2$Crop, type='n', xaxt='n', ylab='Crop damage (billions of $)', main='Crop Damage (billions of $) by Event Type')
points(labels2, econDF2$Crop, pch=19, col='red')
text(labels2, econDF2$Crop, labels=round(econDF2$Crop,2), cex= 0.8, pos=3)
axis(1, las=3, at=seq(econDF2[,1]), labels=tolower(labels2))
dev.off()
