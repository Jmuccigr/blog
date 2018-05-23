# Load and clean 11-year average data
lead11 <- read.csv("~/Downloads/lead_11.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, na.strings="")
lead11[,3] <- as.numeric(lead11[,3])
lead11[4] <- lead11[2]
colnames(lead11)[4] <- colnames(lead11)[2]
lead11[2] <- 1950-lead11[1]
colnames(lead11) <- c("Years bp", "Year", "11-yr median filtered estimated Pb Emissions (kt/a)", "non-background Pb Flux (µg/m2/a)" )

# Load pg/g data
lead <- read.csv("~/Downloads/lead_pgg.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, na.strings="")
lead[3] <- lead[2]
lead[2] <- 1950-lead[1]
colnames(lead) <- c("Years bp", "Year", "pg/g")

# Load one of the two datasets
leaddata <- lead11
leaddata <- lead

# Set max y-axis value to resemble article's choice
ymax <- floor(max(leaddata[3], na.rm=TRUE))

# Grab the whole dataset
startDate <- min(leaddata[,2])
endDate <- max(leaddata[,2])
# ...or grab just a subset of it
startDate <- 0 - .5
endDate <- 100 - .5
# Give the chart an appropriate title
title <- "First Century"

startRow <- which(leaddata[,2] == startDate)
endRow <- which(leaddata[,2] == endDate)

# Enter some reasonable number of tick marks on the x-axis
spacing <- 10
gap <- endDate - startDate
if ( gap > 200 ) spacing <- 20
if ( gap > 500 ) spacing <- 50
if ( gap > 800 ) spacing <- 100
marks <- seq(-1300, 900, by=spacing)

plot(leaddata[startRow:endRow,2], leaddata[startRow:endRow,3], type="l", xlab=colnames(leaddata[2]), ylab=colnames(leaddata[3]), ylim=c(0,ymax), axes=FALSE, main=title)
abline(v=marks, col="lightgray")
axis(1, at=marks, las=2, cex.axis=.6, pos=0)
axis(2, at=seq(0, 5, by=.5), las=2, pos = startDate + 0.5)
