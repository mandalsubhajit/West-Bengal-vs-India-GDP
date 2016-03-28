setwd("D:\\Github\\West-Bengal-vs-India-GDP")

gdp.data <- read.csv("GDP Comparison India vs WB.csv")

####### Creation of Data for Ribbon Plot (Shading between lines) #######
require(reshape2)
require(plyr)
data <- melt(gdp.data, id.vars="Year")
data = ddply(data, c("variable"), function(x) data.frame(approx(x$Year, x$value, xout=seq(min(x$Year), max(x$Year), length.out=1000))))
names(data)[3] = "value"

ribbon.data = ddply(data, c("x"), summarize, ymin=min(value), ymax=max(value))

# For Dynamic Coloring of Area between Lines (Different shades for Positive and Negative)
# The following function divides the chart into segments based on the relative position of the lines
GetSegs <- function(x) {
  segs = x[x$variable=='West.Bengal', ]$value >= x[x$variable=='India', ]$value
  segs.rle = rle(segs)
  
  on.top = ifelse(segs, 'West.Bengal', 'India')
  on.top[is.na(on.top)] = 'West.Bengal'
  
  group = rep.int(1:length(segs.rle$lengths), times=segs.rle$lengths)
  group[is.na(segs)] = NA
  
  data.frame(x=unique(x$x), group, on.top)
}

groups = ddply(data, NULL, GetSegs)
ribbon.data = join(ribbon.data, groups)

####### End of Creation of Data for Ribbon Plot #######

####### Creation of Political Regime Data #######

left_rect <- data.frame(xmin=1977, xmax=2010, ymin=-Inf, ymax=Inf)
tmc_rect <- data.frame(xmin=2010, xmax=2016, ymin=-Inf, ymax=Inf)

####### End of Creation of Political Regime Data #######

# open file for saving our plot
jpeg(filename="WBvsIndia.jpeg", width=1600, height=900)

# Now we are ready to plot our data
require(ggplot2)
require(scales)
ggplot(data=gdp.data, aes(x=Year)) +
  # show political regimes
  geom_rect(data=left_rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="red",
            alpha=0.2,
            inherit.aes = FALSE) +
  geom_rect(data=tmc_rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="green",
            alpha=0.2,
            inherit.aes = FALSE) +
  annotate(geom="text", x=1995, y=0, label= "Left Regime", size=10) + 
  annotate(geom="text", x =2013, y=0, label = "TMC Regime", size=10) +
  # plot lines for the state and national GDP growth
  geom_line(aes(x=Year, y = West.Bengal, colour="West Bengal"), size=2) +
  geom_line(aes(x=Year, y = India, colour="India")) +
  scale_colour_manual("", 
                      breaks = c("West Bengal", "India"),
                      values = c("West Bengal"="blue", "India"="black")) +
  labs(y="GDP Growth Rate") +
  scale_y_continuous(labels=percent) +
  # create the ribbon plot for coloring the area between lines
  geom_ribbon(data=ribbon.data, aes(x=x, ymin=ymin, ymax=ymax, group=group, fill=on.top), alpha=0.7) +
  scale_fill_manual(values = c("West.Bengal" = "white", "India" = "black")) +
  # style and font adjustment
  guides(fill=FALSE) +
  theme(legend.position="top", legend.text=element_text(size=25),
        text = element_text(size=20))

# close the file
dev.off()