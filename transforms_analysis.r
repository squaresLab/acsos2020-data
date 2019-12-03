library("dplyr")
library("ggplot2")

postproc <- read.csv("~/research/seams2020-data/postproc.csv")

postproc <- read.csv("~/research/seams2020-data/postprocmerge.csv")

s0 <- subset(postproc,postproc$trial == 9)

data <- s0

aggdata <- data[,c("generation","runtime","profit")]

aggdata <- aggregate(aggdata,by=list(aggdata$generation,data$transformID), FUN=median,na.rm=TRUE)

aggdata <- aggdata %>%
  group_by(Group.2) %>%
  mutate(cumruntime = cumsum(runtime))

aggdata2 <- aggregate(data$profit, by=list(data$transformID), max)

aggdata2[order(-aggdata2$x),]

# colorblind color scheme
cbPalette <- c("#47242B","#5A607C", "#3EAA9A", "#C3E270", "#A18E7B")

p <- ggplot(data=aggdata, aes(y=profit,x=Group.1,color=Group.2))
p <- p +  theme_bw() + xlab("Generation") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(1,"in"))
p + geom_line(lwd=1.5)
