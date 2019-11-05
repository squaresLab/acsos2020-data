library("ggplot2")
library("dplyr")

# note that by default rstudio starts in the users home directory
# need to switch to the location of the script
setwd("research/seams2020-data")

# filepath relative to the working directory (set above)
repertoire15Gen <- read.csv("repertoire15Gen.csv")

data <- repertoire15Gen

# colorblind color scheme
cbPalette <- c("#47242B","#5A607C", "#3EAA9A", "#C3E270", "#A18E7B")

datacat <- data
# convert generation  to a factor to enable box plots
datacat$generation <- as.factor(datacat$generation) 

# boxplot of utility over generation by initial population
#A4 size
p <- ggplot(data=datacat, aes(x=generation,y=profit,color=init))
p <- p + ylab("Utility") + xlab("Generation") + scale_fill_discrete(name="Initial Population")  + theme_bw()
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.7),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + scale_color_manual(values=cbPalette,name="Starting Plan") +  geom_boxplot(lwd=2,fatten=0.5,,position=position_dodge(width=.95))  + coord_cartesian(xlim=c(1,29.75)) #+ geom_hline(aes(yintercept = 4938.98986581),lwd=1.25)

# take the median by generation and initial population
aggdata <- data[,c("generation","runtime","profit")]
aggdata <- aggregate(aggdata,by=list(aggdata$generation,data$init), FUN=median,na.rm=TRUE)

# calculate cumulative runtime
aggdata <- aggdata %>%
  group_by(Group.2) %>%
  mutate(cumruntime = cumsum(runtime))

# utility over cumulative runtime by initial population
p <- ggplot(data=aggdata, aes(y=profit,x=cumruntime/1000,color=Group.2))
p <- p +  theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(1,"in"))
p + scale_colour_manual(values=cbPalette,name="Starting Plan") + geom_line(lwd=1.5)   + coord_cartesian(xlim=c(0.5,60))

# line graph utility over generation by initial population
p <- ggplot(data=aggdata, aes(y=profit,x=Group.1,color=Group.2))
p <- p +  theme_bw() + xlab("Generation") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(1,"in"))
p + scale_colour_manual(values=cbPalette,name="Initial Population") + geom_line(lwd=1.5)