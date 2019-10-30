library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.6")
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.6")

repertoire15Gen <- read.csv("~/research/seams2020-data/repertoire15Gen.csv")

data <- repertoire15Gen

# colorblind color scheme
cbPalette <- c("#47242B","#5A607C", "#3EAA9A", "#C3E270", "#A18E7B")

datacat <- data
datacat$generation <- as.factor(datacat$generation)

#A4 size
p <- ggplot(data=datacat, aes(x=generation,y=profit,color=init))
p <- p + ylab("Utility") + xlab("Generation") + scale_fill_discrete(name="Initial Population")  + theme_bw()
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.7),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + scale_color_manual(values=cbPalette,name="Starting Plan") +  geom_boxplot(lwd=2,fatten=0.5,,position=position_dodge(width=.95))  + coord_cartesian(xlim=c(1,29.75)) #+ geom_hline(aes(yintercept = 4938.98986581),lwd=1.25)

aggdata <- data[,c("generation","runtime","profit")]

aggdata <- aggregate(aggdata,by=list(aggdata$generation,data$init), FUN=median,na.rm=TRUE)

aggdata <- aggdata %>%
  group_by(Group.2) %>%
  mutate(cumruntime = cumsum(runtime))

# eval time / generation
p <- ggplot(data=aggdata, aes(y=profit,x=cumruntime/1000,color=Group.2))
p <- p +  theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(1,"in"))
p + scale_colour_manual(values=cbPalette,name="Starting Plan") + geom_line(lwd=1.5)   + coord_cartesian(xlim=c(0.5,60))

p <- ggplot(data=aggdata, aes(y=profit,x=Group.1,color=Group.2))
p <- p +  theme_bw() + xlab("Generation") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.5),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(1,"in"))
p + scale_colour_manual(values=cbPalette,name="Initial Population") + geom_line(lwd=1.5)