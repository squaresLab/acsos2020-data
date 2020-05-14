library("ggplot2")
library("dplyr")

# note that by default rstudio starts in the users home directory
# need to switch to the location of the script
setwd("research/seams2020-data")

# filepath relative to the working directory (set above)
repertoire15Gen <- read.csv("repertoire15Gen.csv")

data <- repertoire15Gen

scratch <- read.csv("scratch4s.csv")
baseline <- read.csv("baseline30gen4s")
deckard <- read.csv("deckard4s.csv")
deckardcust <- read.csv("deckardcust4s.csv")
deckardcust$init <- "deckardcustom"

data <- rbind(baseline,deckard)
data <- rbind(data,deckardcust)
data <- rbind(data,scratch)


scratch <- read.csv("repertoireaws_scratch.csv")
baseline <- read.csv("repertoireaws_rep.csv")
deckardcust <- read.csv("repertoireaws_deckard_cust.csv")
trimmer <- read.csv("awstrimmer.csv")

reruns <- read.csv("aws_reruns_merged.csv")

rerep <- read.csv("aws_9_10_repertoire_rerun.csv")
rerep2 <- read.csv("aws_2_5_repertoire_rerun.csv")
rerep3 <- read.csv("aws_5_10_repertoire_rerun.csv")

rijnard <- read.csv("rijnardsawspost.csv")

rijnard5910re <- read.csv("rijnards-5-9-10-rerun.csv")
rijnard25re <- read.csv("rijnards-2-5-rerun.csv")

rijnard$init <- rijnard$transformID

rijnard <- rijnard[,-c(17)]

data <- rbind(scratch,baseline)
data <- rbind(data,deckardcust)
data <- rbind(data,trimmer)

data <- rbind(data,rijnard)

# remove the data from buggy runs for replacement
data <- subset(data,(data$trial != 2 | data$scenarioMutations != 5))
data <- subset(data,(data$trial != 5 | data$scenarioMutations != 10))
data <- subset(data,(data$trial != 9 | data$scenarioMutations != 10))
               
data <- rbind(data,reruns)
data <- rbind(data,rerep)
data <- rbind(data,rerep2)
data <- rbind(data,rerep3)

# rijnard exp replacements
rijnardrep <- rbind(rijnard5910re,rijnard25re)
rijnardrep$init <- rijnardrep$transformID

rijnardrep <- rijnardrep[,-c(17)]

data <- rbind(data,rijnardrep)

# select only the positive results from rijnard results
data <- subset(data, !(data$init %in% c('prune-try-take-second','prune-sequence-take-first','prune-sequence-take-second','decrement-for','scratch','trimmer')))

data <- subset(data,data$scenarioMutations == 10)

# colorblind color scheme
cbPalette <- c("#47242B","#5A607C", "#3EAA9A", "#C3E270", "#A18E7B")

data <- subset(data,data$init %in% c("scratch","trimmer","repertoire"))

datacat <- data
# convert generation  to a factor to enable box plots
datacat$generation <- as.factor(datacat$generation)

# boxplot of utility over generation by initial population
#A4 size
p <- ggplot(data=datacat, aes(x=generation,y=profit,color=init))
p <- p + ylab("Utility") + xlab("Generation") + scale_fill_discrete(name="Initial Population")  + theme_bw()
p <- p + theme(text=element_text(size=27), title=element_text(size=30,face="bold"),legend.position=c(.8,.4),legend.title=element_text(size=30,face="bold"),legend.text=element_text(size=25),legend.key.size=unit(0.75,"in"))
p + scale_color_manual(values=cbPalette,name="Starting Plan") +  geom_line(lwd=2,fatten=0.5,position=position_dodge(width=.95))  + coord_cartesian(xlim=c(1,29.75)) + facet_grid(scenarioMutations~trial) #+ geom_hline(aes(yintercept = 4938.98986581),lwd=1.25)

# take the median by generation and initial population
aggdata <- data[,c("generation","runtime","profit")]
aggdata <- aggregate(aggdata,by=list(aggdata$generation,data$init,data$trial,data$scenarioMutations), FUN=mean,na.rm=TRUE)

#aggdata <- data[,c("generation","runtime","profit")]
#aggdata <- aggregate(aggdata,by=list(aggdata$generation,data$init), FUN=mean,na.rm=TRUE)


# calculate cumulative runtime
aggdata <- aggdata %>%
  group_by(Group.2,Group.3,Group.4) %>%
  mutate(cumruntime = cumsum(runtime))

#aggdata <- aggdata %>%
#  group_by(Group.2) %>%
#  mutate(cumruntime = cumsum(runtime))


aggdata <- subset(aggdata,aggdata$cumruntime < 20*1000)

aggdata <- subset(aggdata,aggdata$Group.4==10)

cbPalette <- c("#a6dba0","#7b3294","#c2a5cf","#008837")

# size = 18x6
# utility over cumulative runtime by initial population
p <- ggplot(data=aggdata, aes(y=profit,x=cumruntime/1000,color=Group.2))
p <- p +  theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=18), title=element_text(size=18,face="bold"),legend.title=element_text(size=18,face="bold"),legend.text=element_text(size=16),legend.key.size=unit(0.3,"in"),legend.position=c(.7,.6))
p + geom_line(lwd=2)   + scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + facet_grid(Group.4~Group.3)+ coord_cartesian(xlim=c(1,60)) +scale_colour_manual(values=cbPalette,name="Initial Population")
#+ coord_cartesian(xlim=c(0.5,125)) 

p <- ggplot(data=aggdata, aes(y=profit,x=cumruntime/1000,color=Group.2))
p <- p +  theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=18), title=element_text(size=18,face="bold"),legend.title=element_text(size=18,face="bold"),legend.text=element_text(size=16),legend.key.size=unit(0.3,"in"),legend.position=c(.7,.3))
p + geom_line(lwd=2)   + scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + facet_grid(Group.4~Group.3)+ coord_cartesian(xlim=c(1,60)) +scale_colour_manual(values=cbPalette,name="Initial Population")#, ylim=c(27500000,30000000)) 
#scale_colour_manual(values=cbPalette,name="Initial Population")

# colorblind color scheme
#cbPalette <- c("#47242B","#5A607C", "#C3E270", "#3EAA9A", "#A18E7B")
cbPalette <- c("#762a83","#af8dc3","#e7d4e8","#d9f0d3","#7fbf7b","#1b7837")

p <- ggplot(data=aggdata, aes(y=profit,x=cumruntime/1000,color=Group.2))
p <- p +  theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=18), title=element_text(size=18,face="bold"),legend.title=element_text(size=18,face="bold"),legend.text=element_text(size=16),legend.key.size=unit(0.3,"in"),legend.position=c(.7,.7))
p + geom_line(lwd=2)   + scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + facet_grid(Group.4~Group.3)+ coord_cartesian(xlim=c(1,300)) +scale_colour_manual(values=cbPalette,name="Initial Population")#, ylim=c(27500000,30000000)) 
#scale_colour_manual(values=cbPalette,name="Initial Population")

p <- ggplot(data=aggdata, aes(y=profit,x=cumruntime/1000,color=Group.2))
p <- p +  theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=18), title=element_text(size=18,face="bold"),legend.title=element_text(size=18,face="bold"),legend.text=element_text(size=16),legend.key.size=unit(0.3,"in"),legend.position=c(.7,.3))
p + geom_line(lwd=2)   + scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + facet_grid(Group.4~Group.3)+ coord_cartesian(xlim=c(1,60)) 
#scale_colour_manual(values=cbPalette,name="Initial Population")

p <- ggplot(data=aggdata, aes(y=profit,x=cumruntime/1000,color=Group.2))
p <- p +  theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=18), title=element_text(size=18,face="bold"),legend.title=element_text(size=18,face="bold"),legend.text=element_text(size=16),legend.key.size=unit(0.3,"in"),legend.position=c(.7,.3))
p + geom_line(lwd=2)   + scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + coord_cartesian(xlim=c(1,240)) 
#scale_colour_manual(values=cbPalette,name="Initial Population")


# here are the transforms graphs
# 18 x 12
# line graph utility over generation by initial population
p <- ggplot(data=aggdata, aes(y=profit,x=cumruntime/1000,color=Group.2))
p <- p +  theme_bw() + xlab("Cumulative Evaluation Time (seconds)") + ylab("Utility") + scale_color_discrete(name="Initial Population") #+ coord_cartesian(xlim=c(0, 20))
p <- p + theme(text=element_text(size=18), title=element_text(size=18,face="bold"),legend.title=element_text(size=18,face="bold"),legend.text=element_text(size=16),legend.key.size=unit(0.3,"in"),legend.position=c(.7,.25))
p + geom_line(lwd=2)   + scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + facet_grid(Group.4~Group.3)+ coord_cartesian(xlim=c(1,60)) +scale_colour_manual(values=cbPalette,name="Initial Population")
