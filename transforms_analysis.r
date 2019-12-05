library("dplyr")
library("ggplot2")

postproc <- read.csv("~/research/seams2020-data/postproc.csv")

postproc <- read.csv("~/research/seams2020-data/postprocmerge3.csv")

#s0 <- subset(postproc,postproc$trial == 5)

#data <- s0

data <- postproc

aggdata <- data[,c("runtime","profit")]

aggdata <- aggregate(aggdata,by=list(data$scenarioMutations,data$transformID), FUN=mean,na.rm=TRUE)

aggdata <- aggdata %>%
  group_by(Group.2) %>%
  mutate(cumruntime = cumsum(runtime))

aggdata2 <- aggregate(data$profit, by=list(data$transformID,data$scenarioMutations), max)

aggdata2srt <-aggdata2[order(aggdata2$Group.2,-aggdata2$x),]

aggdata2srt 

p <- ggplot(aggdata2srt, aes(x=reorder(Group.1,-x), y=x))
p + geom_bar(stat='identity') + facet_wrap(~Group.2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))