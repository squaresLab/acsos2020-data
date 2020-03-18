library("dplyr")
library("ggplot2")

postproc <- read.csv("~/research/seams2020-data/postproc.csv")

postproc <- read.csv("~/research/seams2020-data/postprocmerge3.csv")

postproc <- read.csv("~/research/seams2020-data/postproc3repgen30.csv")

postproc <- read.csv("~/research/seams2020-data/semanticsaltering-25gen.csv")

postproc <- read.csv("~/research/seams2020-data/transformsgen30.csv")

baseline.5bulid <- read.csv("~/research/seams2020-data/baseline30gen0.5build.csv")

baseline.5bulid$transformID <- "baseline.5"

baseline1build<- read.csv("~/research/seams2020-data/baseline30gen1.0build")

baseline1build$transformID <- "baseline1"

baseline.95build <- read.csv("~/research/seams2020-data/baseline30gen0.95build")

baseline.95build$transformID <- "baseline.95"


postproc <- rbind(postproc,baseline.5bulid)

postproc <- rbind(postproc,baseline1build)

postproc <- rbind(postproc,baseline.95build)


baseline30gen4s <- read.csv("~/research/seams2020-data/baseline30gen4s")
baseline30gen4s$transformID <- "baseline4s"

transforms4s <- read.csv("~/research/seams2020-data/transforms4s.csv")

postproc <- rbind(baseline30gen4s,transforms4s)

#s0 <- subset(postproc,postproc$trial == 5)

#data <- s0

#aggdata <- data[,c("runtime","profit")]

#ggdata <- aggregate(aggdata,by=list(data$scenarioMutations,data$transformID), FUN=mean,na.rm=TRUE)

#aggdata <- aggdata %>%
#  group_by(Group.2) %>%
#  mutate(cumruntime = cumsum(runtime))

data <- postproc

aggdata2 <- aggregate(data$profit, by=list(data$Group.2,data$Group.4,data$Group.3), max)

aggdata3 <- aggregate(aggdata2$x, by=list(aggdata2$Group.1), mean)
aggdata3 <- aggregate(aggdata2$x, by=list(aggdata2$Group.1,aggdata2$Group.2), mean)


aggdata3

aggdata3srt <-aggdata3[order(aggdata3$Group.2,-aggdata3$x),]
aggdata3srt <-aggdata3[order(-aggdata3$x),]


aggdata3srt 

p <- ggplot(aggdata2srt, aes(x=reorder(Group.1,-x), y=x))
p + geom_bar(stat='identity') + facet_wrap(~Group.2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
