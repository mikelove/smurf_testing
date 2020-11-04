library(ggplot2)
library(dplyr)
l <- lapply(list.files(path="csv", pattern="*.csv", full=TRUE), function(f) {
  read.csv(f, header=FALSE)
})
dat <- do.call(rbind, l)
colnames(dat) <- c("type","numUniq","time","n","cnt")
dat$numUniq <- factor(dat$numUniq, 1:5)

dat %>% group_by(cnt, n) %>% summarize(count=n()) %>% data.frame

ggplot(dat, aes(x=numUniq,fill=type)) +
  geom_bar(stat="count",position="dodge") +
  facet_grid(n ~ cnt, labeller = label_both)
ggplot(dat, aes(x=type, y=time)) +
  geom_boxplot(outlier.color=NA) +
  geom_jitter(width=.1) +
  facet_grid(n ~ cnt, labeller = label_both)
