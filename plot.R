library(ggplot2)
l <- lapply(list.files(pattern="*.csv"), function(f) {
  read.csv(f, header=FALSE)
})
dat <- do.call(rbind, l)
colnames(dat) <- c("type","numUniq","time","n","cnt")
dat$numUniq <- factor(dat$numUniq, 2:5)
ggplot(dat, aes(x=numUniq,fill=type)) +
  geom_bar(stat="count",position="dodge") +
  facet_grid(n ~ cnt, labeller = label_both)
ggplot(dat, aes(x=type, y=time)) +
  geom_boxplot(outlier.color=NA) +
  geom_jitter(width=.1) +
  facet_wrap(~ n, labeller = label_both)
