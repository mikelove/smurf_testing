cmd_args=commandArgs(TRUE)

n <- as.numeric(cmd_args[1])
cnt <- as.numeric(cmd_args[2])
out <- cmd_args[3]

library(smurf)
library(pbapply)

ans <- pbsapply(1:200, function(i) {
  set.seed(i)
  k <- 5
  size <- rep(rep(c(2,cnt),each=n/2), times=k)
  p <- rep((3 + c(-1,0,0,1,1))/6, each=n)
  y <- rbinom(k*n, prob=p, size=size)
  r <- y/size
  x <- factor(rep(1:k,each=n))
  f <- r ~ p(x, pen="gflasso", refcat="1")
  # binomial
  t <- system.time({
    try1 <- tryCatch({
      fit <- glmsmurf(formula=f, family=binomial(link="logit"), data=data.frame(x,r),
                      weights=size, pen.weights="glm.stand", lambda="cv1se.dev", 
                      control=list(lambda.length=20L, k=5, ncores=1));
      TRUE
      }, error=function(e) FALSE)
  })[[3]]
  # gaussian
  t2 <- system.time({
    try2 <- tryCatch({
      fit2 <- glmsmurf(formula=f, family=gaussian(), data=data.frame(x,r),
                       pen.weights="glm.stand", lambda="cv1se.dev", 
                       control=list(lambda.length=20L, k=5, ncores=1));
      TRUE
      }, error=function(e) FALSE)
  })[[3]]
  if (try1 & try2) {
    coef(fit)
    l <- length(unique(coef(fit)))
    coef(fit2)
    l2 <- length(unique(coef(fit2)))
    out <- c(l,l2,t,t2)
  } else {
    out <- NULL
  }
  out
}, cl=6)

if (!is.matrix(ans)) {
  ans <- do.call(cbind, ans)
}

dat <- data.frame(type=rep(c("bin","gau"),each=ncol(ans)),
                  numUniq=as.vector(t(ans[1:2,])),
                  time=as.vector(t(ans[3:4,])),
                  n=n,
                  cnt=cnt)

write.table(dat, file=out, row.names=FALSE, col.names=FALSE, quote=FALSE, sep=",")
