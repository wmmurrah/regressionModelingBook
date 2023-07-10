
library(ggplot2)

P <- c(50, 100, 200, 300)
p <- seq(1, 20)
W <- c(.2, .5, 1, 2)
w <- 1:20


# t <- function(P, p, W, w) {
#   t <- p*P + w*W*P
#   return(t/60)
# }

frame <- expand.grid(P = P, p=p, W=W, w=w)

frame$t <- NA

for(i in 1: nrow(frame)) {
  frame[i, "t"] <- frame[i, "p"]*frame[i, "P"] + frame[i, "w"]*frame[i,"W"]*frame[i,"P"]
}


frame$t <- p*P + w*W*P
frame$tp <- (p/2)*P + w*W*P
frame$tw <- p*P + (w/2)*W*P

frame$P <- factor(frame$P, labels= paste0("Pages =", P))
frame$p <- factor(frame$p)
frame$W <- factor(frame$W, labels = paste0("No. Tasks=", W))
frame$w <- factor(frame$w)


ggplot(frame, aes(y = t, x = w, color = p, group = p)) + geom_line() + 
  facet_wrap(P ~ W)

mod <- lm(t ~ p*w, data = frame)
anova(mod)
mod2 <- update(mod, . ~ . - p:w )
anova(mod, mod2)
anova(mod2)

summary(mod2)


aggregate(t ~ w + p, frame , range)

aggregate(t ~ p, aggwp, mean)

mod2

plot(p ~ w, frame)


fullmod <- lm(t ~ P + p + W + w, frame)
summary(fullmod)

predict(fullmod, newdata = data.frame(P = 150, p = 3, w = 10, W =2))

p_gt_w <- ifelse(frame$p*frame$P > frame$w*frame$W*frame$P, 1, 0 )
p_gt_wW <- ifelse(frame$p > frame$w*frame$W, 1, 0)

mean(p_gt_w)
mean(p_gt_wW)
