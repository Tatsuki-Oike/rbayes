# 1 ライブラリ
library(ggplot2)
set.seed(1)

# 2 真の分布(ベルヌーイ分布)
x <- 0:1
p <- 0.8
prob <- dbinom(x, 1, p)

ggplot() + 
  geom_bar(aes(x, prob), stat ="identity") +
  labs(x="x", y="prob", title="ベルヌーイ分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 20))

# 3 事前分布(ベータ分布)
a_pre <- 2
b_pre <- 2
x <- seq(0,1, 0.01)
p_prob_pre <- dbeta(x, a_pre, b_pre)

plot <- ggplot()+
  geom_line(aes(x=x, y=p_prob_pre))+
  labs(x="p", y="prob", title="ベータ分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 20))
plot

# 4 データ
data <- rbinom(30, 1, p)
N <- length(data)

# 5 事後分布(ベータ分布)
a_pos <- sum(data) + a_pre
b_pos <- N - sum(data) + b_pre
x <- seq(0,1, 0.01)
p_prob_pos <- dbeta(x, a_pos, b_pos)

plot + geom_line(aes(x=x, y=p_prob_pos), col="blue")
