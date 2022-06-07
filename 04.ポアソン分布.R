# 1 ライブラリ
library(ggplot2)
set.seed(1)

# 2 真の分布(ポアソン分布)
x <- 0:10
lambda <- 3
prob <- dpois(x, lambda)

ggplot() + 
  geom_bar(aes(x, prob), stat ="identity") +
  labs(x="x", y="prob", title="ポアソン分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 24))

# 3 事前分布(ガンマ分布)
a_pre <- 2
b_pre <- 1
x <- seq(0, 6, 0.01)
lambda_prob_pre <- dgamma(x, a_pre, b_pre)

plot <- ggplot()+
  geom_line(aes(x=x, y=lambda_prob_pre))+
  labs(x="λ", y="prob", title="ガンマ分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 24))
plot

# 4 データ
data <- rpois(30, lambda)
N <- length(data)

# 5 事後分布(ガンマ分布)
a_pos <- sum(data) + a_pre
b_pos <- N + b_pre
x <- seq(0, 6, 0.01)
lambda_prob_pos <- dgamma(x, a_pos, b_pos)

plot + 
  geom_line(aes(x=x, y=lambda_prob_pos), col="blue")
