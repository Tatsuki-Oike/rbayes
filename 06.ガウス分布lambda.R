# 1 ライブラリ
library(ggplot2)
set.seed(1)

# 2 真の分布(ガウス分布)
mu <- 170
lambda <- 1/100
x <- seq(100 ,200, 1)
prob <- dnorm(x, mu, 1/sqrt(lambda))

ggplot()+
  geom_line(aes(x=x, y=prob))+
  labs(x="x", y="prob", title="ガウス分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 24))

# 3 事前分布(ガンマ分布)
a_pre <- 2
b_pre <- 1
x <- seq(0, 0.1, 0.001)
lambda_prob_pre <- dgamma(x, a_pre, b_pre)

plot <- ggplot()+
  geom_line(aes(x=x, y=lambda_prob_pre))+
  labs(x="λ", y="prob", title="ガンマ分布")+
  theme_classic(base_family = "HiraKakuPro-W3")+
  theme(text = element_text(size = 24))
plot

# 4 データ
data <- rnorm(30, mu, 1/sqrt(lambda))
N <- length(data)

# 5 事後分布(ガンマ分布)
a_pos <- N/2 + a_pre
b_pos <- sum((data-mu)^2)/2 + b_pre
x <- seq(0, 0.1, 0.001)
lambda_prob_pos <- dgamma(x, a_pos, b_pos)

plot + geom_line(aes(x=x, y=lambda_prob_pos), col="blue")
