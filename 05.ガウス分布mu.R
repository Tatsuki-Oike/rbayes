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

# 3 事前分布(ガウス分布)
mu_pre <- 150
lambda_pre <- 1/10000
x <- seq(100 ,200, 1)
mu_prob_pre <- dnorm(x, mu_pre, 1/sqrt(lambda_pre))

plot <- ggplot()+
  geom_line(aes(x=x, y=mu_prob_pre))+
  labs(x="μ", y="prob", title="ガウス分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 24))
plot

# 4 データ
data <- rnorm(30, mu, 1/sqrt(lambda))
N <- length(data)

# 5 事後分布(ガウス分布)
lambda_pos <- N*lambda + lambda_pre
mu_pos <- (lambda*sum(data) + lambda_pre*mu_pre)/lambda_pos
x <- seq(100 ,200, 1)
mu_prob_pos <- dnorm(x, mu_pos, 1/sqrt(lambda_pos))

plot + geom_line(aes(x=x, y=mu_prob_pos), col="blue")
