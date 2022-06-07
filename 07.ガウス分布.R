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

# 3 事前分布

## 3.1 事前分布(ガンマ分布)
a_pre <- 2
b_pre <- 1
x <- seq(0 ,0.1, 0.001)
lambda_prob_pre <- dgamma(x, a_pre, b_pre)
lambda_sample_pre <- rgamma(1, a_pre, b_pre)

plot_lambda <- ggplot()+
  geom_line(aes(x=x, y=lambda_prob_pre))+
  labs(x="λ", y="prob", title="ガンマ分布")+
  theme_classic(base_family = "HiraKakuPro-W3")+
  theme(text = element_text(size = 24))
plot_lambda

## 3.2 事前分布(正規分布)
mu_pre <- 150
beta_pre <- 0.0001
x <- seq(100 ,200, 1)
mu_prob_pre <- dnorm(x, mu_pre, 1/sqrt(beta_pre*lambda_sample_pre))

plot_mu <- ggplot()+
  geom_line(aes(x=x, y=mu_prob_pre))+
  labs(x="μ", y="prob", title="正規分布(事前)")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 24))
plot_mu

# 4 データ
data <- rnorm(30, mu, 1/sqrt(lambda))
N <- length(data)

# 5 事後分布

## 5.1 パラメータ更新
beta_pos <- N + beta_pre
mu_pos <- (sum(data)+beta_pre*mu_pre)/beta_pos
a_pos <- N/2 + a_pre
b_pos <- (sum(data^2)+beta_pre*mu_pre^2-beta_pos*mu_pos^2)/2 + b_pre
lambda_sample_pos <- rgamma(1, a_pos, b_pos)

## 5.2 事後分布(ガンマ分布)
x <- seq(0 ,0.1, 0.001)
lambda_prob_pos <- dgamma(x, a_pos, b_pos)

plot_lambda + geom_line(aes(x=x, y=lambda_prob_pos), col="blue")

## 5.3 事後分布(正規分布)
x <- seq(100 ,200, 1)
mu_prob_pos <- dnorm(x, mu_pos, 1/sqrt(beta_pos*lambda_sample_pos))

plot_mu + geom_line(aes(x=x, y=mu_prob_pos), col="blue")
