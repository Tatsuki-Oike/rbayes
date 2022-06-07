# 1 ライブラリ
library(ggplot2)
library(MCMCpack)
set.seed(1)

# 2 真の分布(多次元ガウス分布)
mu <- c(170, 60)
lambda <- solve(matrix(c(100, 80, 80, 100), ncol = 2))
samples <- mvrnorm(100, mu = mu, Sigma = solve(lambda))

ggplot()+
  geom_point(aes(x=samples[,1], y=samples[,2]))+
  labs(x=expression(x[1]), y=expression(x[2]),
       title="多次元ガウス分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 24))+
  lims(x=c(140,200), y=c(30,90))

# 3 事前分布(多次元ガウス分布)
mu_pre <- c(150, 50)
lambda_pre <- solve(matrix(c(1000, 100, 100, 1000), ncol = 2))
mu_samples_pre <- 
  mvrnorm(100, mu = mu_pre, Sigma = solve(lambda_pre))

plot <- ggplot()+
  geom_point(aes(x=mu_samples_pre[,1], y=mu_samples_pre[,2]))+
  labs(x=expression(mu[1]), y=expression(mu[2]),
       title="多次元ガウス分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 24))+
  lims(x=c(40,250), y=c(-50,160))
plot

# 4 データ
data <- mvrnorm(30, mu = mu, Sigma = solve(lambda))
N <- nrow(data)

# 5 事後分布(多次元ガウス分布)
lambda_pos <- N*lambda + lambda_pre
mu_pos <- solve(lambda_pos) %*%
  (lambda %*% apply(data, 2, sum) + lambda_pre %*% mu_pre)
mu_samples_pos <- 
  mvrnorm(100, mu = mu_pos, Sigma = solve(lambda_pos))

plot +
  geom_point(aes(x=mu_samples_pos[,1], y=mu_samples_pos[,2]),
             col="blue")
