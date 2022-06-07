# 1 ライブラリ
library(ggplot2)
library(ellipse)
library(MCMCpack)
set.seed(1)

# 2 真の分布(多次元ガウス分布)
mu <- c(170, 60)
lambda <- solve(matrix(c(100, 80, 80, 100), ncol = 2))
samples <- mvrnorm(100, mu = mu, Sigma = solve(lambda))

ell <- ellipse(centre=mu, solve(lambda))
ggplot()+
  geom_point(aes(x=samples[,1], y=samples[,2]))+
  labs(x=expression(x[1]), y=expression(x[2]),
       title="多次元ガウス分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 24))+
  lims(x=c(140,200), y=c(30,90))+
  geom_polygon(data=as.data.frame(ell), aes(x,y),
               fill=NA,colour="black")

# 3 事前分布

## 3.1 事前分布(ウィシャート分布)
n_pre <- 3
w_pre <- diag(2)*0.01
lambda_sample_pre <- rwish(n_pre, w_pre)

plot_lambda <- ggplot() +
  theme_classic(base_family = "HiraKakuPro-W3")+
  theme(text = element_text(size = 20),
        legend.position=c(0.3,0.7)) +
  labs(x=expression(x[1]), y=expression(x[2]),
       title="ウィシャート分布") 
for(i in 1:100){
  lambda_samples_pre <- rwish(n_pre, w_pre)
  ell <-  ellipse(centre=mu, solve(lambda_samples_pre))
  plot_lambda <- plot_lambda + 
    geom_polygon(data=as.data.frame(ell),aes(x,y),
                 fill=NA,colour="black")
}
plot_lambda

## 3.2 事前分布(多次元ガウス分布)
mu_pre <- c(150, 50)
beta_pre <- 0.01
mu_samples_pre <- 
  mvrnorm(100, mu = mu_pre,
          Sigma = solve(beta_pre*lambda_sample_pre))

plot_mu <- ggplot()+
  geom_point(aes(x=mu_samples_pre[,1], y=mu_samples_pre[,2]))+
  labs(x=expression(mu[1]), y=expression(mu[2]),
       title="多次元ガウス分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 24))
plot_mu

# 4 データ
data <- mvrnorm(100, mu = mu, Sigma = solve(lambda))
N <- nrow(data)

# 5 事後分布

## 5.1 パラメータ更新
beta_pos <- N + beta_pre
mu_pos <- (apply(data,2,sum)+beta_pre*mu_pre)/beta_pos
n_pos <- N + n_pre
S <- 0
for(i in 1:N){
  S <- S + data[i,] %*% t(data[i,])
}
w_pos <- 
  solve(S + beta_pre*mu_pre%*%t(mu_pre) - 
          beta_pos*mu_pos%*%t(mu_pos) + solve(w_pre))
lambda_sample_pos <- rwish(n_pos, w_pos)

## 5.2 事後分布(ウィシャート分布)
for(i in 1:100){
  lambda_samples_pos <- rwish(n_pos, w_pos)
  ell <-  ellipse(centre=mu, solve(lambda_samples_pos))
  plot_lambda <- plot_lambda + 
    geom_polygon(data=as.data.frame(ell),aes(x,y),
                 fill=NA, colour="blue")
}
plot_lambda

## 5.3 事後分布(多次元ガウス分布)
mu_samples_pos <- 
  mvrnorm(100, mu = mu_pos,
          Sigma = solve(beta_pos*lambda_sample_pos))

plot_mu + 
  geom_point(aes(x=mu_samples_pos[,1],y=mu_samples_pos[,2]),
             col="blue")
