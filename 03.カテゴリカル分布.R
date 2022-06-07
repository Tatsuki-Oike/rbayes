# 1 ライブラリ
library(ggplot2)
library(MCMCpack) # rdirichlet
set.seed(1)

# 2 真の分布(カテゴリカル分布)
x <- c(1, 2, 3)
p <- c(0.1, 0.6, 0.3)
prob <- p

ggplot() + 
  geom_bar(aes(x, prob), stat ="identity") +
  labs(x="x", y="prob", title="カテゴリカル分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 20))

# 3 事前分布(ディリクレ分布)
alpha_pre <- c(1, 1, 1)
p_samples_pre <- rdirichlet(100, alpha_pre)

plot <- ggplot()+
  geom_point(aes(x=p_samples_pre[,1], y=p_samples_pre[,2]))+
  labs(x="1", y="2", title="ディリクレ分布")+
  theme_classic(base_family = "HiraKakuPro-W3") +
  theme(text = element_text(size = 20))+
  lims(x=0:1, y=0:1)
plot

# 4 データ
data <- which(rmultinom(300, 1, c(0.1, 0.6, 0.3))==1, arr.ind = TRUE)[,1]
N <- length(data)

# 5 事後分布(ディリクレ分布)
alpha_pos <- as.vector(table(data)) + alpha_pre
p_samples_pos <- rdirichlet(100, alpha_pos)

plot + 
  geom_point(aes(x=p_samples_pos[,1], y=p_samples_pos[,2]), col="blue")
