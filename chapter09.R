
# chapter9 ----------------------------------------------------------------

# 第九章の目的
## WinBUSGによるサンプリングの実践
## ただし個人的な事情によりRStanを使うことにした


# load library ------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstan)


# load data ---------------------------------------------------------------

getwd()
setwd(file.path(getwd(),'RTstudy','statmodel','wanko','kubobook_2012/'))
load('./gibbs/d.RData')
length(d)

# x:体サイズ
# y:各個体の種子数
# 種子数yはポアソン分布するものと仮定し、パラメータλは体サイズxによって決定される
# そのときのリンク関数はlogであり、log(λ)=β1 + β2xの関係にあるとする
# データ生成したときの真のパラメータはβ1=1.5, β2=0.1であるとする


# GLMしてみる -----------------------------------------------------------------

glm_model <- glm(y ~ x, data = d, family=poisson)
summary(glm_model)

pred_glm <- predict(glm_model, newdata = d, type = 'response')

ggplot(d, aes(x=x, y=y)) + 
  geom_point(size=5, shape=21, fill='white') +
  geom_line(data=data.frame(pred = pred_glm), lty='dashed', size=2, aes(x=d$x, y=pred))

# 本文とほぼ同じグラフが描ける


# Stanしてみる ----------------------------------------------------------------

# stanに入れるデータを抽出
n_sample <- nrow(d)
x <- d$x
y <- d$y

# stanに渡すためにlist型にする
d_list <- list(n_sample=n_sample,
               x = x,
               y = y)

# stan実行
stan_model <- stan(file = '../chapter09.stan',
                   data = d_list)
stan_model

## 結果
### beta_1=1.56, beta_2=0.08とほぼ真のパラメータに近い値が推定できている
### Rhatを見ると値がほぼ1であり、サンプリング間の相関がなく良い推定になっている

# サンプリングの過程を図示
traceplot(stan_model, pars = c('beta_1', 'beta_2'))

# サンプリング結果を抽出し分布を図示
## beta_1
stan_sampling_res <- extract(stan_model)
g_beta1 <- ggplot(data.frame(x=stan_sampling_res$beta_1), aes(x=x, y=..density..)) +
  geom_histogram(binwidth = 0.01) +
  geom_density(fill="blue", alpha=0.2, bw=0.02) +
  xlab('beta_1')
## beta_2
stan_sampling_res <- extract(stan_model)
g_beta2 <- ggplot(data.frame(x=stan_sampling_res$beta_2), aes(x=x, y=..density..)) +
  geom_histogram(binwidth = 0.001) +
  geom_density(fill="blue", alpha=0.2, bw=0.005) +
  xlab('beta_2')

gridExtra::grid.arrange(g_beta1,g_beta2)

## beta_1とbeta_2の同時分布
ggplot(data.frame(beta_1=stan_sampling_res$beta_1,
                  beta_2=stan_sampling_res$beta_2), aes(x=beta_1, y=beta_2)) +
  geom_point(size=3, alpha=0.1)

# 95%信用区間
## beta_1
quantile(stan_sampling_res$beta_1,probs = c(0.025,0.5,0.975))
## beta_2
quantile(stan_sampling_res$beta_2,probs = c(0.025,0.5,0.975))

# 体サイズに対するλの推定値を図示
sampling_df <- lapply(seq(20), function(i){
  data.frame(x=rep(d$x[i]), lambda=stan_sampling_res$lambda[,i], y=stan_sampling_res$yhat[,i])
}) %>% bind_rows()

interval_df <- lapply(seq(20), function(i){
  data.frame(x=d$x[i],
             lower=quantile(stan_sampling_res$yhat[,i], probs = 0.025),
             upper=quantile(stan_sampling_res$yhat[,i], probs = 0.975),
             median=quantile(stan_sampling_res$yhat[,i], probs = 0.5))
}) %>% bind_rows()

ggplot(sampling_df, aes(x=x, y=exp(lambda))) +
  geom_point(size=3, alpha=0.1) +
  geom_point(d=d, size=5, shape=21, fill='white', aes(x=x, y=y))

# 体サイズに対するyの95%予測区間を図示
ggplot(d, aes(x=x, y=y)) +
  geom_point(size=5, shape=21, fill='white') +
  geom_ribbon(data=interval_df, aes(x=x, ymin=lower, ymax=upper), fill='#000000', alpha=0.3) +
  geom_line(data=interval_df, aes(x=x, y=median))



# 事前分布と事後分布 ---------------------------------------------------------------

# p214のβ1の無情報事前分布を入力としたときの事後分布を計算してみる

# 平均0、標準偏差100000の正規分布を無情報事前分布とする
mu <- 0
sigma <- 100000
beta1_seq <- seq(-10,10,0.01)
posterior_df <- data.frame(beta1_seq=beta1_seq)

# step1:β2=0.0として事後分布を計算
posterior_dist <- sapply(beta1_seq, function(x){
  lambda <- exp(x)
  p <- exp(sum(dpois(x = d$y, lambda = lambda, log = TRUE))) * (dnorm(x,mean=mu, sd=sigma)*0.01)
})

ggplot() +
  geom_line(aes(x=beta1_seq, y=posterior_dist/sum(posterior_dist))) +
  geom_line(aes(x=beta1_seq, y=dnorm(beta1_seq, mean=mu, sd=sigma))) +
  xlim(1.6,2.4)

## 事後分布の平均と分散
posterior_dist <- posterior_dist/sum(posterior_dist)*100
posterior_df$step1 <- posterior_dist
mu_post <- sum(beta1_seq*posterior_dist*0.01)
sigma_post <- sum(((beta1_seq-mu)^2)*posterior_dist*0.01)

mu_post
sigma_post

# sep2:β2が-0.016とサンプリングされたときの事後分布の変化
posterior_dist <- sapply(beta1_seq, function(x){
  lambda <- exp(x + d$x*(-0.016))
  p <- exp(sum(dpois(x = d$y, lambda = lambda, log = TRUE))) * (dnorm(x,mean=mu, sd=sigma)*0.01) #事前分布には無情報正規分布をいつも使う
})

posterior_dist <- posterior_dist/sum(posterior_dist)*100
posterior_df$step2 <- posterior_dist
mu_post <- sum(beta1_seq*posterior_dist*0.01)
sigma_post <- sum(((beta1_seq-mu)^2)*posterior_dist*0.01)

# step3:β2=0.015とサンプリングされたとき
posterior_dist <- sapply(beta1_seq, function(x){
  lambda <- exp(x + d$x*(0.015))
  p <- exp(sum(dpois(x = d$y, lambda = lambda, log = TRUE))) * (dnorm(x,mean=mu, sd=sigma)*0.01) #事前分布には無情報正規分布をいつも使う
})

posterior_dist <- posterior_dist/sum(posterior_dist)*100
posterior_df$step3 <- posterior_dist

# plot
ggplot(data=posterior_df) +
  geom_line(size=2,aes(x=beta1_seq, y=step1/100, color="step1")) +
  geom_line(size=2,aes(x=beta1_seq, y=step2/100, color="step2")) +
  geom_line(size=2,aes(x=beta1_seq, y=step3/100, color="step3")) +
  xlim(1.6,2.4)
