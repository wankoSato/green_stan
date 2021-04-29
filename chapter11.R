
# chapter11 ---------------------------------------------------------------

# 第十一章の目的
## 空間構造のあるデータで階層ベイズする


# load library ------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstan)


# load data ---------------------------------------------------------------

getwd()
setwd(file.path(getwd(),'RTstudy','statmodel','wanko','kubobook_2012/'))
load('spatial/Y.RData')

Y
length(Y)


# データのプロット ----------------------------------------------------------------

ggplot() +
  geom_point(size=5, shape=21, fill='white', aes(x=seq(50), y=Y))

# 元データだけだと図11.2の破線は描けない
# 平滑化スプラインか何かをやっている？

# mgcvライブラリを使って平滑化スプラインを描く
library(mgcv)
data_gam <- data.frame(x=seq(50),y=Y)
gam_model <- gam(y ~ s(x), data=data_gam)
summary(gam_model)
pred_gam <- predict(gam_model, newdata = data_gam, type = 'response')

ggplot() +
  geom_point(size=5, shape=21, fill='white', aes(x=seq(50), y=Y)) +
  geom_line(lty='dashed', size=1, aes(x=seq(50), y=pred_gam))

# 一致はしていないがそれっぽくなったので満足しておく


# 空間構造のあるデータでベイズする --------------------------------------------------------

# stanに入れるデータを抽出
n_sample <- length(Y)
y <- Y

# stanに渡すためにlist型にする
d_list <- list(n_sample=n_sample,
               y = y)

# stan実行
# 状態空間モデルをStanで推定するときの収束を良くするコツ
# https://logics-of-blue.com/tips-for-better-convergence-when-making-state-space-model-with-stan/
# https://ito-hi.blog.ss-blog.jp/2015-02-15
# https://github.com/takitsuba/midoribon/blob/master/Chap11/Chap11.ipynb
stan_model <- stan(file = '../chapter11.stan',
                   data = d_list,
                   chain = 4,                # chain デフォルト4
                   iter = 20000,              # サンプリング回数 デフォルト2000
                   warmup = 10000,
                   thin = 1,
                   control = list(max_treedepth = 20,
                                  adapt_delta = 0.99))

# 結果の表示
options(max.print = 10000)
print(stan_model)

traceplot(stan_model, pars = c('beta', 's'))

stan_sampling_res <- extract(stan_model)
hist(stan_sampling_res$s)
hist(stan_sampling_res$beta)

yhat_mean <- apply(stan_sampling_res$yhat, MARGIN = 2, mean)
yhat_upper <- apply(stan_sampling_res$yhat, MARGIN = 2, quantile, 0.9)
yhat_lower <- apply(stan_sampling_res$yhat, MARGIN = 2, quantile, 0.1)

ggplot() +
  geom_point(size=5, shape=21, fill='white', aes(x=seq(50), y=Y)) +
  geom_line(lty='dashed', size=1, aes(x=seq(50), y=pred_gam)) +
  geom_line(size=1, aes(x=seq(50), y=yhat_mean)) +
  geom_ribbon(aes(x=seq(50), ymin=yhat_lower, ymax=yhat_upper), alpha=0.2)
