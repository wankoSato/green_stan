
# chapter10 ---------------------------------------------------------------

# 第十章の目的
## 階層ベイズモデルの推定


# load library ------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstan)


# load data ---------------------------------------------------------------

getwd()
setwd(file.path(getwd(),'RTstudy','statmodel','wanko','kubobook_2012/'))
data <- read.csv('hbm/data7a.csv')
nested <- read.csv('hbm/nested/d1.csv')


# plot binomial data ------------------------------------------------------

# data7a.csvのデータ
## id : 個体id、個体数100
## y : 個体から8個の種子を採取した時の生存種子数
## yはn=8、生存確率qの二項分布であると仮定できる

# qの最尤推定量
q_hat <- mean(data$y)/8
q_hat

## qの最尤推定量は0.50375である
## このパラメータに基づく二項分布に実際のデータがマッチするか確認

# 図10.1(B)をggplotで描く

## 元データを描画用に変換
data_count <- data %>%
  group_by(y) %>%
  summarise(count = n())

## 二項分布データを作成
data_binom <- data.frame(count=seq(0,8),
                         prob = dbinom(x = seq(0,8), size = 8, prob = q_hat)*100)

## 描画
ggplot(data=data_count, aes(x=y, y=count)) +
  geom_point(size=5, shape=21, fill='black') +
  geom_line(data=data_binom, aes(x=count, y=prob)) +
  geom_point(data=data_binom, size=5, shape=21, fill="white", aes(x=count, y=prob))

## 分散を確認
var(data$y)  # 実データの分散 9.928384
8 * q_hat *(1-q_hat) # 理論値 1.999888

## 過分散になる


# ベイズする前にGLMMしてみる ---------------------------------------------------------

library(glmmML)
# 切片とランダム効果のみのモデル
model_glmm <- glmmML(cbind(y, 8-y) ~ 1, data=data, family = binomial, cluster = id)
summary(model_glmm)

# 結果: beta=0.04496, s=2.769
## rが標準誤差2.769でばらついた値をとる
## logit(q[i]) = beta + r[i]

## 理論値の出し方
### 1. -20~20の間で0.01刻みでrをふったときのrの確率pを算出
### 2. logit(q)=beta+rとしてqを算出し、qの確率分布を求める
### 3. 2で得られた確率分布にrの確率pを乗じて重みづけ
### 4. 重みづけした二項分布を足し合わせて混合分布とする

logistic <- function(x){
  1/(1+exp(-x))
}

beta <- 0.04496
x <- 2.769
r_seq <- seq(-20, 20, 0.01)
binomial_posterior <- lapply(r_seq, function(r){
  p_r <- dnorm(x=r, mean=0, sd=2.769) * 0.01
  p_binom <- logistic(beta+r)
  dist_binom <- dbinom(seq(0,8), size=8, prob=p_binom)* p_r
  names(dist_binom) <- as.character(seq(0,8))
  #out <- t(data.frame(p=dist_binom))
  #colnames(out) <- as.character(seq(0,8))
  return(dist_binom)
}) %>%
  bind_cols() %>%
  apply(MARGIN = 1, sum)

binomial_df <- data.frame(x=seq(0,8),
                          count = binomial_posterior*100)

# 総和が1になるか確認
sum(binomial_posterior)

## 描画
## それっぽい結果になった
ggplot(data=data_count, aes(x=y, y=count)) +
  geom_point(size=5, shape=21, fill='black') +
  geom_line(data=binomial_df, aes(x=x, y=count)) +
  geom_point(data=binomial_df, size=5, shape=21, fill="white", aes(x=x, y=count))


# GLMMのベイズモデル化 ------------------------------------------------------------

# stanコードは同ディレクトリのchapter_10_1.stan

# stanに入れるデータを抽出
n_sample <- nrow(data)
y <- data$y

# stanに渡すためにlist型にする
d_list <- list(n_sample=n_sample,
               y = y)

# stan実行
stan_model <- stan(file = '../chapter10_1.stan',
                   data = d_list,
                   chain = 4,                # chain デフォルト4
                   iter = 2000)              # サンプリング回数 デフォルト2000

# 結果の表示
options(max.print = 10000)
print(stan_model)

## betaの事後分布はほぼゼロ

# サンプリングの過程を図示
traceplot(stan_model, pars = c('beta', 's'))

# サンプリング結果を抽出し分布を図示
# 本文p231とほぼ同じ分布が得られる

## extractでサンプリング結果を抽出
## extractでサンプリング結果が得られるが、このとき抽出した結果はwarmup期間を除いたものである
stan_sampling_res <- extract(stan_model)

## beta
g_beta <- ggplot(data.frame(x=stan_sampling_res$beta), aes(x=x, y=..density..)) +
  geom_histogram(binwidth = 0.05) +
  geom_density(fill="blue", alpha=0.2, bw=0.05) +
  xlab('beta')
## s
g_s <- ggplot(data.frame(x=stan_sampling_res$s), aes(x=x, y=..density..)) +
  geom_histogram(binwidth = 0.05) +
  geom_density(fill="blue", alpha=0.2, bw=0.05) +
  xlab('s')

gridExtra::grid.arrange(g_beta,g_s)

# rのプロット
g_r_list <- lapply(seq(3), function(i){
  g <- ggplot(data.frame(x=stan_sampling_res$r[i,]), aes(x=x, y=..density..)) +
    geom_histogram(binwidth = 0.5) +
    geom_density(fill="blue", alpha=0.2, bw=2) +
    xlab(cat('r[',i,']',sep='')) +
    xlim(-10, 10)
})

gridExtra::grid.arrange(g_r_list[[1]],g_r_list[[2]],g_r_list[[3]])

# 各個体ごとのyの予測値の中央値をとる
## やっていること：

### 各iterationにおけるyの結果を各yごとにカウント
### カウント結果を結合して一つのデータフレームにする
### 各yのカウントの中央値、2.5%qautile, 97.5%quntileを出力
y_hat_df <- lapply(seq(1000), function(x){
  count_tmp <- data.frame(y = stan_sampling_res$yhat[x,]) %>%
    group_by(y) %>%
    summarize(count = n()) %>%
    select(count) %>%
    t()
  count_tmp <- as.data.frame(count_tmp)
  #colnames(count_tmp) <- as.character(seq(0,8))
}) %>%
  bind_rows()

y_hat_data <- data.frame(x = seq(0,8),
                         median = apply(y_hat_df, MARGIN = 2, median, na.rm=T),
                         upper = apply(y_hat_df, MARGIN = 2, quantile, probs=c(0.975), na.rm=T),
                         lower = apply(y_hat_df, MARGIN = 2, quantile, probs=c(0.025), na.rm=T))

# plot
## 本文の図10.4とほぼ同じ結果が得られた
ggplot() +
  geom_ribbon(data=y_hat_data, aes(x=x, ymin=lower, ymax=upper), fill='#000000', alpha=0.3) +
  geom_point(data=data_count, size=5, shape=21, fill='black', aes(x=y, y=count)) +
  geom_line(data=y_hat_data, aes(x=x, y=median)) +
  geom_point(data=y_hat_data, size=5, shape=21, fill="white", aes(x=x, y=median))


# 個体差＋場所差の階層ベイズモデル化 -------------------------------------------------------

# ひとまずプロット
ggplot(data=nested, aes(x=pot, y=y)) +
  geom_boxplot(aes(color=f)) +
  geom_segment(x=1,xend=5, y=6.64, yend=6.64, lty='dashed', color='deeppink', size=1) + 
  geom_segment(x=6,xend=10, y=4.4, yend=4.4, lty='dashed', color='skyblue', size=1)

# 平均と分散を確認
mean(nested[1:50,]$y)    # 6.64
mean(nested[51:100,]$y)  # 4.4
var(nested[1:50,]$y)     # 52.2351
var(nested[51:100,]$y)   # 55.10204

## 種子数がポアソン分布に従うとすると明らかに過分散である
## また鉢事のばらつきも大きい
### 本文のデータは以下の条件で生成されている
### 切片1, 施肥効果の係数0
### 個体差の標準偏差1, 鉢差の標準偏差1
### 切片の平均がやや大きめに見積もられているようにも見えるが、stanで比較的良く推定されている

# stanに渡すデータの作成
d_list <- list(n_sample = nrow(nested),
               n_pot = max(as.numeric(nested$pot)),
               pot = as.numeric(nested$pot),
               f = as.numeric(nested$f)-1,
               y = nested$y)

# stan実行
stan_model <- stan(file = '../chapter10_2.stan',
                   data = d_list,
                   chain = 4,                # chain デフォルト4
                   iter = 2000)              # サンプリング回数 デフォルト2000

# 結果の表示
options(max.print = 10000)
print(stan_model)
## beta_2の95%信用区間が-2.3~0.7と、施肥効果がほぼない結果となった
## この結果は本文とも一致する

traceplot(stan_model, pars = c('beta_1', 'beta_2', 's', 'sp'))

## extractでサンプリング結果を抽出
stan_sampling_res <- extract(stan_model)

## beta
g_beta_1 <- ggplot(data.frame(x=stan_sampling_res$beta_1), aes(x=x, y=..density..)) +
  geom_histogram(binwidth = 0.05) +
  geom_density(fill="blue", alpha=0.2, bw=0.05) +
  xlab('beta_1')
## beta_2
g_beta_2 <- ggplot(data.frame(x=stan_sampling_res$beta_2), aes(x=x, y=..density..)) +
  geom_histogram(binwidth = 0.05) +
  geom_density(fill="blue", alpha=0.2, bw=0.05) +
  xlab('beta_2')
## s
g_s <- ggplot(data.frame(x=stan_sampling_res$s), aes(x=x, y=..density..)) +
  geom_histogram(binwidth = 0.05) +
  geom_density(fill="blue", alpha=0.2, bw=0.05) +
  xlab('s')
## s
g_sp <- ggplot(data.frame(x=stan_sampling_res$sp), aes(x=x, y=..density..)) +
  geom_histogram(binwidth = 0.05) +
  geom_density(fill="blue", alpha=0.2, bw=0.05) +
  xlab('sp')

gridExtra::grid.arrange(g_beta_1,g_beta_2,g_s,g_sp)

# 各個体のyhatの中央値を求める
y_hat_vec <- apply(stan_sampling_res$yhat, MARGIN = 2, median)
y_hat_df <- data.frame(nested, yhat=y_hat_vec)

# 推定値を図示
ggplot(data=y_hat_df, aes(x=pot, y=y)) +
  geom_boxplot(aes(color=f)) +
  geom_segment(x=1,xend=5, y=6.64, yend=6.64, lty='dashed', color='deeppink', size=1) + 
  geom_segment(x=6,xend=10, y=4.4, yend=4.4, lty='dashed', color='skyblue', size=1)

# 各鉢ごとのyhatの分布を求める
y_hat_all <- t(stan_sampling_res$yhat) %>%
  data.frame(.,nested$pot)

y_hat_list <- lapply(levels(nested$pot),function(x){
  vec <- y_hat_all[y_hat_all$nested.pot==x,1:4000] %>% unlist()
  return(data.frame(x=vec))
  })

g_list <- lapply(seq(10), function(i){
  if(i<6){
    fill_color <- 'deeppink'
  }else{
    fill_color <- 'skyblue'
  }
  ggplot(y_hat_list[[i]], aes(x=x, y=..density..)) +
    geom_histogram(binwidth = 1, fill=fill_color) +
    xlim(0,70) +
    xlab(paste('pot',i)) +
    coord_flip()
})

gridExtra::grid.arrange(grobs=g_list, ncol=10)
