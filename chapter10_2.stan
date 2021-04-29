// 入力データ
data {
  int<lower=0> n_sample;      //サンプル数
  int<lower=0> n_pot;         //鉢数
  int<lower=0> pot[n_sample]; //鉢番号 ダミー変数1~10
  int<lower=0> f[n_sample];   //施肥効果 ダミー変数0 or 1
  int<lower=0> y[n_sample];   //生存種子数
}

// 推定したいパラメータ
// 今回は個体差rも含む
parameters {
  real beta_1;        //切片：正規分布に従う
  real beta_2;        //施肥効果の係数
  real r[n_sample];   //個体差：分散sの正規分布に従う
  real<lower=0> s;    //個体差の分散：0-1000の一様分布に従う
  real rp[n_pot];     //鉢差
  real<lower=0> sp;   //鉢差の分散
}


// 各個体のqを線形予測子から作る
transformed parameters {
  real lambda[n_sample];
  for (i in 1:n_sample){
    lambda[i] = beta_1 + beta_2 * f[i] + r[i] + rp[pot[i]];
  }
}

// モデルの設定
model {
  //各パラメータの確率分布
  beta_1 ~ normal(0, 100000);  //beta_1の無情報事前分布
  beta_2 ~ normal(0, 100000);  //beta_2の無情報事前分布
  s ~ uniform(0, 1000);        //sの無情報事前分布(個体差)
  sp ~ uniform(0, 1000);       //spの無情報事前分布(鉢差)
  r ~ normal(0, s);            //個体差rは左記の分布から発生する
  rp ~ normal(0, sp);          //鉢差rpは左記の分布から発生する
  
  //yの推定
  y ~ poisson_log(lambda);

}

//予測区間も計算する
generated quantities {
  real yhat[n_sample];
  for (i in 1:n_sample){
    yhat[i] = poisson_log_rng(lambda[i]);
  }
}
