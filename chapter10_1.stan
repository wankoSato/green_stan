// 参考
// https://tjo.hatenablog.com/entry/2014/05/01/190346

// 入力データ
data {
  int<lower=0> n_sample;      //サンプル数
  int<lower=0> y[n_sample];   //生存種子数
}

// 推定したいパラメータ
// 今回は個体差rも含む
parameters {
  real beta;          //切片：正規分布に従う
  real r[n_sample];   //個体差：分散sの正規分布に従う
  real<lower=0> s;    //個体差の分散：0-1000の一様分布に従う
}


// 各個体のqを線形予測子から作る
transformed parameters {
  real q[n_sample];
  for (i in 1:n_sample){
    q[i] = inv_logit(beta + r[i]);
  }
}

// モデルの設定
model {
  //各パラメータの確率分布
  beta ~ normal(0, 100000);  //betaの無情報事前分布
  s ~ uniform(0, 1000); //sの無情報事前分布
  // ベクトル化で以下のように書いても良い
  // ハヤブサ本p313
  r ~ normal(0, s);
  //for (i in 1:n_sample){
  //  r[i] ~ normal(0, s);  //各個体ごとのr
  //}
  
  //yの推定
  y ~ binomial(8, q);
  //for (i in 1:n_sample){
  //  y[i] ~ binomial(8, q[i]);
  //}
}

//予測区間も計算する
generated quantities {
  int yhat[n_sample];
  for (i in 1:n_sample){
    yhat[i] = binomial_rng(8, q[i]);
  }
}
