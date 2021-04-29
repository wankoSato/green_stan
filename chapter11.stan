// 参考
// https://gaiasky.hatenablog.com/entry/2018/08/15/205222
// https://qiita.com/tackey/items/96171be393912fb333b3
// https://github.com/takitsuba/midoribon/blob/master/Chap11/Chap11.ipynb

// 入力データ
data {
  int<lower=0> n_sample;      //サンプル数
  int<lower=0> y[n_sample];   //観測個体数
  //int<lower=0> near[n_sample];//近傍数, i=1,50のときは1、それ以外は2とする
}

// 推定したいパラメータ
// 今回は個体差rも含む
// log(λ_i) = β + r_i
parameters {
  real beta;        //切片：正規分布に従う
  real mu_zero;
  real r[n_sample];   //場所差：分散sの正規分布に従う
  real<lower=0> s;    //場所差の分散：0-1000の一様分布に従う
}


// 各個体のqを線形予測子から作る
transformed parameters {
  real lambda[n_sample];
  real mu[n_sample];
  for (i in 1:n_sample){
    if (i==1){
      mu[i] = r[1];
      //mu[i] = 0;
    }else if (i==50){
      mu[i] = r[i-1];
    }else{
      //mu[i] = (r[i-1]+r[i+1])/2;
      mu[i] = r[i-1];
    }
    lambda[i] = beta + r[i];
  }
}

// モデルの設定
model {
  //各パラメータの確率分布
  beta ~ normal(0, 1000);    //beta_1の無情報事前分布
  s ~ uniform(0, 10);        //sの無情報事前分布(個体差)
  //for(i in 1:n_sample){        //個体差rは左記の分布から発生する
  //  if(i==1){
  //    r[i] ~ normal(r[i+1], s);
  //  }else if(i==50){
  //    r[i] ~ normal(r[i-1], s);
  //  }else{
  //    r[i] ~ normal((r[i-1]+r[i+1])/2, s);
   // }
  //}
  mu_zero ~ normal(0,10);
  r[1] ~ normal(mu_zero, s);
  for(i in 2:n_sample){
    r[i] ~ normal(mu[i], s);
  }
  //for (i in 2:n_sample){
  //  r[i] ~ normal(mu[i], s);
  //}
              
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
