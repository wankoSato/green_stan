//ポアソン回帰におけるstanコードの書き方参考
//https://stats.biopapyrus.jp/bayesian-statistics/estimation/poisson-regression.html
//https://kosugitti.github.io/JSSP2018Summer/jssp04.html

//入力データの設定
data {
  int<lower=0> n_sample;     //サンプル数
  int<lower=0> y[n_sample];  //種子数
  real<lower=0> x[n_sample]; //体サイズ
}

//推定したいパラメータの設定
//今回はポアソン回帰なので二つのβを推定する
parameters {
  real beta_1;  //切片
  real beta_2;  //xの係数
}

//線形予測子からλを算出
transformed parameters {
  real lambda[n_sample];
  for (n in 1:n_sample) {
    lambda[n] = beta_1 + beta_2 * x[n]; //線形予測子からλを算出
  }
}

//モデルの構成
//前半はパラメータの無情報事前分布を設定
//後半はリンク関数をlogとしたポアソン分布でyをフィッティングする
//その時に使用されるλはtransformed parametersで設定した線形予測子から得られるλである
model {
  beta_1 ~ normal(0, 100000);  //事前分布を無情報っぽい正規分布に
  beta_2 ~ normal(0, 100000);  //同上
  
  for (n in 1:n_sample) {
    y ~ poisson_log(lambda);  //種子数をポアソン分布とする、リンク関数がlogなのでpoisson_logを使う
  }
}

//予測区間も計算する
generated quantities {
  real yhat[n_sample];
  yhat = poisson_log_rng(lambda);
}
