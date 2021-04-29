// �Q�l
// https://gaiasky.hatenablog.com/entry/2018/08/15/205222
// https://qiita.com/tackey/items/96171be393912fb333b3
// https://github.com/takitsuba/midoribon/blob/master/Chap11/Chap11.ipynb

// ���̓f�[�^
data {
  int<lower=0> n_sample;      //�T���v����
  int<lower=0> y[n_sample];   //�ϑ��̐�
  //int<lower=0> near[n_sample];//�ߖT��, i=1,50�̂Ƃ���1�A����ȊO��2�Ƃ���
}

// ���肵�����p�����[�^
// ����͌̍�r���܂�
// log(��_i) = �� + r_i
parameters {
  real beta;        //�ؕЁF���K���z�ɏ]��
  real mu_zero;
  real r[n_sample];   //�ꏊ���F���Us�̐��K���z�ɏ]��
  real<lower=0> s;    //�ꏊ���̕��U�F0-1000�̈�l���z�ɏ]��
}


// �e�̂�q����`�\���q������
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

// ���f���̐ݒ�
model {
  //�e�p�����[�^�̊m�����z
  beta ~ normal(0, 1000);    //beta_1�̖���񎖑O���z
  s ~ uniform(0, 10);        //s�̖���񎖑O���z(�̍�)
  //for(i in 1:n_sample){        //�̍�r�͍��L�̕��z���甭������
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
              
  //y�̐���
  y ~ poisson_log(lambda);

}

//�\����Ԃ��v�Z����
generated quantities {
  real yhat[n_sample];
  for (i in 1:n_sample){
    yhat[i] = poisson_log_rng(lambda[i]);
  }
}
