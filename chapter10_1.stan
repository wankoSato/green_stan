// �Q�l
// https://tjo.hatenablog.com/entry/2014/05/01/190346

// ���̓f�[�^
data {
  int<lower=0> n_sample;      //�T���v����
  int<lower=0> y[n_sample];   //������q��
}

// ���肵�����p�����[�^
// ����͌̍�r���܂�
parameters {
  real beta;          //�ؕЁF���K���z�ɏ]��
  real r[n_sample];   //�̍��F���Us�̐��K���z�ɏ]��
  real<lower=0> s;    //�̍��̕��U�F0-1000�̈�l���z�ɏ]��
}


// �e�̂�q����`�\���q������
transformed parameters {
  real q[n_sample];
  for (i in 1:n_sample){
    q[i] = inv_logit(beta + r[i]);
  }
}

// ���f���̐ݒ�
model {
  //�e�p�����[�^�̊m�����z
  beta ~ normal(0, 100000);  //beta�̖���񎖑O���z
  s ~ uniform(0, 1000); //s�̖���񎖑O���z
  // �x�N�g�����ňȉ��̂悤�ɏ����Ă��ǂ�
  // �n���u�T�{p313
  r ~ normal(0, s);
  //for (i in 1:n_sample){
  //  r[i] ~ normal(0, s);  //�e�̂��Ƃ�r
  //}
  
  //y�̐���
  y ~ binomial(8, q);
  //for (i in 1:n_sample){
  //  y[i] ~ binomial(8, q[i]);
  //}
}

//�\����Ԃ��v�Z����
generated quantities {
  int yhat[n_sample];
  for (i in 1:n_sample){
    yhat[i] = binomial_rng(8, q[i]);
  }
}
