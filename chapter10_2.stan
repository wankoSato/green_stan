// ���̓f�[�^
data {
  int<lower=0> n_sample;      //�T���v����
  int<lower=0> n_pot;         //����
  int<lower=0> pot[n_sample]; //���ԍ� �_�~�[�ϐ�1~10
  int<lower=0> f[n_sample];   //�{����� �_�~�[�ϐ�0 or 1
  int<lower=0> y[n_sample];   //������q��
}

// ���肵�����p�����[�^
// ����͌̍�r���܂�
parameters {
  real beta_1;        //�ؕЁF���K���z�ɏ]��
  real beta_2;        //�{����ʂ̌W��
  real r[n_sample];   //�̍��F���Us�̐��K���z�ɏ]��
  real<lower=0> s;    //�̍��̕��U�F0-1000�̈�l���z�ɏ]��
  real rp[n_pot];     //����
  real<lower=0> sp;   //�����̕��U
}


// �e�̂�q����`�\���q������
transformed parameters {
  real lambda[n_sample];
  for (i in 1:n_sample){
    lambda[i] = beta_1 + beta_2 * f[i] + r[i] + rp[pot[i]];
  }
}

// ���f���̐ݒ�
model {
  //�e�p�����[�^�̊m�����z
  beta_1 ~ normal(0, 100000);  //beta_1�̖���񎖑O���z
  beta_2 ~ normal(0, 100000);  //beta_2�̖���񎖑O���z
  s ~ uniform(0, 1000);        //s�̖���񎖑O���z(�̍�)
  sp ~ uniform(0, 1000);       //sp�̖���񎖑O���z(����)
  r ~ normal(0, s);            //�̍�r�͍��L�̕��z���甭������
  rp ~ normal(0, sp);          //����rp�͍��L�̕��z���甭������
  
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
