//�|�A�\����A�ɂ�����stan�R�[�h�̏������Q�l
//https://stats.biopapyrus.jp/bayesian-statistics/estimation/poisson-regression.html
//https://kosugitti.github.io/JSSP2018Summer/jssp04.html

//���̓f�[�^�̐ݒ�
data {
  int<lower=0> n_sample;     //�T���v����
  int<lower=0> y[n_sample];  //��q��
  real<lower=0> x[n_sample]; //�̃T�C�Y
}

//���肵�����p�����[�^�̐ݒ�
//����̓|�A�\����A�Ȃ̂œ�̃��𐄒肷��
parameters {
  real beta_1;  //�ؕ�
  real beta_2;  //x�̌W��
}

//���`�\���q����ɂ��Z�o
transformed parameters {
  real lambda[n_sample];
  for (n in 1:n_sample) {
    lambda[n] = beta_1 + beta_2 * x[n]; //���`�\���q����ɂ��Z�o
  }
}

//���f���̍\��
//�O���̓p�����[�^�̖���񎖑O���z��ݒ�
//�㔼�̓����N�֐���log�Ƃ����|�A�\�����z��y���t�B�b�e�B���O����
//���̎��Ɏg�p�����ɂ�transformed parameters�Őݒ肵�����`�\���q���瓾����ɂł���
model {
  beta_1 ~ normal(0, 100000);  //���O���z�𖳏����ۂ����K���z��
  beta_2 ~ normal(0, 100000);  //����
  
  for (n in 1:n_sample) {
    y ~ poisson_log(lambda);  //��q�����|�A�\�����z�Ƃ���A�����N�֐���log�Ȃ̂�poisson_log���g��
  }
}

//�\����Ԃ��v�Z����
generated quantities {
  real yhat[n_sample];
  yhat = poisson_log_rng(lambda);
}
