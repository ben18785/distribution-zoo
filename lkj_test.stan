data{
  int K;
  corr_matrix[K] X;
  real eta;
}

generated quantities{
  real log_prob = lkj_corr_lpdf(X| eta);
}