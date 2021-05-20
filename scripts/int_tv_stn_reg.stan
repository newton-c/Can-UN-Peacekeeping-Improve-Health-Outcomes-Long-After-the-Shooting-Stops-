data {
    int <lower=0> N;
    int <lower=0> K;
    matrix[N, K] X;
    vector[N] y;
}

parameters {
    real alpha;
    vector[K] beta;
    real <lower=0> sigma;
}

model {
    alpha ~ normal(55, 10);
    beta ~ normal(0, 10);
    y ~ normal(alpha + X * beta, sigma);
}

generated quantities {
    }
