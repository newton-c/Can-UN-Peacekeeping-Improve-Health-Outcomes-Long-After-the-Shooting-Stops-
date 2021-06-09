data {
        int <lower=0> N;
        int <lower=0> K;
        matrix[N, K] X;
        vector[N] y;
    }

parameters {
        real alpha;
        real Bpko;
        real Bdt;
        real Bint;
        real Bbcw;
        real Bhe;
        real Bhdi;
        real Bug;
        real Bgini;
        real Btrop;
        real Bpol;
        real Bef;
        real B05;
        real B10;
        real B15;
        real <lower=0> sigma;
    }

model {
        alpha ~ normal(55, 10);
        Bpko ~ normal(0.35, 1.42);
        Bdt ~ normal(-0.64, 4.89);
        Bint ~ normal(0.11, 2.38);
        Bbcw ~ normal(-0.60, 2.59);
        Bhe ~ normal(0.35, 5.08);
        Bhdi ~ normal(0.36, 4.02);
        Bug ~ normal(-0.99, 5.61);
        Bgini ~ normal(-0.53, 3.69);
        Btrop ~ normal(-0.38, 6.92);
        Bpol ~ normal(-0.11, 3.09);
        Bef ~ normal(-0.13, 2.45);
        B05 ~ normal(0, 10);
        B10 ~ normal(0, 10);
        B15 ~ normal(0, 10);
        sigma ~ cauchy(0, 2);
        y ~ normal(alpha + Bpko * X[1] + Bdt * X[2] + Bint * X[3] + Bhe * X[4] +
        Bhdi * X[5] + Bbcw * X[6] + Bug * X[7] + Bgini * X[8] + Btrop * X[9] +
        Bpol * X[10] + Bef * X[11] + B05 * X[12] + B10 * X[13] + B15 * X[14],
        sigma);
    }

generated quantities {
    }
