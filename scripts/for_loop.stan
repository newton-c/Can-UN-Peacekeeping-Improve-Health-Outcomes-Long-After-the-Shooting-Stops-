
data{
    int<lower=1> N;
    real y[N];
    real pko_years[N];
    real total_violence_1000[N];
    real pkoxdeath[N];
    real helog_knn[N];
    real hdi_knn[N];
    real civilwarborder[N];
    real urbangrowth_knn[N];
    real gini_knn[N];
    real tropical[N];
    real xpolity_knn[N];
    real ef_knn[N];
    real y05[N];
    real y10[N];
    real y15[N];
}
        
parameters{
	real a;
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
	real<lower=0> sigma;
}

model{
	vector[N] mu;
	sigma ~ cauchy( 0 , 2 );
	B15 ~ normal( 0 , 10 );
	B10 ~ normal( 0 , 10 );
	B05 ~ normal( 0 , 10 );
	Bef ~ normal( -0.13 , 2.45 );
	Bpol ~ normal( -0.11 , 3.09 );
	Btrop ~ normal( -0.38 , 6.92 );
	Bgini ~ normal( -0.53 , 3.69 );
	Bug ~ normal( -0.99 , 5.61 );
	Bhdi ~ normal( 0.36 , 4.02 );
	Bhe ~ normal( 0.35 , 5.08 );
	Bbcw ~ normal( -0.6 , 2.59 );
	Bint ~ normal( 0.11 , 2.38 );
	Bdt ~ normal( -0.64 , 4.89 );
	Bpko ~ normal( 0.35 , 1.42 );
	a ~ normal( 55 , 10 );
	for ( i in 1:N ) {
		mu[i] = a + Bpko * pko_years[i] + Bdt * total_violence_1000[i] +
		Bint * pkoxdeath[i] + Bhe * helog_knn[i] + Bhdi * hdi_knn[i] +
		Bbcw * civilwarborder[i] + Bug * urbangrowth_knn[i] + 
		Bgini * gini_knn[i] + Btrop * tropical[i] + Bpol * xpolity_knn[i] +
		Bef * ef_knn[i] + B05 * y05[i] + B10 * y10[i] + B15 * y15[i];
	}
	y ~ normal( mu , sigma );
}

generated quantities{
	vector[N] mu;
	for ( i in 1:N ) {
		mu[i] = a + Bpko * pko_years[i] + Bdt * total_violence_1000[i] +
		Bint * pkoxdeath[i] + Bhe * helog_knn[i] + Bhdi * hdi_knn[i] +
		Bbcw * civilwarborder[i] + Bug * urbangrowth_knn[i] + 
		Bgini * gini_knn[i] + Btrop * tropical[i] + Bpol * xpolity_knn[i] +
		Bef * ef_knn[i] + B05 * y05[i] + B10 * y10[i] + B15 * y15[i];
	}
}
