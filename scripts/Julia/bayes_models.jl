using DataFrames
using StatsPlots
using Turing
using Queryverse
using Random

Random.seed!(2753) # prime number with each digit being prime

df = load("data/hale_data.dta") |> DataFrame

#df.y05 = 0
#df.y10 = 0
#df.y15 = 0
#
#if (df.year == 2005)
#        df.y05 = 1
#    elseif (df.year == 2010)
#        df.y10 = 1
#    elseif (df.year == 2015)
#        df.y15 = 1
#end
#
#if isnan(df.tropical)
#        df.tropical = 1
#end
#
#df.osv_per1000 = df.osv / 1000
#df.deathstotal_and_osv_1000 = df.deathstotal_and_osv_10000 / 10
#
df = select(df, :hale, :pko_years, :total_violence_1000,
    :osv_1000, :brd_1000, :pkoxdeath, :pkoxosv, :pkoxbrd, :civilwarborder, 
    :helog_knn, :hdi_knn,
    :urbangrowth_knn, :gini_knn, :tropical , :xpolity_knn, :ef_knn, :y05 , :y10,
    :y15)
dropmissing!(df)

# Define additive model
@model function model_add(y, x₁, x₂, x₃, 𝚡₄, 𝚡₅, 𝚡₆, 𝚡₇, 𝚡₈, 𝚡₉, 𝚡10,
    𝚡11, 𝚡12, 𝚡13)
  σ ~ truncated(Cauchy(0, 2), 0, Inf)

  βpko ~ Normal(1.714, 6.886)
  βdt ~ Normal(-0.363, 1.881)
  βbcw ~ Normal(-10.196, 43.414)
  βhe ~ Normal(2.109, 29.542)
  βhdi ~ Normal(5.520, 60.861)
  βug ~ Normal(-7.211, 40.847)
  βgini ~ Normal(-51.364, 359.454)
  βtrop ~ Normal(-3.22, 58.099)
  βpol ~ Normal(-0.135, 3.757)
  βef ~ Normal(-0.922, 18.047)
  β05 ~ Normal(0, 10)
  β10 ~ Normal(0, 10)
  β15 ~ Normal(0, 10)

  α ~ Normal(55, 10)

  μ = α .+ βpko * x₁ .+ βdt * x₂ .+ βbcw * x₃ .+ βhe * 𝚡₄ .+ βhdi * 𝚡₅ .+
    βug * 𝚡₆ .+ βgini * 𝚡₇ .+ βtrop * 𝚡₈ .+ βpol * 𝚡₉ .+ βef * 𝚡10 .+
    β05 * 𝚡11  .+ β10 * 𝚡12 .+ β15 * 𝚡13
  y .~ Normal.(μ, σ)
end


# Define interation model
@model function model_int(y, x₁, x₂, x₃, 𝚡₄, 𝚡₅, 𝚡₆, 𝚡₇, 𝚡₈, 𝚡₉, 𝚡10,
    𝚡11, 𝚡12, 𝚡13)
  σ ~ truncated(Cauchy(0, 2), 0, Inf)

  βpko ~ Normal(1.551, 7.509)
  βdt ~ Normal(-0.558, 4.630)
  βint ~ Normal(0.052, 1.113)
  βbcw ~ Normal(-10.161, 43.603)
  βhe ~ Normal(2.034, 29.953)
  βhdi ~ Normal(5.427, 61.087)
  βug ~ Normal(-7.229, 41.008)
  βgini ~ Normal(-52.547, 363.284)
  βtrop ~ Normal(-3.189, 58.275)
  βpol ~ Normal(-0.133, 3.771)
  βef ~ Normal(-0.967, 18.066)
  β05 ~ Normal(0, 10)
  β10 ~ Normal(0, 10)
  β15 ~ Normal(0, 10)

  α ~ Normal(55, 10)

  μ = α .+ βpko * x₁ .+ βdt * x₂ .+ βint * x₁ .* x₂ .+ βbcw * x₃ .+
    βhe * 𝚡₄ .+ βhdi * 𝚡₅ .+ βug * 𝚡₆ .+ βgini * 𝚡₇ .+ βtrop * 𝚡₈ .+
    βpol * 𝚡₉ .+ βef * 𝚡10 .+ β05 * 𝚡11  .+ β10 * 𝚡12 .+ β15 * 𝚡13
  y .~ Normal.(μ, σ)
end


# Additive, total violence
a_tv = model_add(df.dale, df.pkoyearsany, df.deathstotal_and_osv_1000,
    df.civilwarborder, df.helog_knn, df.hdi_knn, df.urbangrowth_knn,
    df.gini_knn, df.tropical, df.xpolity_knn, df.ef_knn, df.y05, df.y10,
    df.y15);

chains = sample(a_tv, NUTS(), 2000)
StatsPlots.plot(chains)


# Additive, one-sided violence
a_osv = model_add(df.dale, df.pkoyearsany, df.osv_per1000,
    df.civilwarborder, df.helog_knn, df.hdi_knn, df.urbangrowth_knn,
    df.gini_knn, df.tropical, df.xpolity_knn, df.ef_knn, df.y05, df.y10,
    df.y15);

chains = sample(a_osv, NUTS(), 2000)


# Additive, battle-related deaths
a_brd = model_add(df.dale, df.pkoyearsany, df.deathstotal_new,
    df.civilwarborder, df.helog_knn, df.hdi_knn, df.urbangrowth_knn,
    df.gini_knn, df.tropical, df.xpolity_knn, df.ef_knn, df.y05, df.y10,
    df.y15);

chains = sample(a_brd, NUTS(), 1000)

# Interaction, total violence
i_tv = model_int(df.hale, df.pko_years,
    df.pkoxdeath, df.civilwarborder, df.helog_knn, df.hdi_knn,
    df.urbangrowth_knn, df.gini_knn, df.tropical , df.xpolity_knn, df.ef_knn,
    df.y05 , df.y10, df.y15);

@time chains = sample(i_tv, NUTS(), 1000)


# Interaction, one-sided violence
i_osv = model_int(df.dale, df.pkoyearsany, df.osv_per1000,
    df.civilwarborder, df.helog_knn, df.hdi_knn, df.urbangrowth_knn,
    df.gini_knn, df.tropical, df.xpolity_knn, df.ef_knn, df.y05, df.y10,
    df.y15);

chains = sample(a_osv, NUTS(), 1000)


# Interaction, battle-related deaths
i_brd = model_int(df.dale, df.pkoyearsany, df.deathstotal_new,
    df.civilwarborder, df.helog_knn, df.hdi_knn, df.urbangrowth_knn,
    df.gini_knn, df.tropical, df.xpolity_knn, df.ef_knn, df.y05, df.y10,
    df.y15);

chains = sample(a_brd, NUTS(), 1000)
# Define interation model
@model function model_int(y, x₁, x₂, x₃, 𝚡₄, 𝚡₅, 𝚡₆, 𝚡₇, 𝚡₈, 𝚡₉, 𝚡10,
    𝚡11, 𝚡12, 𝚡13)
  σ ~ truncated(Cauchy(0, 2), 0, Inf)

  βpko ~ Normal(1.714, 6.886)
  βdt ~ Normal(-0.363, 1.881)
  βint ~ Normal(0.052, 1.113)
  βbcw ~ Normal(-10.196, 43.414)
  βhe ~ Normal(2.109, 29.542)
  βhdi ~ Normal(5.520, 60.861)
  βug ~ Normal(-7.211, 40.847)
  βgini ~ Normal(-51.364, 359.454)
  βtrop ~ Normal(-3.22, 58.099)
  βpol ~ Normal(-0.135, 3.757)
  βef ~ Normal(-0.922, 18.047)
  β05 ~ Normal(0, 10)
  β10 ~ Normal(0, 10)
  β15 ~ Normal(0, 10)

  α ~ Normal(55, 10)

  μ = α .+ βpko * x₁ .+ βdt * x₂ .+ βint * x₁ .* x₂ .+ βbcw * x₃ .+
    βhe * 𝚡₄ .+ βhdi * 𝚡₅ .+ βug * 𝚡₆ .+ βgini * 𝚡₇ .+ βtrop * 𝚡₈ .+
    βpol * 𝚡₉ .+ βef * 𝚡10 .+ β05 * 𝚡11  .+ β10 * 𝚡12 .+ β15 * 𝚡13
  y .~ Normal.(μ, σ)
end


# regularized priors

# Define interation model
@model function model_int(y, x₁, x₂, x₃, 𝚡₄, 𝚡₅, 𝚡₆, 𝚡₇, 𝚡₈, 𝚡₉, 𝚡10,
    𝚡11, 𝚡12, 𝚡13)
  σ ~ truncated(Cauchy(0, 2), 0, Inf)

  βpko ~ Normal(0, 10)
  βdt ~ Normal(0, 10)
  βint ~ Normal(0, 10)
  βbcw ~ Normal(0, 10)
  βhe ~ Normal(0, 10)
  βhdi ~ Normal(0, 10)
  βug ~ Normal(0, 10)
  βgini ~ Normal(0, 10)
  βtrop ~ Normal(0, 10)
  βpol ~ Normal(0, 10)
  βef ~ Normal(0, 10)
  β05 ~ Normal(0, 10)
  β10 ~ Normal(0, 10)
  β15 ~ Normal(0, 10)

  α ~ Normal(55, 10)

  μ = α .+ βpko * x₁ .+ βdt * x₂ .+ βint * x₁ .* x₂ .+ βbcw * x₃ .+
    βhe * 𝚡₄ .+ βhdi * 𝚡₅ .+ βug * 𝚡₆ .+ βgini * 𝚡₇ .+ βtrop * 𝚡₈ .+
    βpol * 𝚡₉ .+ βef * 𝚡10 .+ β05 * 𝚡11  .+ β10 * 𝚡12 .+ β15 * 𝚡13
  y .~ Normal.(μ, σ)
end

# Interaction, total violence
i_tv = model_int(df.hale, df.pko_years, df.total_violence_1000, 
    df.civilwarborder, df.helog_knn, df.hdi_knn, df.urbangrowth_knn,
    df.gini_knn, df.tropical, df.xpolity_knn, df.ef_knn, df.y05, df.y10,
    df.y15);

chains = sample(i_tv, NUTS(), 1000)
