using DataFrames
using StatsPlots
using Turing
using Queryverse

df = load("data/matched_data.dta") |> DataFrame

df.y05 = 0
df.y10 = 0
df.y15 = 0

if (df.year == 2005)
    df.y05 = 1

elseif (df.year == 2010)
    df.y10 = 1

elseif (df.year == 2015)
    df.y15 = 1
end

df.pkoxdeath = df.pkoyearsany * df.deathstotal_and_osv_10000

@model function m4(y, x1, x2, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
    α ~ Normal(0, 1)
    βpko ~ Normal(0, 1),
    βdt ~ Normal(0, 1),
    βint ~ Normal(0, 1),
    βidp ~ Normal(0, 1),
    βhe ~ Normal(0, 1),
    βhdi ~ Normal(0, 1),
    βbcw ~ Normal(0, 1),
    βug ~ Normal(0, 1),
    βgini ~ Normal(0, 1),
    βtrop ~ Normal(0, 1),
    βpol ~ Normal(0, 1),
    βef ~ Normal(0, 1),
    βprior ~ Normal(0, 1),
    β05 ~ Normal(0, 1),
    β10 ~ Normal(0, 1),
    β15 ~ Normal(0, 1),
    σ ~ truncated(Cauchy(0, 1), 0, Inf)

    μ = α .+ βpko * x1 .+ βdt * x2 .+ βidp * x4 + βhe * x5 .+
        βhdi * x6 .+ βbcw * x7 .+ βug * x8 .+ βgini * x9 .+ βtrop * x10 .+
        βpol * x11 .+ βef * x12 .+ βprior * x13 .+ β05 * x14 .+ β10 * x15 .+
        β15 * x16

    y .~ Normal.(μ, σ)
end

model = m4(df.dale, df.pkoyearsany, df.deathstotal_and_osv_10000,
            df.idps_per1000, df.helog_knn,
            df.hdi_knn, df.civilwarborder, df.urbangrowth_knn,
            df.gini_knn, df.tropical , df.xpolity_knn, df.ef_knn,
            df.priorpko, df.y05 , df.y10, df.y15)

chains = sample(model, NUTS(), 1000)
