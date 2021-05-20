# Can UN Peacekeeping Improve Health Outcomes Long After the Shooting Stops?

This is a repository for all of the files needed to replicate Benson, Newton, Roy, and Tucker (forthcoming). Most of the analysis is done in R, but the Bayesian models use Stan, so that needs to be installed a head of time.
In addition, results are replicated in Julia using Turing.jl. This was initially done for reliability, but the Julia models are more performant, and can be used it runtime is an issue. 

`main.R` will run the complete results in R. This can take some time. If you only want to run certain models, all of the scripts can be run separately, but they must be run in order. 
Most models run on 2-core, but this can be increased if you have the computing power. To adjust the cores used, modify the scripts with `models` in the name (the code specifies `cores = n`).

