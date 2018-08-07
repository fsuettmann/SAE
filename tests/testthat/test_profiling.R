library(SAE)

context("Evaluates speed of some functions")

load("C:/Users/nikos/Desktop/SAE_proj_data.RData")


Rprof("profiling package")

debugonce(bootstrap.y)

y_boot <- bootstrap.y(model1 = model, model_fit1 = inference_survey$model_fit_surv,
                      censusdata1 = censusdata, n_boot1 = 50, n_obs = n_obs_census)
Rprof(NULL)

summaryRprof("profiling package")
