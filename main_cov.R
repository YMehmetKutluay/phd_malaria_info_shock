## DEPENDENCIES ####
library(mlogit)

## LOAD DATA ####
# Load data
d <- read.csv("https://www.dropbox.com/s/gy8hitd47bumzvg/CE_R.csv?dl=1")
# Set the column number where choice-dependent variables begin
l_border <- 10
# Convert data into mlogit data, wide form
data <- mlogit.data(
  d, 
  choice = "choice", 
  shape = "wide", 
  varying = 14:549,
  id = "id",
  sep = ""
)

## RUN MLOGIT MODELS ####
# Set seed
set.seed(98457)
#save.image("session-main_cov.RData")
# Set number of draws
draws <- 1000
# Run baseline mixed logit model
baseline_mxl <- mlogit(choice~price + asc + pregnant + baby + child + protection + 
                       own_risk + duration + info_pregnant + info_child + info_baby + 
                       info_protection + info_ownrisk + info_duration + cn_pow_pregnant + 
                       cn_pow_child + cn_pow_baby + cn_pow_protection + cn_pow_ownrisk + 
                       cn_pow_duration + info_cn_pow_pregnant + info_cn_pow_child + 
                       info_cn_pow_baby + info_cn_pow_protection + info_cn_pow_ownrisk + 
                       info_cn_pow_duration| 0, data, 
                       rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                protection = 'n', own_risk = 'n', duration = 'n', 
                                info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                info_protection = 'n', info_ownrisk = 'n', info_duration = 'n',
                                cn_pow_pregnant = 'n', cn_pow_child = 'n', cn_pow_baby = 'n', 
                                cn_pow_protection = 'n', cn_pow_ownrisk = 'n', cn_pow_duration = 'n',
                                info_cn_pow_pregnant = 'n', info_cn_pow_child = 'n', 
                                info_cn_pow_baby = 'n', info_cn_pow_protection = 'n', 
                                info_cn_pow_ownrisk = 'n', info_cn_pow_duration = 'n'), 
                       R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)

baseline_aic <- AIC(baseline.mxl)

noschool_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + 
                         own_risk + duration + noschool_pregnant + noschool_baby + noschool_child + 
                         noschool_protection + noschool_ownrisk + noschool_duration + 
                         info_pregnant + info_child + info_baby + info_protection + info_ownrisk +
                         info_duration + cn_pow_pregnant + cn_pow_child + cn_pow_baby + 
                         cn_pow_protection + cn_pow_ownrisk + cn_pow_duration + 
                         info_cn_pow_pregnant + info_cn_pow_child + info_cn_pow_baby + 
                         info_cn_pow_protection + info_cn_pow_ownrisk + 
                         info_cn_pow_duration | 0, data, 
                       rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                protection = 'n', own_risk = 'n', duration = 'n', 
                                info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                info_protection = 'n', info_ownrisk = 'n', 
                                info_duration = 'n', cn_pow_pregnant = 'n', cn_pow_child = 'n',
                                cn_pow_baby = 'n', cn_pow_protection = 'n', cn_pow_ownrisk = 'n', 
                                cn_pow_duration = 'n', info_cn_pow_pregnant = 'n', 
                                info_cn_pow_child = 'n', info_cn_pow_baby = 'n', 
                                info_cn_pow_protection = 'n', info_cn_pow_ownrisk = 'n', 
                                info_cn_pow_duration = 'n'), 
                       R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)

noschool_aic <- AIC(noschool.mxl)

hhmostlyme_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + 
                           own_risk + duration + hh_mostly_me_pregnant + hh_mostly_me_baby + 
                           hh_mostly_me_child + hh_mostly_me_protection + hh_mostly_me_ownrisk +
                           hh_mostly_me_duration + info_pregnant + info_child + info_baby + 
                           info_protection + info_ownrisk + info_duration + cn_pow_pregnant + 
                           cn_pow_child + cn_pow_baby + cn_pow_protection + cn_pow_ownrisk + 
                           cn_pow_duration + info_cn_pow_pregnant + info_cn_pow_child + 
                           info_cn_pow_baby + info_cn_pow_protection + info_cn_pow_ownrisk + 
                           info_cn_pow_duration | 0, data, 
                         rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                  protection = 'n',own_risk = 'n', duration = 'n', 
                                  info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                  info_protection = 'n', info_ownrisk = 'n', info_duration = 'n', 
                                  cn_pow_pregnant = 'n', cn_pow_child = 'n', cn_pow_baby = 'n', 
                                  cn_pow_protection = 'n', cn_pow_ownrisk = 'n', 
                                  cn_pow_duration = 'n', info_cn_pow_pregnant = 'n', 
                                  info_cn_pow_child = 'n', info_cn_pow_baby = 'n',
                                  info_cn_pow_protection = 'n', info_cn_pow_ownrisk = 'n', 
                                  info_cn_pow_duration = 'n'), 
                         R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)

hhmostlyme_aic <- AIC(hhmostlyme.mxl)

monsoonmalaria_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + 
                               own_risk + duration + monsoon_malaria_pregnant + 
                               monsoon_malaria_baby + monsoon_malaria_child + 
                               monsoon_malaria_protection + monsoon_malaria_ownrisk + 
                               monsoon_malaria_duration + info_pregnant + 
                               info_child + info_baby + info_protection + info_ownrisk + 
                               info_duration + cn_pow_pregnant + cn_pow_child + cn_pow_baby + 
                               cn_pow_protection + cn_pow_ownrisk + cn_pow_duration + 
                               info_cn_pow_pregnant + info_cn_pow_child + info_cn_pow_baby + 
                               info_cn_pow_protection + info_cn_pow_ownrisk + 
                               info_cn_pow_duration | 0, data, 
                             rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                      protection = 'n', own_risk = 'n', duration = 'n', 
                                      info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                      info_protection = 'n', info_ownrisk = 'n', 
                                      info_duration = 'n', cn_pow_pregnant = 'n', 
                                      cn_pow_child = 'n', cn_pow_baby = 'n', 
                                      cn_pow_protection = 'n', cn_pow_ownrisk = 'n', 
                                      cn_pow_duration = 'n', info_cn_pow_pregnant = 'n', 
                                      info_cn_pow_child = 'n', info_cn_pow_baby = 'n', 
                                      info_cn_pow_protection = 'n', info_cn_pow_ownrisk = 'n',
                                      info_cn_pow_duration = 'n'), 
                             R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)

monsoonmalaria_aic <- AIC(monsoonmalaria.mxl)


symptommal_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + own_risk + 
                           duration + symptom_mal_pregnant + symptom_mal_baby + symptom_mal_child +
                           symptom_mal_protection + symptom_mal_ownrisk + symptom_mal_duration + 
                           info_pregnant + info_child + info_baby + info_protection + info_ownrisk + 
                           info_duration + cn_pow_pregnant + cn_pow_child + cn_pow_baby + 
                           cn_pow_protection + cn_pow_ownrisk + cn_pow_duration + 
                           info_cn_pow_pregnant + info_cn_pow_child + info_cn_pow_baby + 
                           info_cn_pow_protection + info_cn_pow_ownrisk + 
                           info_cn_pow_duration | 0, data, 
                         rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                  protection = 'n', own_risk = 'n', duration = 'n', 
                                  info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                  info_protection = 'n', info_ownrisk = 'n', info_duration = 'n', 
                                  cn_pow_pregnant = 'n', cn_pow_child = 'n', cn_pow_baby = 'n', 
                                  cn_pow_protection = 'n', cn_pow_ownrisk = 'n', 
                                  cn_pow_duration = 'n', info_cn_pow_pregnant = 'n', 
                                  info_cn_pow_child = 'n', info_cn_pow_baby = 'n', 
                                  info_cn_pow_protection = 'n', info_cn_pow_ownrisk = 'n', 
                                  info_cn_pow_duration = 'n'), 
                         R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)

symptommal_aic <- AIC(symptommal.mxl)

lnhhincome_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + own_risk + 
                           duration + ln_hh_income_pregnant + ln_hh_income_baby + 
                           ln_hh_income_child + ln_hh_income_protection + ln_hh_income_ownrisk + 
                           ln_hh_income_duration + info_pregnant + info_child + info_baby + 
                           info_protection + info_ownrisk + info_duration + cn_pow_pregnant + 
                           cn_pow_child + cn_pow_baby + cn_pow_protection + cn_pow_ownrisk + 
                           cn_pow_duration + info_cn_pow_pregnant + info_cn_pow_child + 
                           info_cn_pow_baby + info_cn_pow_protection + info_cn_pow_ownrisk + 
                           info_cn_pow_duration | 0, data, 
                         rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                  protection = 'n', own_risk = 'n', duration = 'n', 
                                  info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                  info_protection = 'n', info_ownrisk = 'n', info_duration = 'n', 
                                  cn_pow_pregnant = 'n', cn_pow_child = 'n', cn_pow_baby = 'n', 
                                  cn_pow_protection = 'n', cn_pow_ownrisk = 'n', 
                                  cn_pow_duration = 'n', info_cn_pow_pregnant = 'n', 
                                  info_cn_pow_child = 'n', info_cn_pow_baby = 'n', 
                                  info_cn_pow_protection = 'n', info_cn_pow_ownrisk = 'n', 
                                  info_cn_pow_duration = 'n'), 
                         R = draws, Halton = NA, print.level = 0, panel = TRUE,ncores = 2)

lnhhincome_aic <- AIC(lnhhincome.mxl)

hadmalaria_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + own_risk + 
                           duration + hadmalaria_pregnant + hadmalaria_baby + hadmalaria_child + 
                           hadmalaria_protection + hadmalaria_ownrisk + hadmalaria_duration + 
                           info_pregnant + info_child + info_baby + info_protection + info_ownrisk + 
                           info_duration + cn_pow_pregnant + cn_pow_child + cn_pow_baby + 
                           cn_pow_protection + cn_pow_ownrisk + cn_pow_duration + 
                           info_cn_pow_pregnant + info_cn_pow_child + info_cn_pow_baby + 
                           info_cn_pow_protection + info_cn_pow_ownrisk + 
                           info_cn_pow_duration | 0, data, 
                         rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                  protection = 'n', own_risk = 'n', duration = 'n', 
                                  info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                  info_protection = 'n', info_ownrisk = 'n', info_duration = 'n', 
                                  cn_pow_pregnant = 'n', cn_pow_child = 'n', cn_pow_baby = 'n', 
                                  cn_pow_protection = 'n', cn_pow_ownrisk = 'n', 
                                  cn_pow_duration = 'n', info_cn_pow_pregnant = 'n', 
                                  info_cn_pow_child = 'n', info_cn_pow_baby = 'n', 
                                  info_cn_pow_protection = 'n', info_cn_pow_ownrisk = 'n', 
                                  info_cn_pow_duration = 'n'), 
                         R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)

hadmalaria_aic <- AIC(hadmalaria.mxl)

mosqbreed_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + own_risk + 
                          duration + mosq_breed_pregnant + mosq_breed_baby + mosq_breed_child + 
                          mosq_breed_protection + mosq_breed_ownrisk + mosq_breed_duration + 
                          info_pregnant + info_child + info_baby + info_protection + info_ownrisk + 
                          info_duration + cn_pow_pregnant + cn_pow_child + cn_pow_baby + 
                          cn_pow_protection + cn_pow_ownrisk + cn_pow_duration + 
                          info_cn_pow_pregnant + info_cn_pow_child + info_cn_pow_baby + 
                          info_cn_pow_protection + info_cn_pow_ownrisk + 
                          info_cn_pow_duration | 0, data, 
                        rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                 protection = 'n', own_risk = 'n', duration = 'n', 
                                 info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                 info_protection = 'n', info_ownrisk = 'n', info_duration = 'n', 
                                 cn_pow_pregnant = 'n', cn_pow_child = 'n', cn_pow_baby = 'n', 
                                 cn_pow_protection = 'n', cn_pow_ownrisk = 'n', 
                                 cn_pow_duration = 'n', info_cn_pow_pregnant = 'n', 
                                 info_cn_pow_child = 'n', info_cn_pow_baby = 'n', 
                                 info_cn_pow_protection = 'n', info_cn_pow_ownrisk = 'n', 
                                 info_cn_pow_duration = 'n'), 
                        R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)

mosqbreed_aic <- AIC(mosqbreed.mxl)

malrisk_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + duration + 
                        mal_risk_pregnant + mal_risk_baby + mal_risk_child + mal_risk_protection +
                        mal_risk_duration + info_pregnant + info_child + info_baby + 
                        info_protection + info_ownrisk + info_duration + cn_pow_pregnant + 
                        cn_pow_child + cn_pow_baby + cn_pow_protection + cn_pow_ownrisk + 
                        cn_pow_duration + info_cn_pow_pregnant + info_cn_pow_child + 
                        info_cn_pow_baby + info_cn_pow_protection + info_cn_pow_ownrisk + 
                        info_cn_pow_duration | 0, data, 
                      rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                               protection = 'n', duration = 'n', info_pregnant = 'n', 
                               info_child = 'n', info_baby = 'n', info_protection = 'n', 
                               info_ownrisk = 'n', info_duration = 'n', cn_pow_pregnant = 'n', 
                               cn_pow_child = 'n', cn_pow_baby = 'n', cn_pow_protection = 'n',
                               cn_pow_ownrisk = 'n', cn_pow_duration = 'n', 
                               info_cn_pow_pregnant = 'n', info_cn_pow_child = 'n', 
                               info_cn_pow_baby = 'n', info_cn_pow_protection = 'n', 
                               info_cn_pow_ownrisk = 'n', info_cn_pow_duration = 'n'), 
                      R = draws, Halton = NA, print.level = 0, panel = TRUE,ncores = 2)

malrisk_aic <- AIC(malrisk.mxl)

varprior_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + own_risk +
                         duration + var_prior_pregnant + var_prior_baby + var_prior_child + 
                         var_prior_protection + var_prior_ownrisk + var_prior_duration + 
                         info_pregnant + info_child + info_baby + info_protection + info_ownrisk +
                         info_duration + cn_pow_pregnant + cn_pow_child + cn_pow_baby + 
                         cn_pow_protection + cn_pow_ownrisk + cn_pow_duration + 
                         info_cn_pow_pregnant + info_cn_pow_child + info_cn_pow_baby + 
                         info_cn_pow_protection + info_cn_pow_ownrisk + 
                         info_cn_pow_duration | 0, data, 
                       rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                protection = 'n', own_risk = 'n', duration = 'n', 
                                info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                info_protection = 'n', info_ownrisk = 'n', info_duration = 'n', 
                                cn_pow_pregnant = 'n', cn_pow_child = 'n', cn_pow_baby = 'n', 
                                cn_pow_protection = 'n', cn_pow_ownrisk = 'n', 
                                cn_pow_duration = 'n', info_cn_pow_pregnant = 'n', 
                                info_cn_pow_child = 'n', info_cn_pow_baby = 'n', 
                                info_cn_pow_protection = 'n', info_cn_pow_ownrisk = 'n', 
                                info_cn_pow_duration = 'n'), 
                       R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)

varprior_aic <- AIC(varprior.mxl)

write.table(
  stargazer(
    baseline_mxl, hadmalaria_mxl, monsoonmalaria_mxl, mosqbreed_mxl, symptommal_mxl, 
    type = "text", out.header = TRUE, selection.equation = TRUE
  ), 
  file = "main_cov1.txt"
)

write.table(
  stargazer(
    baseline_mxl, malrisk_mxl, varprior_mxl, type = "text", out.header = TRUE,
    selection.equation = TRUE
  ),
  file = "main_cov2.txt"
)

write.table(
  stargazer(
    baseline_mxl, hadmalaria_mxl, hhmostlyme_mxl, lnhhincome_mxl, malrisk_mxl, 
    monsoonmalaria_mxl, mosqbreed_mxl, noschool_mxl, symptommal_mxl, varprior_mxl, 
    type = "text", out.header = TRUE, selection.equation = TRUE
  ),
  file = "main_cov3.txt"
)

capture.output(
  stargazer(
    baseline_mxl, monsoonmalaria_mxl, mosqbreed_mxl, type = "latex", out.header = FALSE, 
    selection.equation = TRUE, model.names = TRUE, align = FALSE, float = TRUE, 
    single.row = TRUE, column.sep.width = "1pt", omit = 39:73
  ),
  file = file.path(getwd(),"Tex Files/tables/power_know_mal.tex")
)

capture.output(
  stargazer(
    baseline_mxl, malrisk_mxl, varprior_mxl, hadmalaria_mxl, type = "latex", out.header = FALSE, 
    selection.equation = TRUE, model.names = TRUE, align = FALSE, float = TRUE, 
    single.row = TRUE, column.sep.width = "1pt", omit = 44:68
  ),
  file = file.path(getwd(), "Tex Files/tables/power_mal.tex")
)

capture.output(
  stargazer(
    baseline_mxl, noschool_mxl, lnhhincome_mxl, hhmostlyme_mxl, type = "latex", 
    out.header = FALSE, selection.equation = TRUE, model.names = TRUE, align = FALSE,
    float = TRUE, single.row = TRUE, column.sep.width = "1pt", omit = 45:81
  ),
  file = file.path(getwd(), "Tex Files/tables/power_resp.tex")
)

