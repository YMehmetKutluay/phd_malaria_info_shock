## DEPENDENCIES ####
library(mlogit)
## LOAD DATA ####
d <- read.csv("https://www.dropbox.com/s/n610o6zool2xxdg/CE_R_max.csv?dl=1")

data <- mlogit.data(d, choice = "choice", shape = "wide", varying = 10:501,
                  id = "id", sep = "")
## RUN MLOGIT MODELS ####
# Set seed for likelihood maximization
set.seed(98457)
# Set number of draws for random parameters
draws <- 1000
# Baseline model
baseline_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + 
                         own_risk + duration + info_pregnant + info_child + info_baby + 
                         info_protection + info_ownrisk + info_duration + max_pregnant + 
                         max_child + max_baby + max_protection + max_ownrisk + max_duration + 
                         info_max_pregnant + info_max_child + info_max_baby + info_max_protection +
                         info_max_ownrisk + info_max_duration |0, data, 
                       rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                protection = 'n',own_risk = 'n', duration = 'n', 
                                info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                info_protection = 'n', info_ownrisk = 'n', 
                                info_duration = 'n', max_pregnant = 'n', max_child = 'n', 
                                max_baby = 'n', max_protection = 'n', max_ownrisk = 'n', 
                                max_duration = 'n', info_max_pregnant = 'n', info_max_child = 'n',
                                info_max_baby = 'n', info_max_protection = 'n', 
                                info_max_ownrisk = 'n', info_max_duration = 'n'),
                       R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)

baseline_aic <- AIC(baseline_mxl)

# Baseline + impact of no schooling
noschool_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + 
                         own_risk + duration + noschool_pregnant + noschool_baby + noschool_child + 
                         noschool_protection + noschool_ownrisk + noschool_duration + 
                         info_pregnant + info_child + info_baby + info_protection + 
                         info_ownrisk + info_duration + max_pregnant + max_child + max_baby + 
                         max_protection + max_ownrisk + max_duration + info_max_pregnant + 
                         info_max_child + info_max_baby + info_max_protection + 
                         info_max_ownrisk + info_max_duration| 0, data, 
                       rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                protection = 'n', own_risk = 'n', duration = 'n', 
                                info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                info_protection = 'n', info_ownrisk = 'n', info_duration = 'n',
                                max_pregnant = 'n', max_child = 'n', max_baby = 'n', 
                                max_protection = 'n', max_ownrisk = 'n', max_duration = 'n', 
                                info_max_pregnant = 'n', info_max_child = 'n', info_max_baby = 'n', 
                                info_max_protection = 'n', info_max_ownrisk = 'n', 
                                info_max_duration = 'n'), 
                       R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)
noschool_aic <- AIC(noschool_mxl)

# Baseline and impact of respondent making most household purchasing decisions
hhmostlyme_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + 
                           own_risk + duration + hh_mostly_me_pregnant + hh_mostly_me_baby + 
                           hh_mostly_me_child + hh_mostly_me_protection + hh_mostly_me_ownrisk + 
                           hh_mostly_me_duration + info_pregnant + info_child + info_baby + 
                           info_protection + info_ownrisk + info_duration + max_pregnant + 
                           max_child + max_baby + max_protection + max_ownrisk + 
                           max_duration + info_max_pregnant + info_max_child + info_max_baby + 
                           info_max_protection + info_max_ownrisk + info_max_duration| 0, data, 
                         rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                  protection = 'n', own_risk = 'n', duration = 'n', 
                                  info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                  info_protection = 'n', info_ownrisk = 'n', info_duration = 'n',
                                  max_pregnant = 'n', max_child = 'n', max_baby = 'n', 
                                  max_protection = 'n', max_ownrisk = 'n', max_duration = 'n',
                                  info_max_pregnant = 'n', info_max_child = 'n', 
                                  info_max_baby = 'n', info_max_protection = 'n', 
                                  info_max_ownrisk = 'n', info_max_duration = 'n'), 
                         R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)
hhmostlyme_aic <- AIC(hhmostlyme_mxl)

# Baseline and impact of knowing malaria seasonality (in relation to monsoon season)
monsoonmalaria_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + 
                               own_risk + duration + monsoon_malaria_pregnant + 
                               monsoon_malaria_baby + monsoon_malaria_child + 
                               monsoon_malaria_protection + monsoon_malaria_ownrisk + 
                               monsoon_malaria_duration + info_pregnant + info_child + info_baby + 
                               info_protection + info_ownrisk + info_duration + max_pregnant +
                               max_child + max_baby + max_protection + max_ownrisk + max_duration +
                               info_max_pregnant + info_max_child + info_max_baby + 
                               info_max_protection + info_max_ownrisk + info_max_duration| 0, data,
                             rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                      protection = 'n', own_risk = 'n', duration = 'n', 
                                      info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                      info_protection = 'n', info_ownrisk = 'n', info_duration = 'n',
                                      max_pregnant = 'n', max_child = 'n', max_baby = 'n', 
                                      max_protection = 'n', max_ownrisk = 'n', max_duration = 'n',
                                      info_max_pregnant = 'n', info_max_child = 'n', 
                                      info_max_baby = 'n', info_max_protection = 'n', 
                                      info_max_ownrisk = 'n', info_max_duration = 'n'), 
                             R = draws, Halton = NA, print.level = 0, panel = TRUE, ncores = 2)
monsoonmalaria_aic <- AIC(monsoonmalaria_mxl)

# Baseline and impact of knowing malaria symptoms
symptommal_mxl <- mlogit(choice ~ price + asc + pregnant + baby + child + protection + own_risk +
                           duration + symptom_mal_pregnant + symptom_mal_baby + symptom_mal_child +
                           symptom_mal_protection + symptom_mal_ownrisk + symptom_mal_duration +
                           info_pregnant + info_child + info_baby + info_protection + info_ownrisk +
                           info_duration + max_pregnant + max_child + max_baby + max_protection + 
                           max_ownrisk + max_duration + info_max_pregnant + info_max_child + 
                           info_max_baby + info_max_protection + info_max_ownrisk + 
                           info_max_duration| 0, data, 
                         rpar = c(asc = 'n', pregnant = 'n', child = 'n', baby = 'n', 
                                  protection = 'n',own_risk = 'n', duration = 'n', 
                                  info_pregnant = 'n', info_child = 'n', info_baby = 'n', 
                                  info_protection = 'n', info_ownrisk = 'n', info_duration = 'n',
                                  max_pregnant = 'n', max_child = 'n', max_baby = 'n', 
                                  max_protection = 'n', max_ownrisk = 'n', max_duration = 'n',
                                  info_max_pregnant = 'n', info_max_child = 'n', info_max_baby = 'n',
                                  info_max_protection = 'n', info_max_ownrisk = 'n', 
                                  info_max_duration = 'n'),
                         R = draws, Halton = NA, print.level = 0, panel = TRUE,ncores = 2)
symptommal_aic<-AIC(symptommal_mxl)


lnhhincome_mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                       own_risk + duration + ln_hh_income_pregnant + ln_hh_income_baby + ln_hh_income_child + 
                       ln_hh_income_protection + ln_hh_income_ownrisk + ln_hh_income_duration + info_pregnant + 
                       info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                       max_pregnant + max_child + max_baby + max_protection + max_ownrisk +
                       max_duration + info_max_pregnant + info_max_child + 
                       info_max_baby + info_max_protection + info_max_ownrisk + 
                       info_max_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                       baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                       info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                       info_duration='n', max_pregnant='n', max_child='n', max_baby='n', 
                       max_protection='n', max_ownrisk='n', max_duration='n', 
                       info_max_pregnant='n', info_max_child='n', info_max_baby='n',
                       info_max_protection='n', info_max_ownrisk='n', info_max_duration='n'),
                       R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
lnhhincome_aic<-AIC(lnhhincome_mxl)


hadmalaria_mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                       own_risk + duration + hadmalaria_pregnant + hadmalaria_baby + hadmalaria_child + 
                       hadmalaria_protection + hadmalaria_ownrisk + hadmalaria_duration + info_pregnant + 
                       info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                       max_pregnant + max_child + max_baby + max_protection + max_ownrisk +
                       max_duration + info_max_pregnant + info_max_child + 
                       info_max_baby + info_max_protection + info_max_ownrisk + 
                       info_max_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                       baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                       info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                       info_duration='n', max_pregnant='n', max_child='n', max_baby='n', 
                       max_protection='n', max_ownrisk='n', max_duration='n', 
                       info_max_pregnant='n', info_max_child='n', info_max_baby='n',
                       info_max_protection='n', info_max_ownrisk='n', info_max_duration='n'),
                       R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
hadmalaria_aic<-AIC(hadmalaria_mxl)

save.image("session-main_max.RData")
mosqbreed_mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                    own_risk + duration + mosq_breed_pregnant + mosq_breed_baby + mosq_breed_child + 
                    mosq_breed_protection + mosq_breed_ownrisk + mosq_breed_duration + info_pregnant + 
                    info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                    max_pregnant + max_child + max_baby + max_protection + max_ownrisk +
                    max_duration + info_max_pregnant + info_max_child + 
                    info_max_baby + info_max_protection + info_max_ownrisk + 
                    info_max_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                    baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                    info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                    info_duration='n', max_pregnant='n', max_child='n', max_baby='n', 
                    max_protection='n', max_ownrisk='n', max_duration='n', 
                    info_max_pregnant='n', info_max_child='n', info_max_baby='n',
                    info_max_protection='n', info_max_ownrisk='n', info_max_duration='n'),
                    R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
mosqbreed_aic<-AIC(mosqbreed_mxl)

malrisk_mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                    duration + mal_risk_pregnant + mal_risk_baby + mal_risk_child + 
                    mal_risk_protection + mal_risk_duration + info_pregnant + 
                    info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                    max_pregnant + max_child + max_baby + max_protection + max_ownrisk +
                    max_duration + info_max_pregnant + info_max_child + 
                    info_max_baby + info_max_protection + info_max_ownrisk + 
                    info_max_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                    baby='n', protection='n', duration='n', info_pregnant='n', 
                    info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                    info_duration='n', max_pregnant='n', max_child='n', max_baby='n', 
                    max_protection='n', max_ownrisk='n', max_duration='n', 
                    info_max_pregnant='n', info_max_child='n', info_max_baby='n',
                    info_max_protection='n', info_max_ownrisk='n', info_max_duration='n'),
                    R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
malrisk_aic<-AIC(malrisk_mxl)

varprior_mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                     own_risk + duration + var_prior_pregnant + var_prior_baby + var_prior_child + 
                     var_prior_protection + var_prior_ownrisk + var_prior_duration + info_pregnant + 
                     info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                     max_pregnant + max_child + max_baby + max_protection + max_ownrisk +
                     max_duration + info_max_pregnant + info_max_child + 
                     info_max_baby + info_max_protection + info_max_ownrisk + 
                     info_max_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                     baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                     info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                     info_duration='n', max_pregnant='n', max_child='n', max_baby='n', 
                     max_protection='n', max_ownrisk='n', max_duration='n', 
                     info_max_pregnant='n', info_max_child='n', info_max_baby='n',
                     info_max_protection='n', info_max_ownrisk='n', info_max_duration='n'),
                     R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
varprior_aic<-AIC(varprior_mxl)

#write.table(stargazer(baseline_mxl, hadmalaria_mxl, monsoonmalaria_mxl, 
#                      mosqbreed_mxl, symptommal_mxl,
#                      type="text", out.header=TRUE,selection.equation=TRUE),
#            file="main_cov1.txt")
#write.table(stargazer(baseline_mxl, malrisk_mxl, varprior_mxl,
#                      type="text", out.header=TRUE,selection.equation=TRUE),
#            file="main_cov2.txt")
#write.table(stargazer(baseline_mxl, hadmalaria_mxl, hhmostlyme_mxl, 
#                      lnhhincome_mxl, malrisk_mxl, monsoonmalaria_mxl, 
#                      mosqbreed_mxl, noschool_mxl, symptommal_mxl, varprior_mxl,
#                      type="text", out.header=TRUE,selection.equation=TRUE),
#            file="main_cov3.txt")
#capture.output(stargazer(baseline_mxl, monsoonmalaria_mxl,
#                         mosqbreed_mxl, type="latex", out.header=FALSE,
#                         selection.equation=TRUE, model.names=TRUE,align=FALSE,float=TRUE,
#                         single.row=TRUE, column.sep.width="1pt", omit=39:73),
#               file="C:/Users/yky400/Dropbox/Choice Experiment Mumbai/Tex and Do File/tables/power_know_mal.tex")

#capture.output(stargazer(baseline_mxl, malrisk_mxl, varprior_mxl, hadmalaria_mxl,
#                         type="latex", out.header=FALSE,
#                         selection.equation=TRUE, model.names=TRUE,align=FALSE,float=TRUE,
#                         single.row=TRUE, column.sep.width="1pt", omit=44:68),
#               file="C:/Users/yky400/Dropbox/Choice Experiment Mumbai/Tex and Do File/tables/power_mal.tex")

#capture.output(stargazer(baseline_mxl, noschool_mxl, lnhhincome_mxl,hhmostlyme_mxl,
#                         type="latex", out.header=FALSE,
#                         selection.equation=TRUE, model.names=TRUE,align=FALSE,float=TRUE,
#                         single.row=TRUE, column.sep.width="1pt", omit=45:81),
#               file="C:/Users/yky400/Dropbox/Choice Experiment Mumbai/Tex and Do File/tables/power_resp.tex")
