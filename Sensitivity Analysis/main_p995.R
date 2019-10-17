rm(list=ls())
#setwd("C:/Users/yky400/Dropbox/Choice Experiment Mumbai/Choice Models R")
library(mlogit)
d<-read.csv("CE_R_p995.csv")
data<-mlogit.data(d,choice="choice",shape="wide",varying=7:474,
                  id="id",sep="")

library(reshape)
data<-rename(data,c(pi_pregnant = "p995_pregnant", pi_child = "p995_child", 
                    pi_baby = "p995_baby", pi_protection = "p995_protection", 
                    pi_ownrisk = "p995_ownrisk", pi_duration = "p995_duration", 
                    info_pi_pregnant="info_p995_pregnant", 
                    info_pi_child = "info_p995_child", 
                    info_pi_baby = "info_p995_baby", 
                    info_pi_protection = "info_p995_protection", 
                    info_pi_ownrisk = "info_p995_ownrisk", 
                    info_pi_duration="info_p995_duration"))

set.seed(98457)
save.image("session-main_p995.RData")
draws<-1000

baseline.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                     own_risk + duration + info_pregnant + info_child + info_baby + 
                     info_protection + info_ownrisk + info_duration + p995_pregnant + 
                     p995_child + p995_baby + p995_protection + p995_ownrisk + 
                     p995_duration + info_p995_pregnant + info_p995_child + 
                     info_p995_baby + info_p995_protection + info_p995_ownrisk + 
                     info_p995_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                     baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                     info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                     info_duration='n', p995_pregnant='n', p995_child='n', p995_baby='n', 
                     p995_protection='n', p995_ownrisk='n', p995_duration='n', 
                     info_p995_pregnant='n', info_p995_child='n', info_p995_baby='n',
                     info_p995_protection='n', info_p995_ownrisk='n', info_p995_duration='n'),
                     R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
baseline.aic<-AIC(baseline.mxl)

save.image("session-main_p995.RData")
noschool.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                     own_risk + duration + noschool_pregnant + noschool_baby + noschool_child + 
                     noschool_protection + noschool_ownrisk + noschool_duration + info_pregnant + 
                     info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                     p995_pregnant + p995_child + p995_baby + p995_protection + p995_ownrisk +
                     p995_duration + info_p995_pregnant + info_p995_child + 
                     info_p995_baby + info_p995_protection + info_p995_ownrisk + 
                     info_p995_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                     baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                     info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                     info_duration='n', p995_pregnant='n', p995_child='n', p995_baby='n', 
                     p995_protection='n', p995_ownrisk='n', p995_duration='n', 
                     info_p995_pregnant='n', info_p995_child='n', info_p995_baby='n',
                     info_p995_protection='n', info_p995_ownrisk='n', info_p995_duration='n'),
                     R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
noschool.aic<-AIC(noschool.mxl)

save.image("session-main_p995.RData")
hhmostlyme.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                       own_risk + duration + hh_mostly_me_pregnant + hh_mostly_me_baby + hh_mostly_me_child + 
                       hh_mostly_me_protection + hh_mostly_me_ownrisk + hh_mostly_me_duration + info_pregnant + 
                       info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                       p995_pregnant + p995_child + p995_baby + p995_protection + p995_ownrisk +
                       p995_duration + info_p995_pregnant + info_p995_child + 
                       info_p995_baby + info_p995_protection + info_p995_ownrisk + 
                       info_p995_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                       baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                       info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                       info_duration='n', p995_pregnant='n', p995_child='n', p995_baby='n', 
                       p995_protection='n', p995_ownrisk='n', p995_duration='n', 
                       info_p995_pregnant='n', info_p995_child='n', info_p995_baby='n',
                       info_p995_protection='n', info_p995_ownrisk='n', info_p995_duration='n'),
                       R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
hhmostlyme.aic<-AIC(hhmostlyme.mxl)

save.image("session-main_p995.RData")
monsoonmalaria.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                           own_risk + duration + monsoon_malaria_pregnant + monsoon_malaria_baby + monsoon_malaria_child + 
                           monsoon_malaria_protection + monsoon_malaria_ownrisk + monsoon_malaria_duration + info_pregnant + 
                           info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                           p995_pregnant + p995_child + p995_baby + p995_protection + p995_ownrisk +
                           p995_duration + info_p995_pregnant + info_p995_child + 
                           info_p995_baby + info_p995_protection + info_p995_ownrisk + 
                           info_p995_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                           baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                           info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                           info_duration='n', p995_pregnant='n', p995_child='n', p995_baby='n', 
                           p995_protection='n', p995_ownrisk='n', p995_duration='n', 
                           info_p995_pregnant='n', info_p995_child='n', info_p995_baby='n',
                           info_p995_protection='n', info_p995_ownrisk='n', info_p995_duration='n'),
                           R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
monsoonmalaria.aic<-AIC(monsoonmalaria.mxl)

save.image("session-main_p995.RData")
symptommal.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                       own_risk + duration + symptom_mal_pregnant + symptom_mal_baby + symptom_mal_child + 
                       symptom_mal_protection + symptom_mal_ownrisk + symptom_mal_duration + info_pregnant + 
                       info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                       p995_pregnant + p995_child + p995_baby + p995_protection + p995_ownrisk +
                       p995_duration + info_p995_pregnant + info_p995_child + 
                       info_p995_baby + info_p995_protection + info_p995_ownrisk + 
                       info_p995_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                       baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                       info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                       info_duration='n', p995_pregnant='n', p995_child='n', p995_baby='n', 
                       p995_protection='n', p995_ownrisk='n', p995_duration='n', 
                       info_p995_pregnant='n', info_p995_child='n', info_p995_baby='n',
                       info_p995_protection='n', info_p995_ownrisk='n', info_p995_duration='n'),
                       R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
symptommal.aic<-AIC(symptommal.mxl)

save.image("session-main_p995.RData")
lnhhincome.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                       own_risk + duration + ln_hh_income_pregnant + ln_hh_income_baby + ln_hh_income_child + 
                       ln_hh_income_protection + ln_hh_income_ownrisk + ln_hh_income_duration + info_pregnant + 
                       info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                       p995_pregnant + p995_child + p995_baby + p995_protection + p995_ownrisk +
                       p995_duration + info_p995_pregnant + info_p995_child + 
                       info_p995_baby + info_p995_protection + info_p995_ownrisk + 
                       info_p995_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                       baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                       info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                       info_duration='n', p995_pregnant='n', p995_child='n', p995_baby='n', 
                       p995_protection='n', p995_ownrisk='n', p995_duration='n', 
                       info_p995_pregnant='n', info_p995_child='n', info_p995_baby='n',
                       info_p995_protection='n', info_p995_ownrisk='n', info_p995_duration='n'),
                       R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
lnhhincome.aic<-AIC(lnhhincome.mxl)

save.image("session-main_p995.RData")
hadmalaria.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                       own_risk + duration + hadmalaria_pregnant + hadmalaria_baby + hadmalaria_child + 
                       hadmalaria_protection + hadmalaria_ownrisk + hadmalaria_duration + info_pregnant + 
                       info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                       p995_pregnant + p995_child + p995_baby + p995_protection + p995_ownrisk +
                       p995_duration + info_p995_pregnant + info_p995_child + 
                       info_p995_baby + info_p995_protection + info_p995_ownrisk + 
                       info_p995_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                       baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                       info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                       info_duration='n', p995_pregnant='n', p995_child='n', p995_baby='n', 
                       p995_protection='n', p995_ownrisk='n', p995_duration='n', 
                       info_p995_pregnant='n', info_p995_child='n', info_p995_baby='n',
                       info_p995_protection='n', info_p995_ownrisk='n', info_p995_duration='n'),
                       R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
hadmalaria.aic<-AIC(hadmalaria.mxl)

save.image("session-main_p995.RData")
mosqbreed.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                    own_risk + duration + mosq_breed_pregnant + mosq_breed_baby + mosq_breed_child + 
                    mosq_breed_protection + mosq_breed_ownrisk + mosq_breed_duration + info_pregnant + 
                    info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                    p995_pregnant + p995_child + p995_baby + p995_protection + p995_ownrisk +
                    p995_duration + info_p995_pregnant + info_p995_child + 
                    info_p995_baby + info_p995_protection + info_p995_ownrisk + 
                    info_p995_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                    baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                    info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                    info_duration='n', p995_pregnant='n', p995_child='n', p995_baby='n', 
                    p995_protection='n', p995_ownrisk='n', p995_duration='n', 
                    info_p995_pregnant='n', info_p995_child='n', info_p995_baby='n',
                    info_p995_protection='n', info_p995_ownrisk='n', info_p995_duration='n'),
                    R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
mosqbreed.aic<-AIC(mosqbreed.mxl)

save.image("session-main_p995.RData")
malrisk.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                    duration + mal_risk_pregnant + mal_risk_baby + mal_risk_child + 
                    mal_risk_protection + mal_risk_duration + info_pregnant + 
                    info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                    p995_pregnant + p995_child + p995_baby + p995_protection + p995_ownrisk +
                    p995_duration + info_p995_pregnant + info_p995_child + 
                    info_p995_baby + info_p995_protection + info_p995_ownrisk + 
                    info_p995_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                    baby='n', protection='n', duration='n', info_pregnant='n', 
                    info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                    info_duration='n', p995_pregnant='n', p995_child='n', p995_baby='n', 
                    p995_protection='n', p995_ownrisk='n', p995_duration='n', 
                    info_p995_pregnant='n', info_p995_child='n', info_p995_baby='n',
                    info_p995_protection='n', info_p995_ownrisk='n', info_p995_duration='n'),
                    R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
malrisk.aic<-AIC(malrisk.mxl)

save.image("session-main_p995.RData")
varprior.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                     own_risk + duration + var_prior_pregnant + var_prior_baby + var_prior_child + 
                     var_prior_protection + var_prior_ownrisk + var_prior_duration + info_pregnant + 
                     info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                     p995_pregnant + p995_child + p995_baby + p995_protection + p995_ownrisk +
                     p995_duration + info_p995_pregnant + info_p995_child + 
                     info_p995_baby + info_p995_protection + info_p995_ownrisk + 
                     info_p995_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                     baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                     info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                     info_duration='n', p995_pregnant='n', p995_child='n', p995_baby='n', 
                     p995_protection='n', p995_ownrisk='n', p995_duration='n', 
                     info_p995_pregnant='n', info_p995_child='n', info_p995_baby='n',
                     info_p995_protection='n', info_p995_ownrisk='n', info_p995_duration='n'),
                     R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
varprior.aic<-AIC(varprior.mxl)

#write.table(stargazer(baseline.mxl, hadmalaria.mxl, monsoonmalaria.mxl, 
#                      mosqbreed.mxl, symptommal.mxl,
#                      type="text", out.header=TRUE,selection.equation=TRUE),
#            file="main_cov1.txt")
#write.table(stargazer(baseline.mxl, malrisk.mxl, varprior.mxl,
#                      type="text", out.header=TRUE,selection.equation=TRUE),
#            file="main_cov2.txt")
#write.table(stargazer(baseline.mxl, hadmalaria.mxl, hhmostlyme.mxl, 
#                      lnhhincome.mxl, malrisk.mxl, monsoonmalaria.mxl, 
#                      mosqbreed.mxl, noschool.mxl, symptommal.mxl, varprior.mxl,
#                      type="text", out.header=TRUE,selection.equation=TRUE),
#            file="main_cov3.txt")
#capture.output(stargazer(baseline.mxl, monsoonmalaria.mxl,
#                         mosqbreed.mxl, type="latex", out.header=FALSE,
#                         selection.equation=TRUE, model.names=TRUE,align=FALSE,float=TRUE,
#                         single.row=TRUE, column.sep.width="1pt", omit=39:73),
#               file="C:/Users/yky400/Dropbox/Choice Experiment Mumbai/Tex and Do File/tables/power_know_mal.tex")

#capture.output(stargazer(baseline.mxl, malrisk.mxl, varprior.mxl, hadmalaria.mxl,
#                         type="latex", out.header=FALSE,
#                         selection.equation=TRUE, model.names=TRUE,align=FALSE,float=TRUE,
#                         single.row=TRUE, column.sep.width="1pt", omit=44:68),
#               file="C:/Users/yky400/Dropbox/Choice Experiment Mumbai/Tex and Do File/tables/power_mal.tex")

#capture.output(stargazer(baseline.mxl, noschool.mxl, lnhhincome.mxl,hhmostlyme.mxl,
#                         type="latex", out.header=FALSE,
#                         selection.equation=TRUE, model.names=TRUE,align=FALSE,float=TRUE,
#                         single.row=TRUE, column.sep.width="1pt", omit=45:81),
#               file="C:/Users/yky400/Dropbox/Choice Experiment Mumbai/Tex and Do File/tables/power_resp.tex")

save.image("session-main_p995.RData")