rm(list=ls())
#setwd("C:/Users/yky400/Dropbox/Choice Experiment Mumbai/Choice Models R")
library(mlogit)
d<-read.csv("CE_R_p975.csv")
data<-mlogit.data(d,choice="choice",shape="wide",varying=7:474,
                  id="id",sep="")

library(reshape)
data<-rename(data,c(ph_pregnant = "p975_pregnant", ph_child = "p975_child", 
                    ph_baby = "p975_baby", ph_protection = "p975_protection", 
                    ph_ownrisk = "p975_ownrisk", ph_duration = "p975_duration", 
                    info_ph_pregnant="info_p975_pregnant", 
                    info_ph_child = "info_p975_child", 
                    info_ph_baby = "info_p975_baby", 
                    info_ph_protection = "info_p975_protection", 
                    info_ph_ownrisk = "info_p975_ownrisk", 
                    info_ph_duration="info_p975_duration"))

set.seed(98457)
save.image("session-main_p975.RData")
draws<-1000

baseline.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                     own_risk + duration + info_pregnant + info_child + info_baby + 
                     info_protection + info_ownrisk + info_duration + p975_pregnant + 
                     p975_child + p975_baby + p975_protection + p975_ownrisk + 
                     p975_duration + info_p975_pregnant + info_p975_child + 
                     info_p975_baby + info_p975_protection + info_p975_ownrisk + 
                     info_p975_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                     baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                     info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                     info_duration='n', p975_pregnant='n', p975_child='n', p975_baby='n', 
                     p975_protection='n', p975_ownrisk='n', p975_duration='n', 
                     info_p975_pregnant='n', info_p975_child='n', info_p975_baby='n',
                     info_p975_protection='n', info_p975_ownrisk='n', info_p975_duration='n'),
                     R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
baseline.aic<-AIC(baseline.mxl)

save.image("session-main_p975.RData")
noschool.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                     own_risk + duration + noschool_pregnant + noschool_baby + noschool_child + 
                     noschool_protection + noschool_ownrisk + noschool_duration + info_pregnant + 
                     info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                     p975_pregnant + p975_child + p975_baby + p975_protection + p975_ownrisk +
                     p975_duration + info_p975_pregnant + info_p975_child + 
                     info_p975_baby + info_p975_protection + info_p975_ownrisk + 
                     info_p975_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                     baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                     info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                     info_duration='n', p975_pregnant='n', p975_child='n', p975_baby='n', 
                     p975_protection='n', p975_ownrisk='n', p975_duration='n', 
                     info_p975_pregnant='n', info_p975_child='n', info_p975_baby='n',
                     info_p975_protection='n', info_p975_ownrisk='n', info_p975_duration='n'),
                     R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
noschool.aic<-AIC(noschool.mxl)

save.image("session-main_p975.RData")
hhmostlyme.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                       own_risk + duration + hh_mostly_me_pregnant + hh_mostly_me_baby + hh_mostly_me_child + 
                       hh_mostly_me_protection + hh_mostly_me_ownrisk + hh_mostly_me_duration + info_pregnant + 
                       info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                       p975_pregnant + p975_child + p975_baby + p975_protection + p975_ownrisk +
                       p975_duration + info_p975_pregnant + info_p975_child + 
                       info_p975_baby + info_p975_protection + info_p975_ownrisk + 
                       info_p975_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                       baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                       info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                       info_duration='n', p975_pregnant='n', p975_child='n', p975_baby='n', 
                       p975_protection='n', p975_ownrisk='n', p975_duration='n', 
                       info_p975_pregnant='n', info_p975_child='n', info_p975_baby='n',
                       info_p975_protection='n', info_p975_ownrisk='n', info_p975_duration='n'),
                       R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
hhmostlyme.aic<-AIC(hhmostlyme.mxl)

save.image("session-main_p975.RData")
monsoonmalaria.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                           own_risk + duration + monsoon_malaria_pregnant + monsoon_malaria_baby + monsoon_malaria_child + 
                           monsoon_malaria_protection + monsoon_malaria_ownrisk + monsoon_malaria_duration + info_pregnant + 
                           info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                           p975_pregnant + p975_child + p975_baby + p975_protection + p975_ownrisk +
                           p975_duration + info_p975_pregnant + info_p975_child + 
                           info_p975_baby + info_p975_protection + info_p975_ownrisk + 
                           info_p975_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                           baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                           info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                           info_duration='n', p975_pregnant='n', p975_child='n', p975_baby='n', 
                           p975_protection='n', p975_ownrisk='n', p975_duration='n', 
                           info_p975_pregnant='n', info_p975_child='n', info_p975_baby='n',
                           info_p975_protection='n', info_p975_ownrisk='n', info_p975_duration='n'),
                           R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
monsoonmalaria.aic<-AIC(monsoonmalaria.mxl)

save.image("session-main_p975.RData")
symptommal.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                       own_risk + duration + symptom_mal_pregnant + symptom_mal_baby + symptom_mal_child + 
                       symptom_mal_protection + symptom_mal_ownrisk + symptom_mal_duration + info_pregnant + 
                       info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                       p975_pregnant + p975_child + p975_baby + p975_protection + p975_ownrisk +
                       p975_duration + info_p975_pregnant + info_p975_child + 
                       info_p975_baby + info_p975_protection + info_p975_ownrisk + 
                       info_p975_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                       baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                       info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                       info_duration='n', p975_pregnant='n', p975_child='n', p975_baby='n', 
                       p975_protection='n', p975_ownrisk='n', p975_duration='n', 
                       info_p975_pregnant='n', info_p975_child='n', info_p975_baby='n',
                       info_p975_protection='n', info_p975_ownrisk='n', info_p975_duration='n'),
                       R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
symptommal.aic<-AIC(symptommal.mxl)

save.image("session-main_p975.RData")
lnhhincome.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                       own_risk + duration + ln_hh_income_pregnant + ln_hh_income_baby + ln_hh_income_child + 
                       ln_hh_income_protection + ln_hh_income_ownrisk + ln_hh_income_duration + info_pregnant + 
                       info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                       p975_pregnant + p975_child + p975_baby + p975_protection + p975_ownrisk +
                       p975_duration + info_p975_pregnant + info_p975_child + 
                       info_p975_baby + info_p975_protection + info_p975_ownrisk + 
                       info_p975_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                       baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                       info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                       info_duration='n', p975_pregnant='n', p975_child='n', p975_baby='n', 
                       p975_protection='n', p975_ownrisk='n', p975_duration='n', 
                       info_p975_pregnant='n', info_p975_child='n', info_p975_baby='n',
                       info_p975_protection='n', info_p975_ownrisk='n', info_p975_duration='n'),
                       R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
lnhhincome.aic<-AIC(lnhhincome.mxl)

save.image("session-main_p975.RData")
hadmalaria.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                       own_risk + duration + hadmalaria_pregnant + hadmalaria_baby + hadmalaria_child + 
                       hadmalaria_protection + hadmalaria_ownrisk + hadmalaria_duration + info_pregnant + 
                       info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                       p975_pregnant + p975_child + p975_baby + p975_protection + p975_ownrisk +
                       p975_duration + info_p975_pregnant + info_p975_child + 
                       info_p975_baby + info_p975_protection + info_p975_ownrisk + 
                       info_p975_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                       baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                       info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                       info_duration='n', p975_pregnant='n', p975_child='n', p975_baby='n', 
                       p975_protection='n', p975_ownrisk='n', p975_duration='n', 
                       info_p975_pregnant='n', info_p975_child='n', info_p975_baby='n',
                       info_p975_protection='n', info_p975_ownrisk='n', info_p975_duration='n'),
                       R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
hadmalaria.aic<-AIC(hadmalaria.mxl)

save.image("session-main_p975.RData")
mosqbreed.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                    own_risk + duration + mosq_breed_pregnant + mosq_breed_baby + mosq_breed_child + 
                    mosq_breed_protection + mosq_breed_ownrisk + mosq_breed_duration + info_pregnant + 
                    info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                    p975_pregnant + p975_child + p975_baby + p975_protection + p975_ownrisk +
                    p975_duration + info_p975_pregnant + info_p975_child + 
                    info_p975_baby + info_p975_protection + info_p975_ownrisk + 
                    info_p975_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                    baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                    info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                    info_duration='n', p975_pregnant='n', p975_child='n', p975_baby='n', 
                    p975_protection='n', p975_ownrisk='n', p975_duration='n', 
                    info_p975_pregnant='n', info_p975_child='n', info_p975_baby='n',
                    info_p975_protection='n', info_p975_ownrisk='n', info_p975_duration='n'),
                    R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
mosqbreed.aic<-AIC(mosqbreed.mxl)

save.image("session-main_p975.RData")
malrisk.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                    duration + mal_risk_pregnant + mal_risk_baby + mal_risk_child + 
                    mal_risk_protection + mal_risk_duration + info_pregnant + 
                    info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                    p975_pregnant + p975_child + p975_baby + p975_protection + p975_ownrisk +
                    p975_duration + info_p975_pregnant + info_p975_child + 
                    info_p975_baby + info_p975_protection + info_p975_ownrisk + 
                    info_p975_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                    baby='n', protection='n', duration='n', info_pregnant='n', 
                    info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                    info_duration='n', p975_pregnant='n', p975_child='n', p975_baby='n', 
                    p975_protection='n', p975_ownrisk='n', p975_duration='n', 
                    info_p975_pregnant='n', info_p975_child='n', info_p975_baby='n',
                    info_p975_protection='n', info_p975_ownrisk='n', info_p975_duration='n'),
                    R=draws, Halton=NA, print.level=0, panel=TRUE,ncores=2)
malrisk.aic<-AIC(malrisk.mxl)

save.image("session-main_p975.RData")
varprior.mxl<-mlogit(choice~price+asc+pregnant+baby + child + protection + 
                     own_risk + duration + var_prior_pregnant + var_prior_baby + var_prior_child + 
                     var_prior_protection + var_prior_ownrisk + var_prior_duration + info_pregnant + 
                     info_child + info_baby + info_protection + info_ownrisk + info_duration + 
                     p975_pregnant + p975_child + p975_baby + p975_protection + p975_ownrisk +
                     p975_duration + info_p975_pregnant + info_p975_child + 
                     info_p975_baby + info_p975_protection + info_p975_ownrisk + 
                     info_p975_duration|0,data, rpar=c(asc='n', pregnant ='n', child = 'n', 
                     baby='n', protection='n',own_risk='n', duration='n', info_pregnant='n', 
                     info_child='n', info_baby='n', info_protection='n', info_ownrisk='n', 
                     info_duration='n', p975_pregnant='n', p975_child='n', p975_baby='n', 
                     p975_protection='n', p975_ownrisk='n', p975_duration='n', 
                     info_p975_pregnant='n', info_p975_child='n', info_p975_baby='n',
                     info_p975_protection='n', info_p975_ownrisk='n', info_p975_duration='n'),
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

save.image("session-main_p975.RData")