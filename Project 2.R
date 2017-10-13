library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)
require(MASS)
require(ISLR)
require(ggplot2)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OnRhcnJhbnRybCIsImlzcyI6ImFnZW50OnRhcnJhbnRybDo6MDE1OTQxYzQtNTUyZC00YjI3LWIxNGEtYzllN2ExMjYxN2FiIiwiaWF0IjoxNTA1MzEzMjAyLCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.vWrAbNkyU0mhgsdXXL-bxESWzppmpm8wguw9uI7pJ64ZsDtovi8kbWbPYS5pPcX8DDnVMuYxJJWHhqdxv--R_w"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/marcusgabe-ut/parkinsons-data"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons"),
  dataset = project
)

summary(df)
attach(df)
# add column with binary version of status
df = df %>% dplyr::mutate(status2 = ifelse(status == "true", 1, 0))

##                                  ##
## Create training and testing data ##
##                                  ##

train = sample(nrow(df), 97)
#View(train)
test = df[-train,]
#View(test)

# KNN requires the testing and training sets to be the same length
# since there are an odd number of rows, choose 97 from the 98 testing set
test_knn = sample(nrow(test), 97)


##                    ##
## Correlation charts ##
##                    ##

# predictors about vocal fundamental frequency
fund_freq_df = dplyr::select(df, mdvp_fo_hz, mdvp_flo_hz, mdvp_fhi_hz)
# predictors about variation in vocal fundamental frequency
freq_var_df = dplyr::select(df, mdvp_jitter, mdvp_jitter_abs,mdvp_rap,mdvp_ppq,jitter_ddp)
# predictors about variation in amplitude 
amp_var_df = dplyr::select(df, mdvp_shimmer, mdvp_shimmer_db, shimmer_apq3, shimmer_apq5, mdvp_apq, shimmer_dda)
# predictors for other vocal aspects
other_preds_df = dplyr::select(df, nhr, hnr, rpde, d2, dfa, spread1, spread2, ppe)

# fundamental frequency variables are all correlated
pairs(fund_freq_df)
# variation in fundamental frequency variables (jitters) are all correlated
pairs(freq_var_df)
# variation in amplitude variables (shimmers) are all correlated
pairs(amp_var_df)
# other variables mostly not correlated except for spread1&ppe and spread1&spread2
pairs(other_preds_df)

# predictors with one fund freq variable, one jitter, one shimmer, and the uncorrelated other variables
try_preds_df = dplyr::select(df, mdvp_fo_hz, mdvp_jitter, mdvp_shimmer, nhr, rpde, d2, dfa, spread1)
# mdvp_jitter&mdvp_shimmer and jitter&nhr are correlated
pairs (try_preds_df)

# remove nhr, and make two sets of uncorrelated predictors - one with jitter, one with shimmer
uncor_preds1_df = dplyr::select(df, mdvp_fo_hz, mdvp_jitter, rpde, d2, dfa, spread1)
pairs(uncor_preds1_df)
uncor_preds2_df = dplyr::select(df, mdvp_fo_hz, mdvp_shimmer, rpde, d2, dfa, spread1)
pairs(uncor_preds2_df)


##    ##
## LR ##
##    ##

#
# GLM1 - analyzing average hertz and jitter
#
glm1.fit=glm(status2 ~ mdvp_fo_hz + mdvp_jitter,
             data=df, family=binomial,
             subset=train)
summary(glm1.fit)
glm1.probs=predict(glm1.fit,newdata=test,type="response")
#glm1.probs[1:5]
glm1.pred=ifelse(glm1.probs>0.5,"1","0")
status2.test = test$status2
table(glm1.pred,status2.test)
mean(glm1.pred==status2.test)

#
# GLM2 - analyzing average hertz and shimmer
#
glm2.fit=glm(status2 ~ mdvp_fo_hz + mdvp_shimmer,
             data=df, family=binomial,
             subset=train)
summary(glm2.fit)
glm2.probs=predict(glm2.fit,newdata=test,type="response")
#glm2.probs[1:5]
glm2.pred=ifelse(glm2.probs>0.5,"1","0")
status2.test = test$status2
table(glm2.pred,status2.test)
mean(glm2.pred==status2.test)

#
# GLM3 - analyzing five uncorrelated predictors with jitter
#
glm3.fit=glm(status2 ~ mdvp_fo_hz + mdvp_jitter + rpde + d2 + dfa + spread1,
             data=df, family=binomial,
             subset=train)
summary(glm3.fit)
glm3.probs=predict(glm3.fit,newdata=test,type="response")
#glm.probs[1:5]
glm3.pred=ifelse(glm3.probs>0.5,"1","0")
status2.test = test$status2
table(glm3.pred,status2.test)
mean(glm3.pred==status2.test)

#
# GLM4 - analyzing five uncorrelated predictors with shimmer
#
glm4.fit=glm(status2 ~ mdvp_fo_hz + mdvp_shimmer + rpde + d2 + dfa + spread1,
             data=df, family=binomial,
             subset=train)
summary(glm4.fit)
glm4.probs=predict(glm4.fit,newdata=test,type="response")
#glm4.probs[1:5]
glm4.pred=ifelse(glm4.probs>0.5,"1","0")
status2.test = test$status2
table(glm4.pred,status2.test)
mean(glm4.pred==status2.test)

# mean of best model
glm_mean = mean(glm1.pred==status2.test)


##      ##
## LDA  ##
##      ##

#
# LDA1 - analyzing average hertz and jitter
#
lda1.fit=lda(status ~ mdvp_fo_hz + mdvp_jitter,
             data=df, subset=train)
lda1.fit
plot(lda1.fit)

lda1.pred=predict(lda1.fit, test)
lda1_df = data.frame(lda1.pred)
ggplot(lda1_df) + geom_histogram(mapping=aes(x=LD1))
ggplot(lda1_df) + geom_boxplot(mapping = aes(x=class,y=LD1))
table(lda1.pred$class,test$status)
table(lda1.pred$class==test$status)
table(lda1.pred$class!=test$status)
mean(lda1.pred$class==test$status)

#
# LDA2 - analyzing average hertz and shimmer
#
lda2.fit=lda(status ~ mdvp_fo_hz + mdvp_shimmer,
             data=df, subset=train)
lda2.fit
plot(lda2.fit)

lda2.pred=predict(lda2.fit, test)
lda2_df = data.frame(lda2.pred)
ggplot(lda2_df) + geom_histogram(mapping=aes(x=LD1))
ggplot(lda2_df) + geom_boxplot(mapping = aes(x=class,y=LD1))
table(lda2.pred$class,test$status)
table(lda2.pred$class==test$status) #no real falses because everyone with jitters has PD ->jitters is "perfect" predictor of PD
table(lda2.pred$class!=test$status)
mean(lda2.pred$class==test$status)

#
# LDA3 - analyzing five uncorrelated predictors with jitter
#
lda3.fit=lda(status ~ mdvp_fo_hz + mdvp_jitter + rpde + d2 + dfa + spread1,
             data=df, subset=train)
lda3.fit
plot(lda3.fit)

lda3.pred=predict(lda3.fit, test)
lda3_df = data.frame(lda3.pred)
ggplot(lda3_df) + geom_histogram(mapping=aes(x=LD1))
ggplot(lda3_df) + geom_boxplot(mapping = aes(x=class,y=LD1))
table(lda3.pred$class,test$status)
table(lda3.pred$class==test$status)
table(lda3.pred$class!=test$status)
mean(lda3.pred$class==test$status)

#
# LDA4 - analyzing five uncorrelated predictors with shimmer
#
lda4.fit=lda(status ~ mdvp_fo_hz + mdvp_shimmer + rpde + d2 + dfa + spread1,
             data=df, subset=train)
lda4.fit
plot(lda4.fit)

lda4.pred=predict(lda4.fit, test)
lda4_df = data.frame(lda4.pred)
ggplot(lda4_df) + geom_histogram(mapping=aes(x=LD1))
ggplot(lda4_df) + geom_boxplot(mapping = aes(x=class,y=LD1))
table(lda4.pred$class,test$status)
table(lda4.pred$class==test$status)
table(lda4.pred$class!=test$status)
mean(lda4.pred$class==test$status)

# mean of best model
lda_mean = mean(lda1.pred$class==test$status)


##      ##
## QDA  ##
##      ##

#
# QDA1 - analyzing average hertz and jitter
#
qda1.fit = qda(status ~ mdvp_fo_hz + mdvp_jitter,
              data=df, subset=train)
qda1.fit
qda1.pred = predict(qda1.fit, test)
table(qda1.pred$class,test$status)
table(qda1.pred$class==test$status)
table(qda1.pred$class!=test$status)
mean(qda1.pred$class==test$status)

#
# QDA2 - analyzing average hertz and shimmer
#
qda.fit2 = qda(status ~ mdvp_fo_hz + mdvp_shimmer,
               data=df, subset=train)
qda.fit2
qda.pred2 = predict(qda.fit2, test)
table(qda.pred2$class,test$status)
table(qda.pred2$class==test$status)
table(qda.pred2$class!=test$status)
mean(qda.pred2$class==test$status)

#
# QDA3 - analyzing 5 uncorrelated predictors with jitter
#
qda.fit3 = qda(status ~ mdvp_fo_hz + mdvp_jitter + rpde + d2 + dfa + spread1,
               data=df, subset=train)
qda.fit3
qda.pred3 = predict(qda.fit3, test)
table(qda.pred3$class,test$status)
table(qda.pred3$class==test$status)
table(qda.pred3$class!=test$status)
mean(qda.pred3$class==test$status)

#
# QDA4 - analyzing 5 uncorrelated predictors with shimmer
#
qda.fit4 = qda(status ~ mdvp_fo_hz + mdvp_shimmer + rpde + d2 + dfa + spread1,
               data=df,subset=train)
qda.fit4
qda.pred4 = predict(qda.fit4, test)
table(qda.pred4$class,test$status)
table(qda.pred4$class==test$status)
table(qda.pred4$class!=test$status)
mean(qda.pred4$class==test$status)

# mean of best model
qda_mean = mean(qda1.pred$class==test$status)


##      ##
## KNN  ##
##      ##

#
# KNN1 - analyzing average hertz and jitter
#
predictors1=cbind(mdvp_fo_hz, mdvp_jitter)
knn1.pred=class::knn(predictors1[train, ],predictors1[test_knn,],status[train],k=1)
table(knn1.pred,status[test_knn])
mean(knn1.pred==status[test_knn])

#
# KNN2 - analyzing average hertz and shimmer
#
predictors2=cbind(mdvp_fo_hz, mdvp_shimmer)
knn2.pred=class::knn(predictors2[train, ],predictors2[test_knn,],status[train],k=1)
table(knn2.pred,status[test_knn])
mean(knn2.pred==status[test_knn])

#
# KNN3- analyzing five uncorrelated predictors with jitter
#
predictors3=cbind(mdvp_fo_hz, mdvp_jitter, rpde, d2, dfa, spread1)
knn3.pred=class::knn(predictors3[train, ],predictors3[test_knn,],status[train],k=1)
table(knn3.pred,status[test_knn])
mean(knn3.pred==status[test_knn])

#
# KNN4 - analyzing five uncorrelated predictors with shimmer
#
predictors4=cbind(mdvp_fo_hz, mdvp_shimmer, rpde, d2, dfa, spread1)
knn4.pred=class::knn(predictors4[train, ],predictors4[test_knn,],status[train],k=1)
table(knn4.pred,status[test_knn])
mean(knn4.pred==status[test_knn])

# mean of best model
knn_mean = mean(knn1.pred==status[test_knn])


##                                            ##
## Comparison of the mean correct predictions ##
##                                            ##

glm_mean
lda_mean
qda_mean
knn_mean
