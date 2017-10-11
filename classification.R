require(MASS)
require(ggplot2)
require(ISLR)
require(dplyr)
require(data.world)

project <- "https://data.world/marcusgabe-ut/parkinsons-data"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons"),
  dataset = project
)
summary(df)
names(df)
# get only continuous variables
sdf = dplyr::select(df, - status, - name)
pairs(sdf)
s = sample(nrow(df), 40)
lda.fit=lda(status~mdvp_fhi_hz + mdvp_jitter,data=df, subset=s)
lda.fit
plot(lda.fit)
train=df[-s,]
lda.pred=predict(lda.fit,train)
pred_df = data.frame(lda.pred)
ggplot(pred_df) + geom_histogram(mapping = aes(x=LD1)) + facet_wrap(~ class)
ggplot(pred_df) + geom_boxplot(mapping = aes(x=class, y=LD1))
table(lda.pred$class,train$status)
mean(lda.pred$class==train$status)
