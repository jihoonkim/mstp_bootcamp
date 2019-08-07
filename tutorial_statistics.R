### load libraries
library(data.table)
library(ggplot2)
library(Hmisc)
library(MASS)
library(ResourceSelection)
library(rmeta)
library(ROCR)




### Birth Weight data
data(birthwt)
bwt.df <- with(birthwt, {
  race <- factor(race, labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0)
  ftv <- factor(ftv)
  levels(ftv)[-(1:2)] <- "2+"
  data.frame(low = factor(low), bwt, age, lwt, race, smoke = (smoke > 0),
             ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})


### Exploratory data analysis
ggplot(bwt.df, aes(x=race, y=bwt, fill=race)) + geom_boxplot(width=0.4) 
ggplot(bwt.df, aes(x=smoke, y=bwt, fill=smoke))  + geom_boxplot(width=0.4) 
ggplot(bwt.df, aes(x=ht, y=bwt, fill=ht)) + 
  geom_boxplot(width=0.4) + xlab("History of hypertension")
ggplot(bwt.df, aes(x=ui, y=bwt, fill=ui) ) + 
  geom_boxplot(width=0.4) + xlab("Presence of uterine irritability")
ggplot(bwt.df, aes(x=ftv, y=bwt, fill=ftv)) +
   geom_boxplot(width=0.4) + xlab("Number of physician visits during the first trimester")
ggplot(bwt.df, aes(x=ptd, y=bwt, fill=ptd)) + 
  geom_boxplot(width=0.4)  + xlab("History of premature labors")



### Fit a logistic regression model
bwt <- with(birthwt, {
  race <- factor(race, labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0)
  ftv <- factor(ftv)
  levels(ftv)[-(1:2)] <- "2+"
  data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0),
             ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})
options(contrasts = c("contr.treatment", "contr.poly"))
m1 = glm(low ~ ., data = bwt, family = "binomial")
summary(m1)$coef




### model discrimination
mypred = prediction(m1$fitted.values, m1$y)
roc.perf = performance(mypred, measure = "tpr", x.measure = "fpr")
auc.perf = performance(mypred, measure = "auc")
plot(roc.perf, col = "orange", lwd = 2,
     main = paste ( "AUC = ", round(auc.perf@y.values[[1]], 2), sep="" )  )
abline(a = 0, b = 1, lty = 2)


### model calibration
myhl = hoslem.test(m1$y, m1$fitted.values / 2, g=10)
mycalib = data.frame(  
            predictionbin = factor(  c( rownames(myhl$observed), 
                                        rownames(myhl$observed)),
                                    levels = rownames(myhl$observed)),
            observedfreq = c(myhl$observed[,1], myhl$observed[,2]),
            birthweight = factor( c( rep("normal", 10), rep("low", 10) ),
                                  levels = c("normal", "low")) )
ggplot(data=mycalib, 
         aes(x=predictionbin, y=observedfreq, fill = birthweight)) +
      geom_bar(stat="identity", position=position_dodge()) +
      theme(axis.text.x=element_text(angle=30, hjust=1) ) +
     scale_fill_manual(values = c("royalblue", "orange")) +
     ylab("Observed frequency") +
     xlab("Prediction probability bin") +
     ggtitle( paste("pvalue.HL = ", round(myhl$p.value, 4), sep=""))


### survival analysis: Kaplan-Meier curve
m1.surv = survfit( Surv(time,status) ~ sex, data = lung)
print(m1.surv)
m1.diff = survdiff(Surv(time, status) ~ sex, data = lung)
m1.diff
ggsurvplot(m.surv, data = lung, conf.int = TRUE, pval = TRUE, 
           palette = c("orange", "royalblue"),
           legend.labs =  c("Male", "Female"), surv.median.line = "hv",
           risk.table = "abs_pct", risk.table.col = "strata", ggtheme = theme_bw() )

### Check the cumulative hazard functions for a categorical variable
m1.surv = survfit( Surv(time,status) ~ sex, data = lung)
ggsurvplot(m1.surv, data = lung, fun = function(x) -log(-log(x)), 
           palette = c("orange", "royalblue"),
           legend.labs =  c("Male", "Female"), ggtheme = theme_bw() ) +
      ylab("-log(- log(Survival))")


### Check the cumulative hazard functions for a continuous variable
m.coxPH <- coxph(Surv(time, status) ~ age*sex, data =  lung)
summary(m.coxPH)
check_PH <- cox.zph(m.coxPH, transform = "km")
check_PH
plot(check_PH, var = 1)
abline(h = coef(m.coxPH)[1], col = "red", lwd = 2)




sex_df <- with(lung,
               data.frame(sex = c(1, 2), 
                          age = rep(mean(age, na.rm = TRUE), 2),
                          ph.ecog = c(1, 1)
               )
)
m2.surv <- survfit(m.coxPH, newdata = sex_df)
ggsurvplot(m2.surv, data = sex_df, conf.int = TRUE, 
           palette = c("orange", "royalblue"), 
           legend.labs= c("Male", "Female"),  surv.median.line = "hv",
           ggtheme = theme_bw() )



res.cox <- coxph(Surv(time, status) ~ age + sex + wt.loss, data =  lung)
res.cox
test.ph <- cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
