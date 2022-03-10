#----Packages----#
library(survival)
library(survminer)
library(GGally)
library(lessR)
#----Data-----# 
Retention <- read.csv("http://peopleanalytics-regression-book.org/data/job_retention.csv")

Intention_category <- cut(Retention$intention, breaks = c(0,3,6,10), 
                     labels = c("Low", "Moderate", "High"))
head(Intention_category, 10)

Retention <- cbind(Retention[,c(1:7)], IG = Intention_category)

#----EDA----# 
setwd("D:/Survival_analysis")
pdf("EDA.pdf", paper = "USr", height =12, width = 12)
GGally::ggpairs(Retention[,1:7])
dev.off()
#---Null-model---# 
N_Mod <- survfit(Surv(event = left, time = month) ~ 1, 
                 data = Retention, 
                 type = "kaplan-meier")
print(N_Mod)
summary(N_Mod)
pdf("Null_model.pdf", height =12, width = 16, paper = "USr")
survminer::ggsurvplot(N_Mod, data = Retention, risk.table = T, conf.int = T, 
                      ggtheme = theme_minimal())
dev.off()
#---Gender---# 
G_Mod <- survfit(Surv(event = left, time = month) ~ gender, 
                 data = Retention, 
                 type = "kaplan-meier")
print(G_Mod)

pdf("Gender_predictor.pdf", height =12, width = 16, paper = "USr")
survminer::ggsurvplot(G_Mod, data = Retention, risk.table = T, conf.int = T, 
                      ggtheme = theme_minimal(), pval = T, pval.method = T)
dev.off()
G_Mod_Cox <- coxph(Surv(event = left, time = month) ~ gender, 
                   data = Retention)

G_mod_assumption <- cox.zph(G_Mod_Cox)
G_mod_assumption
survminer::ggcoxzph(G_mod_assumption,  
                    font.main = 10, 
                    font.x = 10, 
                    font.y = 10)
summary(G_Mod_Cox)

#---Survival-Analysis-For-Intention---# 
setwd("D:/Survival_analysis")
I_Mod <- survival::survfit(formula  = Surv(event = left, time = month) ~ IG, 
                          data = Retention)
summary(I_Mod)

pdf("Intention_predictor.pdf", height =12, width = 16, paper = "USr")
survminer::ggsurvplot(I_Mod, pval = T, conf.int = T, 
                               palette = c("blue", "red", "orange"), 
                               linetype = c("solid", "dashed", "dotted"), 
                               xlab = "Month", 
                               ylab = "Retention Rate")
dev.off()

I_Mod_Cox <- survival::coxph(Surv(event = left, time = month) ~ IG, data = Retention)

I_Mod_assumption <- cox.zph(I_Mod_Cox)
I_Mod_assumption
survminer::ggcoxzph(I_Mod_assumption, 
                    font.main = 10, 
                    font.x = 10, 
                    font.y = 10)
summary(I_Mod_Cox)

#-------Controlling-for-gender-field-and-level-------# 
Complete_model <- survival::coxph(formula = Surv(event = left, time = month) ~ gender + field + level + sentiment, data = Retention)

summary(Complete_model)

ph_check <- survival::cox.zph(Complete_model)
ph_check

pdf("coxph_assumption.pdf", height =12, width =16, paper = "USr")
survminer::ggcoxzph(ph_check,  
                    font.main = 10, 
                    font.x = 10, 
                    font.y = 10)
dev.off()
#-------------------------------------------------------------------------#

#-------Final-Model-Continuous-Predictors--------# 
Retention$Intention_center <- scale(Retention$intention, center = T, scale = F)
Retention$Sentiment_center <- scale(Retention$sentiment, center = T, scale = F)

Final_model <- coxph(Surv(event = left, time = month) ~ gender + Intention_center + 
                       Sentiment_center + field + level, data = Retention)

Final_model_assumption <- survival::cox.zph(Final_model)
Final_model_assumption

survminer::ggcoxzph(Final_model_assumption, 
                    font.main = 10, 
                    font.x = 10, 
                    font.y = 10)

summary(Final_model)


#---Model-Inference-(Note - Educational field is relative in field, and high level in organization)
#-Removing insignificant variables and only including partially significant and significant-# 
#-Imaginary-subject-# 
Int <- 4 - mean(Retention$intention) #(Range from_0-10)
Sent <- 6 - mean(Retention$sentiment)   #(Range from 0-10)
Finance <- 1 #(Works in Finance)
Health <- 0 #(Not working in health)
level <- 1 #(at low level in Organizational hierarchy)

Odds_risk = 1.22 * Int - 0.96 * Sent + 1.29 * Finance + 1.29 * Health + 1.18 * level 


#----------------------------------End---------------------------------------------------# 



