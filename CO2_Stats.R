#Analyses are to test significance of treatment (control DIW vs. Moderate Mn vs. High Mn) effect on soil CO2 flux
#Results: no significant Tx effect found

#GLM to investigate treatment effect on soil CO2 flux

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

tx <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)
time <- c(0, 0, 0, 0.5, 0.5, 0.5, 24, 24, 24, 72, 72, 72, 120, 120, 120,0, 0, 0, 0.5, 0.5, 0.5, 24, 24, 24, 72, 72, 72, 120, 120, 120,0, 0, 0, 0.5, 0.5, 0.5, 24, 24, 24, 72, 72, 72, 120, 120, 120)
co2 <- c(1.8321201030466,3.68992027842611,4.13182439433777,4.08861224513561,5.05899019016093,4.61420264632043,2.11084731065696,2.17025558424408,2.46877229786776,2.56297504897968,2.21381934440958,2.3439743981708,2.71887449738507,2.23259106897184,2.24095814062402,5.35139376828162,3.7613771769527,3.68109030488609,3.77306814943184,2.16486545797267,2.05992596702846,1.96168786051501,2.39764007054749,2.05774487986156,1.94952922478255,2.0369273622013,1.9733193109499,2.11661020127335,2.10039835976842,2.11742628918388,2.02215290754487,2.15539248296827,2.96932249796848,2.02552136328128,2.14567535295861,2.05616530885117,1.94233352322238,1.94524990164589,2.04539937025944,1.92569029277729,2.00360918661831,1.94334549025279,2.38844065286223,2.09595891524901,2.12776604259266)
tx.time <- c("c0","c0","c0","c0.5","c0.5","c0.5","c24","c24","c24","c72","c72","c72","c120","c120","c120","m0","m0","m0","m0.5","m0.5","m0.5","m24","m24","m24","m72","m72","m72","m120","m120","m120","h0","h0","h0","h0.5","h0.5","h0.5","h24","h24","h24","h72","h72","h72","h120","h120","h120")
df.co2 <- data.frame(tx, time, co2,tx.time)
print(df.co2)

co2glm <- glm(co2 ~ tx.time)
summary(co2glm)         

#ANOVA
aov.co2 <- aov(co2~tx.time, data=df.co2)
summary(aov.co2)

#Tukey's pot-hoc test (#Tukey's requires categories as factors)
aov.factor.co2 = aov(co2 ~ factor(tx.time), data = df.co2)
tukey.co2<-TukeyHSD(aov.factor.co2)
tukey.co2

tukey.cld.co2 <- multcompLetters4(aov.factor.co2, tukey.co2)
print(tukey.cld.co2)

#Time 0: no significant differences
co2.0 <- c(1.8321201030466,3.68992027842611,4.13182439433777,5.35139376828162,3.7613771769527,3.68109030488609,2.02215290754487,2.15539248296827,2.96932249796848)
tx.time.0 <- c("c0","c0","c0","m0","m0","m0","h0","h0","h0")
df.co2.0 <- data.frame(co2.0,tx.time.0)
print(df.co2.0)
cg0<-c(1.8321201030466,3.68992027842611,4.13182439433777)
mg0<-c(5.35139376828162,3.7613771769527,3.68109030488609)
hg0<-c(2.02215290754487,2.15539248296827,2.96932249796848)
shapiro.test(cg0)#normal
shapiro.test(mg0)#normal
shapiro.test(hg0)#normal

aov.co2.0 <- aov(co2.0~tx.time.0, data=df.co2.0)
summary(aov.co2.0)

aov.factor.0 = aov(co2.0 ~ factor(tx.time.0), data = df.co2.0)
tukey.0<-TukeyHSD(aov.factor.0)
tukey.0

#Time 0.5: C0.5 significantly differs from M0.5 and H0.5
co2.0.5 <- c(4.08861224513561,5.05899019016093,4.61420264632043,3.77306814943184,2.16486545797267,2.05992596702846,2.02552136328128,2.14567535295861,2.05616530885117)
tx.time.0.5 <- c("c0.5","c0.5","c0.5","m0.5","m0.5","m0.5","h0.5","h0.5","h0.5")
df.co2.0.5 <- data.frame(co2.0.5,tx.time.0.5)
print(df.co2.0.5)
cg0.5<-c(4.08861224513561,5.05899019016093,4.61420264632043)
mg0.5<-c(3.77306814943184,2.16486545797267,2.05992596702846)
hg0.5<-c(2.02552136328128,2.14567535295861,2.05616530885117)
shapiro.test(cg0.5)#normal
shapiro.test(mg0.5)#normal
shapiro.test(hg0.5)#normal

aov.co2.0.5 <- aov(co2.0.5~tx.time.0.5, data=df.co2.0.5)
summary(aov.co2.0.5)

aov.factor.0.5 = aov(co2.0.5 ~ factor(tx.time.0.5), data = df.co2.0.5)
tukey.0.5<-TukeyHSD(aov.factor.0.5)
tukey.0.5

#Time 24: no significant differences
tt.24 <- c("c24","c24","c24","m24","m24","m24","h24","h24","h24")
co2.24 <- c(2.11084731065696,2.17025558424408,2.46877229786776,1.96168786051501,2.39764007054749,2.05774487986156,1.94233352322238,1.94524990164589,2.04539937025944)
df.24 <- data.frame(co2.24,tt.24)
print(df.24)
cg24<-c(2.11084731065696,2.17025558424408,2.46877229786776)
mg24<-c(1.96168786051501,2.39764007054749,2.05774487986156)
hg24<-c(1.94233352322238,1.94524990164589,2.04539937025944)
shapiro.test(cg24)#normal
shapiro.test(mg24)#normal
shapiro.test(hg24)#not normal

aov.24 = aov(co2.24 ~ tt.24, data=df.24)
summary(aov.24)

aov.factor.24 = aov(co2.24 ~ factor(tt.24), data = df.24)
tukey.24<-TukeyHSD(aov.factor.24)
tukey.24

kruskal.test(co2.24 ~ tt.24, data=df.24)#no significant differences
dunnTest(co2.24~factor(tt.24), data=df.24, method="bonferroni")


#Time 72: C72 differs from M72 and H72
tt.72 <- c("c72","c72","c72","m72","m72","m72","h72","h72","h72")
co2.72 <- c(2.56297504897968,2.21381934440958,2.3439743981708,1.94952922478255,2.0369273622013,1.9733193109499,1.92569029277729,2.00360918661831,1.94334549025279)
df.72 <- data.frame(co2.72,tt.72)
print(df.72)
cg72<-c(2.56297504897968,2.21381934440958,2.3439743981708)
mg72<-c(1.94952922478255,2.0369273622013,1.9733193109499)
hg72<-c(1.92569029277729,2.00360918661831,1.94334549025279)
shapiro.test(cg72)#normal
shapiro.test(mg72)#normal
shapiro.test(hg72)#normal

aov.72 = aov(co2.72 ~ tt.72, data=df.72)
summary(aov.72)

aov.factor.72 = aov(co2.72 ~ factor(tt.72), data = df.72)
tukey.72<-TukeyHSD(aov.factor.72)
tukey.72


#Time 120: no significant differences
tt.120 <- c("c120","c120","c120","m120","m120","m120","h120","h120","h120")
co2.120 <- c(2.71887449738507,2.23259106897184,2.24095814062402,2.11661020127335,2.10039835976842,2.11742628918388,2.38844065286223,2.09595891524901,2.12776604259266)
df.120 <- data.frame(co2.120,tt.120)
print(df.120)
cg120<-c(2.71887449738507,2.23259106897184,2.24095814062402)
mg120<-c(2.11661020127335,2.10039835976842,2.11742628918388)
hg120<-c(2.38844065286223,2.09595891524901,2.12776604259266)
shapiro.test(cg120)#not normal
shapiro.test(mg120)#normal
shapiro.test(hg120)#normal

kruskal.test(co2.120 ~ tt.120, data=df.120)#no signficant differences

aov.120 = aov(co2.120 ~ tt.120, data=df.120)
summary(aov.120)

aov.factor.120 = aov(co2.120 ~ factor(tt.120), data = df.120)
tukey.120<-TukeyHSD(aov.factor.120)
tukey.120

########################################################################################################

#control: no signficant diffs
co2.c<-c(cg0,cg0.5,cg24,cg72,cg120)
tx.time.co2.c<-c(rep("c0",3),rep("c0.5",3),rep("c24",3),rep("c72",3),rep("c120",3))
df.co2.c <-data.frame(co2.c,tx.time.co2.c)
df.co2.c
shapiro.test(cg0)
shapiro.test(cg0.5)
shapiro.test(cg24)
shapiro.test(cg72)
shapiro.test(cg120)#not normal

kruskal.test(co2.c ~ factor(tx.time.co2.c), data = df.co2.c)#no significant differences
dunnTest(co2.c ~ factor(tx.time.co2.c), data = df.co2.c, method="bonferroni")

#moderate Mn
co2.m<-c(mg0,mg0.5,cg24,mg72,mg120)
tx.time.co2.m<-c(rep("m0",3),rep("m0.5",3),rep("m24",3),rep("m72",3),rep("m120",3))
df.co2.m <-data.frame(co2.m,tx.time.co2.m)
df.co2.m
shapiro.test(mg0)
shapiro.test(mg0.5)
shapiro.test(mg24)
shapiro.test(mg72)
shapiro.test(mg120)

aov.co2.m <- aov(co2.m ~ tx.time.co2.m, data=df.co2.m)
summary(aov.co2.m)

aov.factor.co2.m = aov(co2.m ~ factor(tx.time.co2.m), data = df.co2.m)
tukey.co2.m<-TukeyHSD(aov.factor.co2.m)
tukey.co2.m

tukey.cld.co2.m <- multcompLetters4(aov.factor.co2.m, tukey.co2.m)
print(tukey.cld.co2.m)

#high Mn
co2.h<-c(hg0,hg0.5,cg24,hg72,hg120)
tx.time.co2.h<-c(rep("h0",3),rep("h0.5",3),rep("h24",3),rep("h72",3),rep("h120",3))
df.co2.h <-data.frame(co2.h,tx.time.co2.h)
df.co2.h
shapiro.test(hg0)
shapiro.test(hg0.5)
shapiro.test(hg24) #not normal
shapiro.test(hg72)
shapiro.test(hg120)

kruskal.test(co2.h ~ factor(tx.time.co2.h), data = df.co2.h)#no significant differences
dunnTest(co2.h ~ factor(tx.time.co2.h), data = df.co2.h, method="bonferroni")

