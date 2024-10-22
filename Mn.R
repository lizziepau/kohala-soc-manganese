library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library("colorspace")
library(tidyselect)
library(multcomp)
library(multcompView)
library(dplyr)
library(readr)
library(FSA)

c_diw<-c(0.0487743617084296,0.0377528608742081,0.00274707356842131,
         0.35786101213754,0.0554658153764299,0.00509361888659277,
         0.380086734693878,0.0450173051676811,0.0160084880636605,
         0.408825112107623,0.0611717050981955,0.0355629007329875,
         0.979958336697618,0.0273459840548198,0.0148842257597685)
C0_diw<-c(	0.0487743617084296,0.0377528608742081,0.00274707356842131)
C0.5_diw<-c(	0.35786101213754,0.0554658153764299,0.00509361888659277)
C24_diw<-c(	0.380086734693878,0.0450173051676811,0.0160084880636605)
C72_diw	<-c(0.408825112107623,0.0611717050981955,0.0355629007329875)
C120_diw<-c(	0.979958336697618,0.0273459840548198,0.0148842257597685)

m_diw<-c(23.873454260812,11.2356269824487,12.1000762961096,
         58.7592660121552,41.4738991453731,22.6433784593267,
         29.2349363168521,25.0731950486236,24.1844525617347,
         25.4313260787251,22.5129143423826,18.5072933625286,
         15.805320778111,20.1851933922244,20.8104728474086)
M0_diw	<-c(23.873454260812,11.2356269824487,12.1000762961096)
M0.5_diw	<-c(58.7592660121552,41.4738991453731,22.6433784593267)
M24_diw	<-c(29.2349363168521,25.0731950486236,24.1844525617347)
M72_diw	<-c(25.4313260787251,22.5129143423826,18.5072933625286)
M120_diw<-c(	15.805320778111,20.1851933922244,20.8104728474086)

h_diw<-c(206.603926589341,197.449967003043,143.861157436217,
         224.425106847984,228.424511823929,245.521566197354,
         195.94956176288,220.081356308718,206.167772064606,
         157.12253621801,180.450790834258,160.886679593316,
         166.907565415431,138.87149930667,173.543362996723)
H0_diw	<-c(206.603926589341,197.449967003043,143.861157436217)
H0.5_diw<-c(	224.425106847984,228.424511823929,245.521566197354)
H24_diw<-c(	195.94956176288,220.081356308718,206.167772064606)
H72_diw	<-c(157.12253621801,180.450790834258,160.886679593316)
H120_diw	<-c(166.907565415431,138.87149930667,173.543362996723)

mn.0_diw <-c(0.0487743617084296,0.0377528608742081,0.00274707356842131,
             23.873454260812,11.2356269824487,12.1000762961096,
             206.603926589341,197.449967003043,143.861157436217)
mn.0.5_diw<-c(0.35786101213754,0.0554658153764299,0.00509361888659277,
              58.7592660121552,41.4738991453731,22.6433784593267,
              224.425106847984,228.424511823929,245.521566197354)
mn.24_diw<-c(0.380086734693878,0.0450173051676811,0.0160084880636605,
            29.2349363168521,25.0731950486236,24.1844525617347,
            195.94956176288,220.081356308718,206.167772064606)
mn.72_diw<-c(0.408825112107623,0.0611717050981955,0.0355629007329875,
             25.4313260787251,22.5129143423826,18.5072933625286,
             157.12253621801,180.450790834258,160.886679593316)
mn.120_diw<-c(0.979958336697618,0.0273459840548198,0.0148842257597685,
              15.805320778111,20.1851933922244,20.8104728474086,
              166.907565415431,138.87149930667,173.543362996723)

c_napp<-c(15.8465998785671,14.1294249088557,12.2605809128631,
          15.9516636340005,9.36634821046628,9.71989360803851,
          14.2200177497288,15.7442280945758,14.0052848318463,
          14.3192825112108,20.0904061530158,19.8828932261768,
          8.67740963855422,8.79342679843714,10.3823942701228)
C0_napp<-c(15.8465998785671,14.1294249088557,12.2605809128631)
C0.5_napp<-c(15.9516636340005,9.36634821046628,9.71989360803851)
C24_napp<-c(14.2200177497288,15.7442280945758,14.0052848318463)
C72_napp<-c(14.3192825112108,20.0904061530158,19.8828932261768)
C120_napp	<-c(8.67740963855422,8.79342679843714,10.3823942701228)

m_napp<-c(51.1471439628483,61.9224019658032,122.522069812673,
          141.62217032967,81.2735880697131,99.5800364431487,
          93.1727803099362,95.2966579891649,113.942444414858,
          111.583314711359,87.4109004739336,110.428935453301,
          81.9429066314748,97.4825015893198,99.7609367945824)
M0_napp	<-c(51.1471439628483,61.9224019658032,122.522069812673)
M0.5_napp	<-c(141.62217032967,81.2735880697131,99.5800364431487)
M24_napp<-c(93.1727803099362,95.2966579891649,113.942444414858)
M72_napp<-c(111.583314711359,87.4109004739336,110.428935453301)
M120_napp	<-c(81.9429066314748,97.4825015893198,99.7609367945824)

h_napp<-c(144.580592833876,119.243982024228,145.65647135986,
          133.72614418793,142.377194024784,272.36330523302,
          155.088622205936,179.505334051724,195.052595603178,
          318.812383461694,249.520505460655,251.909350877193,
          313.800611387084,225.554153334187,187.585732195093)
H0_napp<-c(	144.580592833876,119.243982024228,145.65647135986)
H0.5_napp<-c(	133.72614418793,142.377194024784,272.36330523302)
H24_napp<-c(	155.088622205936,179.505334051724,195.052595603178)
H72_napp<-c(	318.812383461694,249.520505460655,251.909350877193)
H120_napp<-c(	313.800611387084,225.554153334187,187.585732195093)

mn.0_napp<-c(15.8465998785671,14.1294249088557,12.2605809128631,
             51.1471439628483,61.9224019658032,122.522069812673,
             144.580592833876,119.243982024228,145.65647135986)
mn.0.5_napp<-c(15.9516636340005,9.36634821046628,9.71989360803851,
               141.62217032967,81.2735880697131,99.5800364431487,
               133.72614418793,142.377194024784,272.36330523302)
mn.24_napp<-c(14.2200177497288,15.7442280945758,14.0052848318463,
             93.1727803099362,95.2966579891649,113.942444414858,
             155.088622205936,179.505334051724,195.052595603178)
mn.72_napp<-c(14.3192825112108,20.0904061530158,19.8828932261768,
              111.583314711359,87.4109004739336,110.428935453301,
              318.812383461694,249.520505460655,251.909350877193)
mn.120_napp<-c(8.67740963855422,8.79342679843714,10.3823942701228,
               81.9429066314748,97.4825015893198,99.7609367945824,
               313.800611387084,225.554153334187,187.585732195093)



#Time 0_diw: H0_diw significantly greater than C0_diw and M0_diw
mn.0_diw
time.0 <- c(rep("c0",3), rep("m0",3), rep("h0",3))
df.diw.0 <- data.frame(mn.0_diw,time.0)
print(df.diw.0)
shapiro.test(C0_diw)#normal
shapiro.test(M0_diw)#normal
shapiro.test(H0_diw)#normal

kruskal.test(mn.0_diw~time.0, data=df.diw.0)

aov.diw.0 <- aov(mn.0_diw~time.0, data=df.diw.0)
summary(aov.diw.0)

aov.factor.diw.0 = aov(mn.0_diw ~ factor(time.0), data = df.diw.0)
tukey.0.diw<-TukeyHSD(aov.factor.diw.0)
tukey.0.diw

#Time 0.5_diw: all differ
mn.0.5_diw
time.0.5 <- c(rep("c0.5",3), rep("m0.5",3), rep("h0.5",3))
df.diw.0.5 <- data.frame(mn.0.5_diw,time.0.5)
print(df.diw.0.5)
shapiro.test(C0.5_diw)#normal
shapiro.test(M0.5_diw)#normal
shapiro.test(H0.5_diw)#normal

kruskal.test(mn.0.5_diw~time.0.5, data=df.diw.0.5)

aov.diw.0.5 <- aov(mn.0.5_diw~time.0.5, data=df.diw.0.5)
summary(aov.diw.0.5)

aov.factor.diw.0.5 = aov(mn.0.5_diw ~ factor(time.0.5), data = df.diw.0.5)
tukey.0.5.diw<-TukeyHSD(aov.factor.diw.0.5)
tukey.0.5.diw

#Time 24_diw: all significantly differ
mn.24_diw
time.24 <- c(rep("c24",3), rep("m24",3), rep("h24",3))
df.diw.24 <- data.frame(mn.24_diw,time.24)
print(df.diw.24)
shapiro.test(C24_diw)#normal
shapiro.test(M24_diw)#normal
shapiro.test(H24_diw)#normal

kruskal.test(mn.24_diw~time.24, data=df.diw.24)

aov.diw.24 <- aov(mn.24_diw~time.24, data=df.diw.24)
summary(aov.diw.24)

aov.factor.diw.24 = aov(mn.24_diw ~ factor(time.24), data = df.diw.24)
tukey.24.diw<-TukeyHSD(aov.factor.diw.24)

tukey.24.diw

#Time 72_diw: all significantly differ
mn.72_diw
time.72 <- c(rep("c72",3), rep("m72",3), rep("h72",3))
df.diw.72 <- data.frame(mn.72_diw,time.72)
print(df.diw.72)
shapiro.test(C72_diw)#normal
shapiro.test(M72_diw)#normal
shapiro.test(H72_diw)#normal

kruskal.test(mn.72_diw~time.72, data=df.diw.72)

aov.diw.72 <- aov(mn.72_diw~time.72, data=df.diw.72)
summary(aov.diw.72)

aov.factor.diw.72 = aov(mn.72_diw ~ factor(time.72), data = df.diw.72)
tukey.72.diw<-TukeyHSD(aov.factor.diw.72)

tukey.72.diw

#Time 120_diw: Kruskal-Wallis with Bonferroni adjustment: only c120 - h120 differ
mn.120_diw
time.120 <- c(rep("c120",3), rep("m120",3), rep("h120",3))
df.diw.120 <- data.frame(mn.120_diw,time.120)
print(df.diw.120)
shapiro.test(C120_diw)#normal
shapiro.test(M120_diw)#normal
shapiro.test(H120_diw)#normal

kruskal.test(mn.120_diw~time.120, data=df.diw.120)
dunnTest(mn.120_diw ~ factor(time.120), data=df.diw.120, method="bonferroni")

aov.diw.120 <- aov(mn.120_diw~time.120, data=df.diw.120)
summary(aov.diw.120)

aov.factor.diw.120 = aov(mn.120_diw ~ factor(time.120), data = df.diw.120)
tukey.120.diw<-TukeyHSD(aov.factor.diw.120)

tukey.120.diw

##################################################################################################################################

#Time 0_napp: C0_napp significantly differs from M0 and H0
mn.0_napp
time.0 <- c(rep("c0",3), rep("m0",3), rep("h0",3))
df.napp.0 <- data.frame(mn.0_napp,time.0)
print(df.napp.0)
shapiro.test(C0_napp)#normal
shapiro.test(M0_napp)#normal
shapiro.test(H0_napp)#normal

kruskal.test(mn.0_napp~time.0, data=df.napp.0)

aov.napp.0 <- aov(mn.0_napp~time.0, data=df.napp.0)
summary(aov.napp.0)

aov.factor.napp.0 = aov(mn.0_napp ~ factor(time.0), data = df.napp.0)
tukey.0.napp<-TukeyHSD(aov.factor.napp.0)
tukey.0.napp

#Time 0.5_napp: only h0.5-c0.5 differ
mn.0.5_napp
time.0.5 <- c(rep("c0.5",3), rep("m0.5",3), rep("h0.5",3))
df.napp.0.5 <- data.frame(mn.0.5_napp,time.0.5)
print(df.napp.0.5)
shapiro.test(C0.5_napp)#normal
shapiro.test(M0.5_napp)#normal
shapiro.test(H0.5_napp)#normal

kruskal.test(mn.0.5_napp~time.0.5, data=df.napp.0.5)

aov.napp.0.5 <- aov(mn.0.5_napp~time.0.5, data=df.napp.0.5)
summary(aov.napp.0.5)

aov.factor.napp.0.5 = aov(mn.0.5_napp ~ factor(time.0.5), data = df.napp.0.5)
tukey.0.5.napp<-TukeyHSD(aov.factor.napp.0.5)
tukey.0.5.napp

#Time 24_napp: all significantly differ
mn.24_napp
time.24 <- c(rep("c24",3), rep("m24",3), rep("h24",3))
df.napp.24 <- data.frame(mn.24_napp,time.24)
print(df.napp.24)
shapiro.test(C24_napp)#normal
shapiro.test(M24_napp)#normal
shapiro.test(H24_napp)#normal

kruskal.test(mn.24_napp~time.24, data=df.napp.24)

aov.napp.24 <- aov(mn.24_napp~time.24, data=df.napp.24)
summary(aov.napp.24)

aov.factor.napp.24 = aov(mn.24_napp ~ factor(time.24), data = df.napp.24)
tukey.24.napp<-TukeyHSD(aov.factor.napp.24)

tukey.24.napp

#Time 72_napp: all significantly differ
mn.72_napp
time.72 <- c(rep("c72",3), rep("m72",3), rep("h72",3))
df.napp.72 <- data.frame(mn.72_napp,time.72)
print(df.napp.72)
shapiro.test(C72_napp)#normal
shapiro.test(M72_napp)#normal
shapiro.test(H72_napp)#normal

kruskal.test(mn.72_napp~time.72, data=df.napp.72)

aov.napp.72 <- aov(mn.72_napp~time.72, data=df.napp.72)
summary(aov.napp.72)

aov.factor.napp.72 = aov(mn.72_napp ~ factor(time.72), data = df.napp.72)
tukey.72.napp<-TukeyHSD(aov.factor.napp.72)

tukey.72.napp

#Time 120_napp: H120 differs from C120 and M120
mn.120_napp
time.120 <- c(rep("c120",3), rep("m120",3), rep("h120",3))
df.napp.120 <- data.frame(mn.120_napp,time.120)
print(df.napp.120)
shapiro.test(C120_napp)#normal
shapiro.test(M120_napp)#normal
shapiro.test(H120_napp)#normal

kruskal.test(mn.120_napp~time.120, data=df.napp.120)
dunnTest(mn.120_napp ~ factor(time.120), data=df.napp.120, method="bonferroni")

aov.napp.120 <- aov(mn.120_napp~time.120, data=df.napp.120)
summary(aov.napp.120)

aov.factor.napp.120 = aov(mn.120_napp ~ factor(time.120), data = df.napp.120)
tukey.120.napp<-TukeyHSD(aov.factor.napp.120)

tukey.120.napp

##################################################################################################################################

#Control diw: no significant differences
c_diw
time.c_diw <- c(rep("0h",3), rep("0.5h",3), rep("24h",3), rep("72h",3), rep("120h",3))
df.c_diw <- data.frame(c_diw,time.c_diw)
print(df.c_diw)
shapiro.test(C0_diw)#normal
shapiro.test(C0.5_diw)#normal
shapiro.test(C24_diw)#normal
shapiro.test(C72_diw)#normal
shapiro.test(C120_diw)#not normal

kruskal.test(c_diw~time.c_diw, data=df.c_diw)
dunnTest(c_diw ~ factor(time.c_diw), data=df.c_diw, method="bonferroni")

#Moderate diw: 0h-0.5h only difference
m_diw
time.m_diw <- c(rep("0h",3), rep("0.5h",3), rep("24h",3), rep("72h",3), rep("120h",3))
df.m_diw <- data.frame(m_diw,time.m_diw)
print(df.m_diw)
shapiro.test(M0_diw)#normal
shapiro.test(M0.5_diw)#normal
shapiro.test(M24_diw)#normal
shapiro.test(M72_diw)#normal
shapiro.test(M120_diw)#normal

aov.m_diw <- aov(m_diw~time.m_diw, data=df.m_diw)
summary(aov.m_diw)

aov.factor.m_diw = aov(m_diw ~ factor(time.m_diw), data=df.m_diw)
tukey.m_diw<-TukeyHSD(aov.factor.m_diw)
tukey.m_diw
cld.mn.m_diw <- multcompLetters4(aov.factor.m_diw, tukey.m_diw)
print(cld.mn.m_diw)
#$`factor(time.m_diw)`
#0.5h  24h  72h 120h   0h 
#"a" "ab" "ab" "ab"  "b" 


#High diw: 120h-0.5h, 72h-0.5h
h_diw
time.h_diw <- c(rep("0h",3), rep("0.5h",3), rep("24h",3), rep("72h",3), rep("120h",3))
df.h_diw <- data.frame(h_diw,time.h_diw)
print(df.h_diw)
shapiro.test(H0_diw)#normal
shapiro.test(H0.5_diw)#normal
shapiro.test(H24_diw)#normal
shapiro.test(H72_diw)#normal
shapiro.test(H120_diw)#normal

aov.h_diw <- aov(h_diw~time.h_diw, data=df.h_diw)
summary(aov.h_diw)

aov.factor.h_diw = aov(h_diw ~ factor(time.h_diw), data=df.h_diw)
tukey.h_diw<-TukeyHSD(aov.factor.h_diw)
tukey.h_diw
cld.mn.h_diw <- multcompLetters4(aov.factor.h_diw, tukey.h_diw)
print(cld.mn.h_diw)
#$`factor(time.h_diw)`
#0.5h  24h   0h  72h 120h 
#"a" "ab" "ab"  "b"  "b" 

##################################################################################################################################
#Control napp: 72h-120h only diff
c_napp
time.c_napp <- c(rep("0h",3), rep("0.5h",3), rep("24h",3), rep("72h",3), rep("120h",3))
df.c_napp <- data.frame(c_napp,time.c_napp)
print(df.c_napp)
shapiro.test(C0_napp)#normal
shapiro.test(C0.5_napp)#normal
shapiro.test(C24_napp)#normal
shapiro.test(C72_napp)#normal
shapiro.test(C120_napp)#normal

aov.c_napp <- aov(c_napp~time.c_napp, data=df.c_napp)
summary(aov.c_napp)

aov.factor.c_napp = aov(c_napp ~ factor(time.c_napp), data=df.c_napp)
tukey.c_napp<-TukeyHSD(aov.factor.c_napp)
tukey.c_napp
cld.mn.c_napp <- multcompLetters4(aov.factor.c_napp, tukey.c_napp)
print(cld.mn.c_napp)
#$`factor(time.c_napp)`
#72h  24h   0h 0.5h 120h 
#"a" "ab" "ab" "ab"  "b" 


#Moderate napp: no diffs
m_napp
time.m_napp <- c(rep("0h",3), rep("0.5h",3), rep("24h",3), rep("72h",3), rep("120h",3))
df.m_napp <- data.frame(m_napp,time.m_napp)
print(df.m_napp)
shapiro.test(M0_napp)#normal
shapiro.test(M0.5_napp)#normal
shapiro.test(M24_napp)#normal
shapiro.test(M72_napp)#normal
shapiro.test(M120_napp)#normal

aov.m_napp <- aov(m_napp~time.m_napp, data=df.m_napp)
summary(aov.m_napp)

aov.factor.m_napp = aov(m_napp ~ factor(time.m_napp), data=df.m_napp)
tukey.m_napp<-TukeyHSD(aov.factor.m_napp)
tukey.m_napp
cld.mn.m_napp <- multcompLetters4(aov.factor.m_napp, tukey.m_napp)
print(cld.mn.m_napp)

#High napp: 72h-0h only diff
h_napp
time.h_napp <- c(rep("0h",3), rep("0.5h",3), rep("24h",3), rep("72h",3), rep("120h",3))
df.h_napp <- data.frame(h_napp,time.h_napp)
print(df.h_napp)
shapiro.test(H0_napp)#normal
shapiro.test(H0.5_napp)#normal
shapiro.test(H24_napp)#normal
shapiro.test(H72_napp)#normal
shapiro.test(H120_napp)#normal

aov.factor.h_napp = aov(h_napp ~ factor(time.h_napp), data=df.h_napp)
tukey.h_napp<-TukeyHSD(aov.factor.h_napp)
tukey.h_napp
cld.mn.h_napp <- multcompLetters4(aov.factor.h_napp, tukey.h_napp)
print(cld.mn.h_napp)
#$`factor(time.h_napp)`
#72h 120h 0.5h  24h   0h 
#"a" "ab" "ab" "ab"  "b" 

##################################################################################################################################
mn.time0<-c(C0_diw, M0_diw,H0_diw,C0_napp,M0_napp,H0_napp)
mn.time0.5<-c(C0.5_diw,M0.5_diw,H0.5_diw,C0.5_napp,M0.5_napp,H0.5_napp)
mn.time24<-c(C24_diw,M24_diw,H24_diw,C24_napp,M24_napp,H24_napp)
mn.time72<-c(C72_diw,M72_diw,H72_diw,C72_napp,M72_napp,H72_napp)
mn.time120<-c(C120_diw,M120_diw,H120_diw,C120_napp,M120_napp,H120_napp)

mn.time.tx0<-c(rep("c.diw.0",3), rep("m.diw.0",3), rep("h.diw.0",3),rep("c.napp.0",3), rep("m.napp.0",3), rep("h.napp.0",3))
mn.time.tx0.5<-c(rep("c.diw.0.5",3), rep("m.diw.0.5",3), rep("h.diw.0.5",3),rep("c.napp.0.5",3), rep("m.napp.0.5",3), rep("h.napp.0.5",3))
mn.time.tx24<-c(rep("c.diw.24",3), rep("m.diw.24",3), rep("h.diw.24",3),rep("c.napp.24",3), rep("m.napp.24",3), rep("h.napp.24",3))
mn.time.tx72<-c(rep("c.diw.72",3), rep("m.diw.72",3), rep("h.diw.72",3),rep("c.napp.72",3), rep("m.napp.72",3), rep("h.napp.72",3))
mn.time.tx120<-c(rep("c.diw.120",3), rep("m.diw.120",3), rep("h.diw.120",3),rep("c.napp.120",3), rep("m.napp.120",3), rep("h.napp.120",3))

##Time 0, DIW and NaPP
df.mn.0<-data.frame(mn.time0,mn.time.tx0)
print(df.mn.0)
shapiro.test(C0_diw)#normal
shapiro.test(M0_diw)#normal
shapiro.test(H0_diw)#normal
shapiro.test(C0_napp)#normal
shapiro.test(M0_napp)#normal
shapiro.test(H0_napp)#normal

aov.mn.0 <- aov(mn.time0~factor(mn.time.tx0), data=df.mn.0)
summary(aov.mn.0)
tukey.mn.0<-TukeyHSD(aov.mn.0)
tukey.mn.0
cld.mn.0 <- multcompLetters4(aov.mn.0, tukey.mn.0)
print(cld.mn.0)
#$`factor(mn.time.tx0)`
#h.diw.0 h.napp.0 m.napp.0  m.diw.0 c.napp.0  c.diw.0 
#"a"     "ab"      "b"      "c"      "c"      "c" 


##Time 0.5, DIW and NaPP
df.mn.0.5<-data.frame(mn.time0.5,mn.time.tx0.5)
print(df.mn.0.5)
shapiro.test(C0.5_diw)#normal
shapiro.test(M0.5_diw)#normal
shapiro.test(H0.5_diw)#normal
shapiro.test(C0.5_napp)#normal
shapiro.test(M0.5_napp)#normal
shapiro.test(H0.5_napp)#normal

aov.mn.0.5 <- aov(mn.time0.5~factor(mn.time.tx0.5), data=df.mn.0.5)
summary(aov.mn.0.5)
tukey.mn.0.5<-TukeyHSD(aov.mn.0.5)
tukey.mn.0.5
cld.mn.0.5 <- multcompLetters4(aov.mn.0.5, tukey.mn.0.5)
print(cld.mn.0.5)
#$`factor(mn.time.tx0.5)`
#h.diw.0.5 h.napp.0.5 m.napp.0.5  m.diw.0.5 c.napp.0.5  c.diw.0.5 
#"a"       "ab"       "bc"       "cd"       "cd"        "d"

##Time 24, DIW and NaPP
df.mn.24<-data.frame(mn.time24,mn.time.tx24)
print(df.mn.24)
shapiro.test(C24_diw)#normal
shapiro.test(M24_diw)#normal
shapiro.test(H24_diw)#normal
shapiro.test(C24_napp)#normal
shapiro.test(M24_napp)#normal
shapiro.test(H24_napp)#normal

aov.mn.24 <- aov(mn.time24~factor(mn.time.tx24), data=df.mn.24)
summary(aov.mn.24)
tukey.mn.24<-TukeyHSD(aov.mn.24)
tukey.mn.24
cld.mn.24 <- multcompLetters4(aov.mn.24, tukey.mn.24)
print(cld.mn.24)

#$`factor(mn.time.tx24)`
#h.diw.24 h.napp.24 m.napp.24  m.diw.24 c.napp.24  c.diw.24 
#"a"       "b"       "c"       "d"       "d"       "d" 

##Time 72, DIW and NaPP
df.mn.72<-data.frame(mn.time72,mn.time.tx72)
print(df.mn.72)
shapiro.test(C72_diw)#normal
shapiro.test(M72_diw)#normal
shapiro.test(H72_diw)#normal
shapiro.test(C72_napp)#normal
shapiro.test(M72_napp)#normal
shapiro.test(H72_napp)#normal

aov.mn.72 <- aov(mn.time72~factor(mn.time.tx72), data=df.mn.72)
summary(aov.mn.72)
tukey.mn.72<-TukeyHSD(aov.mn.72)
tukey.mn.72
cld.mn.72 <- multcompLetters4(aov.mn.72, tukey.mn.72)
print(cld.mn.72)

#$`factor(mn.time.tx72)`
#h.napp.72  h.diw.72 m.napp.72  m.diw.72 c.napp.72  c.diw.72 
#"a"       "b"       "c"       "d"       "d"       "d" 


##Time 120, DIW and NaPP
df.mn.120<-data.frame(mn.time120,mn.time.tx120)
print(df.mn.120)
shapiro.test(C120_diw)#not normal
shapiro.test(M120_diw)#normal
shapiro.test(H120_diw)#normal
shapiro.test(C120_napp)#normal
shapiro.test(M120_napp)#normal
shapiro.test(H120_napp)#normal

aov.mn.120 <- aov(mn.time120~factor(mn.time.tx120), data=df.mn.120)
summary(aov.mn.120)
tukey.mn.120<-TukeyHSD(aov.mn.120)
tukey.mn.120
cld.mn.120 <- multcompLetters4(aov.mn.120, tukey.mn.120)
print(cld.mn.120)
#$`factor(mn.time.tx120)`
#h.napp.120  h.diw.120 m.napp.120  m.diw.120 c.napp.120  c.diw.120 
#"a"        "b"       "bc"       "cd"        "d"        "d"
