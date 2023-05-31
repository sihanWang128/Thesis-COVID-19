
library(ggplot2)
library(tidyverse)
library(dplyr)
temp=read.table('/Users/sihanwang/hosp_estimat/hosp_temp.txt')[,c(1,3,9)]
wind=read.table('/Users/sihanwang/hosp_estimat/hosp_wind.txt')[,c(1,3,9)]
hum=read.table('/Users/sihanwang/hosp_estimat/hosp_humid.txt')[,c(1,3,9)]
precip=read.table('/Users/sihanwang/hosp_estimat/hosp_precip.txt')[,c(1,3,9)]
cases=read.table('/Users/sihanwang/hosp_estimat/hosp_cases.txt')[,c(1,3,9)]
deaths=read.table('/Users/sihanwang/hosp_estimat/hosp_deaths.txt')[,c(1,3,9)]
hosp=read.table('/Users/sihanwang/hosp_estimat/hosp_hospad.txt')[,c(1,3,9)]
icu=read.table('/Users/sihanwang/hosp_estimat/hosp_icuad.txt')[,c(1,3,9)]
reproduction=read.table('/Users/sihanwang/hosp_estimat/hosp_reproduction.txt')[,c(1,3,9)]
fatality=read.table('/Users/sihanwang/hosp_estimat/hosp_fatality.txt')[,c(1,3,9)]
stringency=read.table('/Users/sihanwang/hosp_estimat/hosp_stringency.txt')[,c(1,3,9)]


colnames(temp)=c('variable', 'temp_coef', 'temp_p')
colnames(wind)=c('variable', 'wind_coef', 'wind_p')
colnames(precip)=c('variable', 'precip_coef', 'precip_p')
colnames(hum)=c('variable', 'hum_coef', 'hum_p')
colnames(cases)=c('variable', 'case_coef', 'case_p')
colnames(deaths)=c('variable', 'death_coef', 'deaths_p')
colnames(hosp)=c('variable', 'hosp_coef', 'hosp_p')
colnames(icu)=c('variable', 'icu_coef', 'icu_p')
colnames(fatality)=c('variable', 'fatality_coef', 'fatality_p')
colnames(reproduction)=c('variable','reproduction_coef', 'reproduction_p')
colnames(stringency)=c('variable', 'stringency_coef', 'stringency_p')

df=data.frame(c(temp, wind[, 2:3], precip[,2:3], hum[,2:3], cases[,2:3], 
                deaths[,2:3], hosp[,2:3], icu[,2:3],fatality[,2:3], reproduction[,2:3], stringency[,2:3]))

df.l1=df%>%filter(df$variable=='temp.l1')
df.l2=df%>%filter(df$variable=='temp.l2')
df.l1$week=as.Date(weekly$week[70:142])
df.l2$week=as.Date(weekly$week[70:142])

 

############################################################
###############             p plot           ###############
############################################################
ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$hosp_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$hosp_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('Hospital admission P-value')

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$icu_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$icu_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('ICU admission P-value')

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$temp_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$temp_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('Temperature P-value')

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$case_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$case_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('Confirmed cases P-value')

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$deaths_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$deaths_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('Deaths P-value')

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$fatality_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$fatality_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('Fatality rate P-value')

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$reproduction_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$reproduction_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('Reproduction rate P-value')

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$stringency_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$stringency_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('Stringency P-value')

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$hum_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$hum_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('Humidity P-value')

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$precip_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$precip_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('Rain fall P-value')

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$wind_p, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$wind_p, color='week t-2'))+
  geom_line(mapping=aes(y=0.05, color='0.05'))+
  xlab("time") + ylab('p_value')+scale_color_manual(values=c('black', 'green', 'red'))+
  ggtitle('Wind speed P-value')



############################################################
###############          coef plot           ###############
############################################################

ggplot(df.l1, mapping=aes(as.Date(week)))+
  geom_line(mapping=aes(y=df.l1$hosp_coef, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$hosp_coef, color='week t-2'))+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  scale_x_date(labels=date_format ("%m-%y"))+
  ggtitle('Hospital admission Coeficient')+
  xlab("time") + ylab('Coefficient')+scale_color_manual(values=c( 'lightblue', 'orange'))

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$case_coef, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$case_coef, color='week t-2'))+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  scale_x_date(labels=date_format ("%m-%y"))+
  ggtitle('Confirmed Cases Coeficient')+
  xlab("time") + ylab('Coefficient')+scale_color_manual(values=c( 'lightblue', 'orange'))

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$fatality_coef, color='Fatality'))+
  geom_line(mapping=aes(y=df.l2$reproduction_coef, color='Reproduction'))+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  scale_x_date(labels=date_format ("%m-%y"))+
  ggtitle('Fatality and Reproduction Coefficient')+
  xlab("time") + ylab('Coefficient')+scale_color_manual(values=c( 'lightblue', 'orange'))

ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l1$death_coef, color='week t-1'))+
  geom_line(mapping=aes(y=df.l2$death_coef, color='week t-2'))+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  scale_x_date(labels=date_format ("%m-%y"))+
  ggtitle('Deaths Coefficient')+
  xlab("time") + ylab('Coefficient')+scale_color_manual(values=c( 'lightblue', 'orange'))


ggplot(df.l1, mapping=aes(week))+
  geom_line(mapping=aes(y=df.l2$temp_coef, color='Temperature'))+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  scale_x_date(labels=date_format ("%m-%y"))+
  ggtitle('Temperature Coefficient')+
  xlab("time") + ylab('Coefficient')+scale_color_manual(values=c( 'lightblue'))


ggplot(daily[401:971,], mapping=aes(as.Date(daily$date[401:971])))+
  geom_line(mapping=aes(y=daily$icu_patients[401:971], color="actual"))+
  geom_line(mapping=aes(y =unlist(dayt1_pred_var10), color = "predict"))+
  xlab("Time") + ylab('ICUl admission')+
  theme(panel.background = element_rect(fill = "white", colour = "black"))+
  scale_x_date(labels=date_format ("%m-%y"))+
  scale_color_manual(values=c('red','blue'))


