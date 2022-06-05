# Program to implement a power analysis of a paired experiment
# as in Liermann and Roni (2008)
#N is the number of sites
#n1 is the number of Before years
#n2 is the number of After years
#theta is the true effect size which is the true shift in the difference
# in treatment and control log(smolts) between the Before and After periods.
#
lr<-function(vartime=1,varsite=1,N=20,n1=2,n2=8,theta=.02,alpha=0.05){
sd<-vartime*(1/n1+1/n2)+varsite
sd<-sqrt(sd)
se<-sd/sqrt(N)
cv<-se/theta
delta=theta
q<-qt(p=1-alpha/2,df=N-1)
power<-1-pt(q,ncp=delta/se,df=N-1)+pt(-q,ncp=delta/se,df=N-1)
return(list(vartime=vartime,varsite=varsite,N=N,n1=n1,n2=n2,theta=theta,alpha=alpha,
se=se,cv=cv,power=power))
}
#outputs
#se -- standard error
#cv -- coefficient of variation
#power -- probability of rejecting the null hypothesis of no treatment effect
