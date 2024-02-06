load("I:\\Papers\\Counternull\\epigenetic.RData")
load("I:\\Papers\\Counternull\\w0.RData")


t.rep<-matrix(ncol=1,nrow=19448)

t.obs<-mean(final.first$cg00000029[final.first$exp==1])-mean(final.first$cg00000029[final.first$exp==0])

for(i in 1:19448)
{
  t.rep[i]<-mean(final.first$cg00000029[w0[,i]==1])-mean(final.first$cg00000029[w0[,i]==0])
  print(i)
}

# Fisher-exact p-value
sum(1*(t.rep>=t.obs))/19448
hist(t.rep)
abline(v=t.obs,col="red")

## Counternull

Y.temp<-matrix(ncol=1,nrow=17)
t.rep<-matrix(ncol=1,nrow=19448)

a<-0.018159

for(j in 1:19448)
{
  Y.temp<-final.first$cg00000029
  for(i in 1:17)
  {
    if(w0[i,j]==1 & final.first$exp[i]==0){Y.temp[i]<-final.first[i,"cg00000029"]+a}
    if(w0[i,j]==0 & final.first$exp[i]==1){Y.temp[i]<-final.first[i,"cg00000029"]-a}
  }
  t.rep[j,1]<-mean(Y.temp[w0[,j]==1])-mean(Y.temp[w0[,j]==0])
  print(j)
}

sum(1*(t.rep<=t.obs))/19448