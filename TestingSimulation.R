avg=c()
eg=c()
e1=list()
n1=3/(pi^2)
Pik=c()
Pjk=c()
Ik=-1000:1000
Pik[1001]=0
for(k2 in 1:1000)
{
  Pik[1001-k2]=n1/(k2^2)
  Pik[1001+k2]=n1/(k2^2)
}
Jk=-1000:1000
Pjk[1000:1002]=c(0.5,0,0.5)
for(k1 in 1:999)
{
  Pjk[1000-k1]=0
  Pjk[1002+k1]=0
}
for(u in 1:50)
{
for(k in 1:30)
{
  k=2
  n=50
  p=50
  A=matrix(nrow=n,ncol=p,byrow=T)
  for(i in 1:n)
  {
    if(i %% 2 ==0)
    {
      A[i,]=sample(Ik,size=p,prob=Pik,replace = TRUE)
    }
    else
      A[i,]=sample(Jk,size=p,prob=Pjk,replace = TRUE)
  }
  library(MASS)
  tau=matrix(rep(0,p^2),nrow=p,ncol=p,byrow=T)
  for(m in 1:n)
  {
    for(q in 1:n)
    {
      if(m != q)
      {
        tau = tau + ((as.matrix(sign(A[m,] - A[q,])) %*% t(as.matrix(sign(A[m,] - A[q,]))))/(n*(n-1)))
      }  
    }
  }
  T=tau-diag(diag(tau))
  tra= sum(diag(T%*%T))/p
  avg[k]=tra
  e=eigen(T)
  e1[[k]]=e$values
}
e2=rep(0,p)
for(j in 1:k)
{
  e2=(e2+e1[[j]])
}
e11=(e1[[1]]+e1[[2]])/2
emean=e2/k
avgdi=c()
for(j in 1:k)
{
avgdi[j]=(sum((e1[[j]]-emean)^2))
}
u=1
avgdis=c()
avgdis[u]=sqrt(sum(avgdi)/k)
print(u)
}
hist(avgdis)
q1=quantile(avgdis,probs = 0.95)
w=length(which(avgdis >q1))
w/333
avgdis[1:10]

