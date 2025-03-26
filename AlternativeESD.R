alpha=1
e=list()
di=c()
n1=3/(pi^2)
Pik=c()
Pjk=c()
dif=c()
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
for(u in 1:240)
{
  k=1
  n=400
  p=400
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
  #construction of dependent matrix
  B=matrix(nrow=n,ncol=p,byrow=T)
  for(i in 1:n)
  {
    B[i,1]=A[i,1]
    for(j in 2:p)
    {
      B[i,j]=A[i,j]-(alpha*A[i,j-1])
    }
  }
  library(MASS)
  tau1=matrix(rep(0,p^2),nrow=p,ncol=p,byrow=T)
  for(m in 1:n)
  {
    for(q in 1:n)
    {
      if(m != q)
      {
        tau1 = tau1 + ((as.matrix(sign(A[m,] - A[q,])) %*% t(as.matrix(sign(A[m,] - A[q,]))))/(n*(n-1)))
      }
    }
  }
  T1=tau1-diag(diag(tau1))
  # tra= sum(diag(T%*%T))/p
  #avg[k]=tra
  e1=eigen(T1)
  e[[k]]=e1$values
  tau2=matrix(rep(0,p^2),nrow=p,ncol=p,byrow=T)
  for(m in 1:n)
  {
    for(q in 1:n)
    {
      if(m != q)
      {
        tau2 = tau2 + ((as.matrix(sign(B[m,] - B[q,])) %*% t(as.matrix(sign(B[m,] - B[q,]))))/(n*(n-1)))
      }
    }
  }
  T2=tau2-diag(diag(tau2))
  # tra= sum(diag(T%*%T))/p
  #avg[k]=tra
  e2=eigen(T2)
  e[[k+1]]=e2$values
}
di[u]=ks.test(e[[1]],e[[2]])$s
print(u)
}