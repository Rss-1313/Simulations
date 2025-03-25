fun=function(Pi1k,Pj1k,Pj2k)
{ 
  g=c()
  g1=c()
  g2=c()
  g3=c()
  g4=c()
  g5=c()
  g6=c()
  g7=c()
  g8=c()
  Fj1=c()
  Fbarj1=c()
  Fj2=c()
  Fbarj2=c()
  for(k3 in 2:2001)
  {
    Fbarj1=sum(Pj1k[1:(k3-1)])
    Fbarj2=sum(Pj2k[1:(k3-1)])
    Fj1=sum(Pj1k[1:k3])
    Fj2=sum(Pj2k[1:k3])
    g1[k3-1]=(Fbarj1*Fbarj2*Pi1k[k3])
    g2[k3-1]=(Fbarj1*Fj2*Pi1k[k3])
    g3[k3-1]=(Fj1*Fbarj2*Pi1k[k3])
    g4[k3-1]= (Fj1*Fj2*Pi1k[k3])
    g5[k3-1]= -(Fbarj1*Pi1k[k3])
    g6[k3-1]=-(Fbarj2*Pi1k[k3])
    g7[k3-1]=-(Fj1*Pi1k[k3])
    g8[k3-1]=-(Fj2*Pi1k[k3])
  } 
  g =sum(g1)+sum(g2)+sum(g3)+sum(g4)+sum(g5)+sum(g6)+sum(g7)+sum(g8)+1 
  return(g)
}
fun1=function(a111,b111,c111,a221,b221,c221,theta)
{
  gamma11=(a111+c111)/2
  gamma21=(a221+c221)/2
  gamma1=(gamma11+gamma21)/2
  a112=((a111*a111)+(b111*b111))/2
  b112=((b111*c111)+(b111*a111))/2
  c112=((b111*b111)+(c11*c111))/2
  a222=((a221*a221)+(b221*b221))/2
  b222=((b221*c221)+(b221*a221))/2
  c222=((b221*b221)+(c221*c221))/2
  gamma12=(a112+c112)/2
  gamma22=(a222+c222)/2
  gamma2=(gamma12+gamma22)/2
  a113=((a112*a111)+(b112*b111))/2
  b113=((b112*c111)+(b111*a112))/2
  c113=((b112*b111)+(c112*c111))/2
  a223=((a222*a221)+(b222*b221))/2
  b223=((b222*c221)+(b221*a222))/2
  c223=((b222*b221)+(c222*c221))/2
  gamma13=(a113+c113)/2
  gamma23=(a223+c223)/2
  gamma3=(gamma13+gamma23)/2
  a114=((a113*a111)+(b113*b111))/2
  b114=((b113*c111)+(b111*a113))/2
  c114=((b113*b111)+(c113*c111))/2
  a224=((a223*a221)+(b223*b221))/2
  b224=((b223*c221)+(b221*a223))/2
  c224=((b223*b221)+(c223*c221))/2
  gamma14=(a114+c114)/2
  gamma24=(a224+c224)/2
  gamma4=(gamma14+gamma24)/2
  v1=(2^2)*(theta)*gamma2
  v2=(2^3)*((theta^2)*gamma3)
  v3=(2^4)*(((theta^3)*gamma4)+(2*(theta^2)*(gamma2^2)))
  v=c(v1,v2,v3,gamma1,gamma2,gamma3,gamma4)
  return(v)
}

n1=3/(pi^2)
Pik=c()
Pjk=c()
Ik=-1000:1000
Pik[1001]=0
for(k in 1:1000)
{
  Pik[1001-k]=n1/(k^2)
  Pik[1001+k]=n1/(k^2)
}
Jk=-1000:1000
Pjk[1000:1002]=c(0.5,0,0.5)
for(k1 in 1:999)
{
  Pjk[1000-k1]=0
  Pjk[1002+k1]=0
}
#When i is even we have Pik and i odd means Pjk distribution. i even considered first 
G1oddodd=fun(Pik,Pjk,Pjk)
G1oddeve=fun(Pik,Pjk,Pik)
G1eveodd=fun(Pik,Pik,Pjk)
G1eveeve=fun(Pik,Pik,Pik)
G2oddodd=fun(Pjk,Pjk,Pjk)
G2oddeve=fun(Pjk,Pjk,Pik)
G2eveodd=fun(Pjk,Pik,Pjk)
G2eveeve=fun(Pjk,Pik,Pik)

a11=fun(Pik,Pjk,Pjk)
b11=fun(Pik,Pjk,Pik)
b11=fun(Pik,Pik,Pjk)
c11=fun(Pik,Pik,Pik)
a21=fun(Pjk,Pjk,Pjk)
b21=fun(Pjk,Pjk,Pik)
b21=fun(Pjk,Pik,Pjk)
c21=fun(Pjk,Pik,Pik)
Lval=fun1(a11,b11,c11,a21,b21,c21,0.5)
#Lval contains the three values of 2^rE[Z-gamma1]^r 
#followed by values of gamma1,gamma2,gamma3,gamma4
