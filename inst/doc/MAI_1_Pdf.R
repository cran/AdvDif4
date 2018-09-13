# Anomalous diffusion coefficients function
# Normal PDF function as initial condition and no source.
# No advection, bi-blux (primary diffusion only) and with constant beta.
#
# Beta function
fbeta<-function(p)
{f<-0.8
return(f)}

dbetadp<-function(p)
{f<-0
return(f)}


# Inicial condition
fn<-function(x)
{m<-2
sigma<-0.03
f<-1/(sigma*sqrt(2*pi))*exp(-(x-m)^2/(2*sigma^2))
return(f)}

# velocity
v<-0

# Source function
fs<-function(x,t)
{f<-0
return(f)}

# diffusion coefficients parameter
k2<-0.1
k4<-0

# Space and temporal definition
l<-4
m<-500
tf<-1
n<-1000

# Left boundary contitions
w10<-0
w11<-1
w12<-0
w20<-0
w21<-0
w22<-1
fw1<-function(t)
{ f<-0
 return(f)}
fw2<-function(t)
{ f<-0
 return(f)}

# Right boundary conditions
e10<-0
e11<-1
e12<-0
e20<-0
e21<-0
e22<-1
fe1<-function(t)
{ f<-0
 return(f)}
fe2<-function(t)
{ f<-0
 return(f)}
#
parm=c(k2,k4,v,l,m,tf,n,w10,w11,w12,w20,w21,w22,e10,e11,e12,e20,e21,e22)
func=c(fbeta=fbeta,dbetadp=dbetadp,fn=fn,fs=fs,fw1=fw1,fw2=fw2,fe1=fe1,fe2=fe2)
#
ad<-AdvDif4(parm,func)

eixo<-seq(1,3,by=(4/m))
plot(eixo,ad[1,(m/4+1):(3*m/4+1)],type='l',col="red",xaxt="n",xlab="X", ylab="p(x,t)")
axis(1,seq(0,(3*l/4),(l/4/5)),las=2)
lines(eixo,ad[n/100,(m/4+1):(3*m/4+1)],type='l',col="orange")
lines(eixo,ad[n/10,(m/4+1):(3*m/4+1)],type='l',col="green")
lines(eixo,ad[n,(m/4+1):(3*m/4+1)],type='l',col="black")
