\name{AdvDif4}
\alias{AdvDif4}
\title{Solving 1D Advection Bi-Flux Diffusion Equation}
\usage{AdvDif4(parm,func)}
\arguments{
  \item{parm}{Alternative to inform parameters data. It must contain values for k2,k4,v,l,m,tf,n,w10,w11,w12,w20,w21,w22,e10,e11,e12,e20,e21,e20 needed to run the model.}
  \item{func}{Alternative to inform functions. It must contain the functionsbeta,dbetadp,fn,fw1,fw2,fe1,fe2 needed to run the model.}
}
\value{The resulting matrix with results obtained for each time as rows and at each position as columns.}
\description{This software solves an Advection Bi-Flux Diffusive Problem using the Finite Difference Method FDM.
A file with R commands can be consulted in document folder.}
\examples{
#
# Begin of the first example
#
# Anomalous diffusion coefficients function
# Beta function
fbeta<-function(p)
{f<-0.2
return(f)}
# Beta derivative function
dbetadp<-function(p)
{f<-0
return(f)}
# diffusion coefficients
k2<-1E-3
k4<-1E-5
# Inicial condition
fn<-function(x)
{ f<-sin(pi*x)
 return(f)}
# velocity
v<-0.0
# Space and temporal definition
l<-1
m<-100
tf<-100
n<-1000
# Left boundary conditions
w10<-1
w11<-0
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
e10<-1
e11<-0
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
parm=c(k2,k4,v,l,m,tf,n,w10,w11,w12,w20,w21,w22,e10,e11,e12,e20,e21,e22)
func=c(fbeta=fbeta,dbetadp=dbetadp,fn=fn,fw1=fw1,fw2=fw2,fe1=fe1,fe2=fe2)
#
ad<-AdvDif4(parm,func)
eixo<-seq(0,1,by=0.01)
plot(eixo,ad[1,1:101],type='l',col="red",xaxt="n",xlab="X", ylab="p(x,t)")
axis(1,seq(0,1,0.1),las=2)
lines(eixo,ad[250,1:101],type='l',col="green")
lines(eixo,ad[500,1:101],type='l',col="green")
lines(eixo,ad[750,1:101],type='l',col="green")
lines(eixo,ad[1000,1:101],type='l',col="green")
#
#
# End of the first example
#
#
# Begin of the second example
#
# Anomalous diffusion coefficients function
# Beta function
fbeta<-function(p)
{betamin<-0.2
betamax<-1
gama<-2500
pin<-0.001
f<-betamax-(betamax-betamin)/(1+exp(-gama*(p-pin)))
return(f)}
# Beta derivative function
dbetadp<-function(p)
{betamin<-0.2
betamax<-1
gama<-2500
pin<-0.001
f<-(-gama*(betamax-betamin)*exp(-gama*(p-pin))/((1+exp(-gama*(p-pin)))^2))
return(f)}
# Inicial condition
fn<-function(x){
f<-sin(pi*x)^100
return(f)}
# velocity
v<-0
# diffusion coefficients parameter
k2<-1e-3
k4<-1e-5
# Space and temporal definition
l<-1
m<-100
tf<-1
n<-1000
# Left boundary conditions
w10<-1
w11<-0
w12<-0
w20<-0
w21<-1
w22<-0
fw1<-function(t)
{ f<-0
return(f)}
fw2<-function(t)
{ f<-0
return(f)}
# Right boundary conditions
e10<-1
e11<-0
e12<-0
e20<-0
e21<-1
e22<-0
fe1<-function(t)
{ f<-0
 return(f)}
fe2<-function(t)
{ f<-0
 return(f)}
#
parm=c(k2,k4,v,l,m,tf,n,w10,w11,w12,w20,w21,w22,e10,e11,e12,e20,e21,e22)
func=c(fbeta=fbeta,dbetadp=dbetadp,fn=fn,fw1=fw1,fw2=fw2,fe1=fe1,fe2=fe2)
#
ad<-AdvDif4(parm,func)
eixo<-seq(0,1,by=0.01)
plot(eixo,ad[1,1:101],type='l',col="red",xaxt="n",xlab="X", ylab="p(x,t)")
axis(1,seq(0,1,0.1),las=2)
lines(eixo,ad[250,1:101],type='l',col="green")
lines(eixo,ad[500,1:101],type='l',col="green")
lines(eixo,ad[750,1:101],type='l',col="green")
lines(eixo,ad[1000,1:101],type='l',col="green")
#
# End of the second example
#
# There are more examples inside "doc"" folder
#
}
