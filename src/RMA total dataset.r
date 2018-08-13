#load package for reduced major axis regression
library(smatr)

OleChathI<-c(4.98,5.622,4.212,4.884,5.318)
OleTravI<-c(1.752,1.864,2.032,2.01,1.946,2.126)
RhipScanI<-c(81.52839,67.00354,77.62506,74.62506,74.16344,58.0968857,68.4678833,69.55762)
MyrChathI<-c(36.8101833,31.2799,38.2257333,43.2583429,38.56006,36.0497636,42.7879909)
CoroMacI<-c(38.73402857,34.37891429,36.0289833,36.5839333,35.05898333,39.6904,45.2967,46.80858889)
LepRobI<-c(18.4103,18.233813,15.64012,18.400667,21.60596,19.206231)
CopProI<-c(12.917183,16.62257,11.16299,15.83424,10.97223,14.85205,14.73944,14.419717)
MelChathI<-c(19.94452,20.345419,21.71224,22.368395,19.636909,18.183729,20.48147)
CopAceI<-c(8.82175,9.44165,9.2363417,11.281936,10.23441)
PipExeI<-c(3.10578,3.9236286,4.5070067)
ApiProI<-c(3.0071667,2.2410833,3.70155,2.30534,2.66398)
RhoSapI<-c(166.86342,198.47638,173.20368)
MueAusI<-c(9.94866,8.3007308,9.4523412)
TetTriI<-c(28.400694,29.925886)
CopChathI<-c(38.76)
HebDieI<-c(1.15)
PseuChathI<-c(5.00)
PseuKerI<-c(3.85)
AleExcI<-c(8.5)
StrSmiI<-c(7.5)
MyoKerI<-c(6.35)
MetKerI<-c(3.75)
MacMelI<-c(2.5)
AscLucI<-c(1.35)

PleuCrinM<-c(4.25)
OleVirM<-c(1.2)
RhipScanM<-c(54.90576,61.99508,47.4574875)
MyrArgDivM<-c(8.7,8.1)
CoroCotM<-c(15.5424,19.28276,18.9983)
LepJunM<-c(4.2357556,5.78086,5.42461,6.1233,8.3559833,6.9216)
CopProM<-c(11.75)
MelAlpM<-c(8.8963167,9.1752)
CopAceM<-c(3.263)
PipExeM<-c(3.18822,3.97469,3.59762)
ApiProM<-c(4.28301,3.37756,4.50232)
RhoSapM<-c(80.786)
MueAusM<-c(6.82302,7.50191,6.62624)
TetTriM<-c(15.56789,14.78877,12.40987)
CopRepM<-c(21.6075)
HebEllM<-c(1.1)
PseuCrasM<-c(2.85)
PseuArbM<-c(3.7)
AleExcM<-c(7.5)
StrBanM<-c(5.85)
MyoLaeM<-c(6.75)
MetExcM<-c(4)
MacExe<-c(2)
AscLucM<-c(1.65)

POOL<-function(){
A<-c(sample(OleChathI))
B<-c(sample(OleTravI))
C<-c(sample(RhipScanI))
D<-c(sample(MyrChathI))
E<-c(sample(CoroMacI))
F<-c(sample(LepRobI))
G<-c(sample(CopProI))
H<-c(sample(MelChathI))
I<-c(sample(CopAceI))
J<-c(sample(PipExeI))
K<-c(sample(ApiProI))
L<-c(sample(RhoSapI))
M<-c(sample(MueAusI))
N<-c(sample(TetTriI))
O<-c(sample(CopChathI))
P<-c(sample(HebDieI))
Q<-c(sample(PseuChathI))
Island<<-c(A[1],B[1],C[1],D[1],E[1],F[1],G[1],H[1],I[1],J[1],K[1],L[1],M[1],N[1],O[1],P[1],Q[1])

AA<-c(sample(PleuCrinM))
BB<-c(sample(OleVirM))
CC<-c(sample(RhipScanM))
DD<-c(sample(MyrArgDivM))
EE<-c(sample(CoroCotM))
FF<-c(sample(LepJunM))
GG<-c(sample(CopProM))
HH<-c(sample(MelAlpM))
II<-c(sample(CopAceM))
JJ<-c(sample(PipExeM))
KK<-c(sample(ApiProM))
LL<-c(sample(RhoSapM))
MM<-c(sample(MueAusM))
NN<-c(sample(TetTriM))
OO<-c(sample(CopRepM))
PP<-c(sample(HebEllM))
QQ<-c(sample(PseuCrasM))
Mainland<<-c(AA[1],BB[1],CC[1],DD[1],EE[1],FF[1],GG[1],HH[1],II[1],JJ[1],KK[1],LL[1],MM[1],NN[1],OO[1],PP[1],QQ[1])}

#LogIsland<-c(log(Island))
#LogMainland<-c(log(Mainland))

#fit1 <- line.cis(LogIsland, LogMainland, alpha = 0.05, data = NULL, method = "SMA", intercept = TRUE, f.crit = 0)
#slope.test(LogIsland,LogMainland,test.value = 1,data=NULL, method = "SMA",alpha = 0.05,intercept = TRUE )

B <- 10000  # No.of bootstrap samples
boot.slopes <- rep(NA,B)
for (i in 1:B)
{
	POOL()
	LI <- log(Island)
	LM <- log(Mainland)
	this.fit <- line.cis(LI,LM, alpha = 0.05,         method = "SMA")
	boot.slopes[i] <- this.fit[2,1]}

mean(boot.slopes)
sd(boot.slopes)
summary(boot.slopes)


B <- 10000  # No.of bootstrap samples
boot.elevs <- rep(NA,B)
for (i in 1:B)
{
	POOL()
	LI <- log(Island)
	LM <- log(Mainland)
	this.fit <- line.cis(LI,LM, alpha = 0.05,         method = "SMA")
	boot.elevs[i] <- this.fit[1,1]}

mean(boot.elevs)
sd(boot.elevs)
summary(boot.elevs)




x<-
s<-
n<-
error<-qnorm(0.975)*s/sqrt(n)
left<-x-error
right<-x+error


P-values
 a <- 5
> s <- 2
> n <- 20
> xbar <- 7
> z <- (xbar-a)/(s/sqrt(n))
> z
[1] 4.472136
> 2*pnorm(-abs(z))
