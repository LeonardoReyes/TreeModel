# Tree model Maurits Leonardo
#Loading Packages

if(!require('ggplot2')){
  install.packages('ggplot2')
  require('ggplot2')
} else{require('ggplot2')}

if(!require('reshape2')){
  install.packages('reshape2')
  require('reshape2')
} else{require('reshape2')}

if(!require('scales')){
  install.packages('scales')
  require('scales')
} else{require('scales')}

if(!require('deSolve')){
  install.packages('deSolve')
  require('deSolve')
} else{require('deSolve')}


## Loading data----
data<-read.csv('JuneWET.csv',header=T,skip=1)
sapdata<-read.csv('SAP_QI_MOD.csv',header=T,skip=1)
soildata<-read.csv('Soil_DATA_QI.csv',header=T,skip=2)

## Input Parameters----
NoPlot<-1
Neg<-c(-1) #Negativity of water potential from soil
Opt=0      #if 1 set for optimization runs with SCEM UA

limitini<-1 #range of dataset
limitend<-200 #range of dataset (max 1260)
F_stem<-sapdata$SapNN.Qi[limitini:limitend]*3600#in cm3/hour converted in g/s        stem sap flux from data files
F_stem[F_stem<0.001]<-0.01 #negative values equal to zero (kwam niet voor in periode)
Sz<-limitend # size data set
SapFlow<-F_stem # g/s
ShortWave<-data$Shortwave.Incomming.Radiation[limitini:limitend]       #Watt m^2   Short Wave incomming solar radiation
ShortWave[ShortWave<0]<-0
T_air<-data$Air.Temperature[limitini:limitend]#°C         air temperature
RH<-data$Relative.humidity[limitini:limitend] # #         relative humidity
VPD<-data$VPD[limitini:limitend]              # kPa       Vapour presssure defficit
VPD[VPD<0]<-0
range<-max(VPD,na.rm=T)-min(VPD,na.rm=T)
NormVPD<-(VPD-min(VPD,na.rm=T))/range
#LabelTime<-datestr(data(limitini:limitend,1)+ datenum('30-Dec-1899'))     # Label Times
F_stem_ini<-SapFlow[1]
F_soil_ini<-F_stem_ini[1]
F_soilTg_ini<-F_stem_ini[1]

Psi_soil<-soildata$X75.cm[limitini:limitend]/10*Neg  #MPa        soil matrix potential
# Psi_soil<-rep(1,Sz)*0875                 #MPa        soil matrix potential
#if isempty(Psi_soil)|| ~exist('Psi_soil','var')
#Psi_soil<-10^Pfextrap(soildata$X100cm[limitini:limitend],4)*000009807*Neg #derived from pF dataset
#end
# Curve 1 <- 23_28 cm depth
# Curve 2 <- 48_53 cm depth
# Curve 3 <- 73_78 cm depth
# Curve 4 <- 98_103 cm depth
# Curve 5 <- 123_128 cm depth

# Constants ----
R<-rep(1,Sz)*8314        #J/(mol*K)  Universal gas constant
Vw_0<-rep(1,Sz)*18  #m3/mol     molar volume of water

if(!NoPlot==0){
  
}

## Initial Resistance Guess

# Parameters
ChangeRes<-50000     ## Opt Val 1 100000
ChangeRoot<-5000 ## Opt Val 001 1000
ChangeRoot_Stem<-1000## Opt Val 002 2000
ChageStem_Crown<-5000## Opt Val 001 1000

if(Opt==1){
  R_X_crownair<-rep(1,Sz)*Pars(1)
  #R_X_crownair<-FourierForResistance(limitend,Pars(1))
  R_X_stemcrown<-rep(1,Sz)*Pars(2)
  R_X_rootstem<-rep(1,Sz)*Pars(3)
  R_X_soilrootTu<-rep(1,Sz)*04693
  R_X_ShallowRootDeepRoot<-rep(1,Sz)*Pars(4)
  
  #     R_X_crownair<-FourierForResistance(limitend,[Pars(1) Pars(2)])   #Mpas/g    xylem resistance between crown and air
  #     #R_X_stemcrown<-rep(1,Sz)*05673  #Mpas/g    xylem resistance between stem and crown
  #     R_X_stemcrown<-FourierForResistance(limitend,[Pars(3) Pars(4)])
  #     # R_X_rootstem<-rep(1,Sz)*01618   #Mpas/g    xylem resistance between root and stem
  #     #R_X_rootstem<-rep(1,Sz)*Pars(4)*0095   #Mpas/g    xylem resistance between root and stem
  #     R_X_rootstem<-FourierForResistance(limitend,[Pars(5) Pars(6)])   #Mpas/g    xylem resistance between root and stem
  #     R_X_soilrootTu<-rep(1,Sz)*04693   #Mpas/g    xylem resistance between soil and root
  #     #R_X_ShallowRootDeepRoot<-rep(1,Sz)*Pars(4)*015   #Mpas/g    xylem resistance between soil and root
  #     R_X_ShallowRootDeepRoot<-FourierForResistance(limitend,[Pars(7) Pars(8)])   #Mpas/g    xylem resistance between soil and root
  
  
  #     R_X_crownair<-FourierForResistance(limitend,ParVec(1))
  #     R_X_stemcrown<-rep(1,Sz)*ParVec(2)
  #     R_X_rootstem<-rep(1,Sz)*ParVec(3)
  #     R_X_soilroot<-rep(1,Sz)*ParVec(4)
}else{
#   R_X_crownair<-(NormVPD*9)-1   #Mpas/g    xylem resistance between crown and air
  
  Change<-1
  R_X_crownair<-(VPD*-1)+10  #Mpas/g    xylem resistance between crown and air
  
  # Previous good value R_X_crownair<-FourierForResistance(limitend,200)'*NormVPD
  
  #     R_X_crownair<-NormVPD*Pars(1)   #Mpas/g    xylem resistance between crown and air
  
  #R_X_crownair<-NormResistance(Pars(1)-5,Pars(1),NormVPD,range)   #Mpas/g    xylem resistance between crown and air

  #R_X_crownair<-FourierForResistance(limitend,Pars)'*NormVPD   #Mpas/g    xylem resistance between crown and air
  R_X_stemcrown<-rep(1,Sz)*0.5673/Change  #Mpas/g    xylem resistance between stem and crown
  #R_X_stemcrown<-NormResistance(Pars(2)-1,Pars(2),NormVPD,range)
  R_X_rootstem<-rep(1,Sz)*0.1618/Change   #Mpas/g    xylem resistance between root and stem
  #R_X_rootstem<-rep(1,Sz)*Pars(4)*0.095   #Mpas/g    xylem resistance between root and stem
  #R_X_rootstem<-NormResistance(Pars(3)-05,Pars(3),NormVPD,range)   #Mpas/g    xylem resistance between root and stem
  R_X_soilrootTu<-rep(1,Sz)*0.4693/Change   #Mpas/g    xylem resistance between soil and root
  R_X_ShallowRootDeepRoot<-rep(1,Sz)*0.15/Change   #Mpas/g    xylem resistance between soil and root
  #R_X_ShallowRootDeepRoot<-NormResistance(Pars(4)-05,Pars(4),NormVPD,range)   #Mpas/g    xylem resistance between soil and root
  #     R_X_crownair<-rep(1,Sz)*(00001829*ChangeRes)   #Mpas/g    xylem resistance between crown and air
  #     R_X_stemcrown<-rep(1,Sz)*(00001829*ChageStem_Crown)  #Mpas/g    xylem resistance between stem and crown
  #     R_X_rootstem<-rep(1,Sz)*(00001829*ChangeRoot_Stem)   #Mpas/g    xylem resistance between root and stem
  #     R_X_soilroot<-rep(1,Sz)*(00001829*ChangeRoot)   #Mpas/g    xylem resistance between soil and root
  # Psi_S_stem_p_0<-NormResistance(Pars(5)-10,Pars(5),NormVPD,range)
  # Psi_S_stem_p_0<-Pars(5)
}

## Guess from Tyree and Zimmermann
# Parameters
# R_X_crownair<-rep(1,Sz)*60*10^-9   #Mpas/g    xylem resistance between crown and air
# R_X_stemcrown<-rep(1,Sz)*60*10^-9*025  #Mpas/g    xylem resistance between stem and crown
# R_X_rootstem<-rep(1,Sz)*60*10^-9*025   #Mpas/g    xylem resistance between root and stem
# R_X_soilroot<-rep(1,Sz)*60*10^-9   #Mpas/g    xylem resistance between soil and root
R_S_stem<-rep(1,Sz)*0.1829     #Mpas/g    xylem resistance between stem xylem and storage compartment
R_S_roots<-rep(1,Sz)*0.0001829      #Mpas/g    xylem resistance between root xylem and storage compartment

# Cper<-360
# if Opt<-<-1
#     Cper<-Pars(5)
# else
#     Cper<-Pars(5)
#     #Cper<-503 Optimized value for best results on 01 July 2012
# end
#1 Steppe
#109 Guess for Qi

# C<-Pars(5) ## Original value 0.2123 Cper dummy variable for calibration
# C2<-Pars(6)  ## Original value 0.12
# C3<-Pars(7)  ## Originalvalue 11576

# C<-02123 ## Original value 0.2123
# C2<-0121  ## Original value 0.12
# C3<-11576  ## Originalvalue 11576

C<-0.21 ## Original value 02123
C2<-0.12  ## Original value 012
C3<-11576  ## Originalvalue 11576


# C<-Pars(8) ## Original value 0.2123
# C2<-Pars(9)  ## Original value 0.12
# C3<-Pars(10)  ## Originalvalue 11576

Change2<-1

C_stem<-rep(1,Sz)*C/Change2           #g/Mpa      stem capacitance after De Pauw et al (2008)
C_root<-rep(1,Sz)*C2/Change2          #g/Mpa      root capacitance Initial guess
C_crown<-rep(1,Sz)*C3/Change2         #g/Mpa      crown capacitance
C_rootTg<-rep(1,Sz)*C2*0.25/Change2   #g/Mpa      Deep root capacitance Initial guess
# C_rootTg<-rep(1,Sz)*C2*001          #g/Mpa      Deep root capacitance Initial guess

p_root<-rep(1,Sz)*0.01             #frac     percernage of stem water in roots Initial guess 0.1
p_crown<-rep(1,Sz)*0.04             #frac     percernage of stem water in crown Initial guess 0.4
p_deeproot<-rep(1,Sz)*0.005        #frac     percernage of stem water in deep roots Initial guess 0.05

# p_root<-rep(1,Sz)*00000000001           #frac     percernage of stem water in roots
# p_crown<-rep(1,Sz)*00000000001          #frac     percernage of stem water in crown
# p_deeproot<-rep(1,Sz)*0000000001           #frac     percernage of stem water in deep roots

# p_root<-rep(1,Sz)*Pars(8)           #frac     percernage of stem water in roots
# p_root<-rep(1,Sz)*Pars(7)           #frac     percernage of stem water in roots
# p_crown<-rep(1,Sz)*Pars(8)          #frac     percernage of stem water in crown

Psi_X_initial<-Psi_soil[1]    #Mpa      initial soil water potential
Psi_S_stem_initial<-Psi_soil[1]*1.2#Mpa     initial soil water potential #*12 initial guess (starting model from the night, should be ap one)
Psi_X_stem_initial<-Psi_soil[1]*1.2#Mpa     initial soil water potential #*12 initial guess (starting model from the night, should be ap one)

# p_deeproot<-rep(1,Sz)*Pars(9)
#Model controls
W_stem_max<-rep(1,Sz)*60000                     #g          Maximum water content stored in the stem storage compartment (berekend van boom)
W_root_maxTu<-rep(1,Sz)*W_stem_max*p_root     #g         Maximum water content stored in the root storage compartment
W_crown_max<-rep(1,Sz)*W_stem_max*p_crown     #g         Maximum water content stored in the canopy storage compartment
W_root_maxTg<-rep(1,Sz)*W_stem_max*p_deeproot     #g         Maximum water content stored in the root groundwater storage compartment

pauset<-0 #niet gebruiken
update<-50 #om figuur updaten per tijdstap

## Plotting of input files----
DateTime<-data$TimeStamp[limitini:limitend]
DbInput<-data.frame(DateTime,ShortWave,T_air,VPD,R_X_crownair,Psi_soil,SapFlow)
DbInput<-melt(na.omit(DbInput),id="DateTime")
DbInput$DateTime<-strptime(DbInput$DateTime,format="%m/%d/%y %H:%M")
ggplot(DbInput, aes(x=DateTime, y=value,colour=variable)) + 
  geom_line() +
  facet_wrap(~variable, scale="free")+
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("1 hour"))+ 
  xlab("Date time")

## Plotting of inital Xylem Resistances ----

DbRX<-data.frame(DateTime,R_X_crownair,R_X_stemcrown,R_X_rootstem,R_X_ShallowRootDeepRoot)
DbRX<-melt(na.omit(DbRX),id="DateTime")
DbRX$DateTime<-strptime(DbRX$DateTime,format="%m/%d/%y %H:%M")
ggplot(DbRX, aes(x=DateTime, y=value,colour=variable,group=variable)) + 
  geom_line() +
  facet_wrap(~variable, scale="free")+
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("1 hour"))+ 
  xlab("Date time")+
  ylab(expression(paste(MPa," ",s^-g)))
