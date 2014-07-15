# Tree model Maurits Leonardo
#Parameters ----

## Loading data----
data<-read.csv('JuneWET.csv',header=T,skip=1)
sapdata<-read.csv('SAP_QI_MOD.csv',header=T,skip=1)
soildata<-read.csv('Soil_DATA_QI.csv',header=T,skip=2)

## Input Varaibles----
Neg<-1 #Negativity of water potential from soil

limitini<-1 #range of dataset
limitend<-200 #range of dataset (max 1260)
F_stem<-sapdata$SapNN.Qi[limitini:limitend] #g/s        stem sap flux from data files
F_stem[F_stem<0.001]<-0.01 #negative values equal to zero (kwam niet voor in periode)
Sz<-limitend # size data set
SapFlow<-F_stem # g/hour
ShortWave<-data$Shortwave.Incomming.Radiation[limitini:limitend]       #Watt m^2   Short Wave incomming solar radiation
ShortWave[ShortWave<0]<-0
T_air<-data$Air.Temperature[limitini:limitend]#°C         air temperature
RH<-data$Relative.humidity[limitini:limitend] # #         relative humidity
VPD<-data$VPD[limitini:limitend]              # kPa       Vapour presssure defficit
VPD[VPD<0]<-0
range<-max(VPD)-min(VPD)
NormVPD<-(VPD-min(VPD))./range
LabelTime<-datestr(data(limitini:limitend,1)+ datenum('30-Dec-1899'))     # Label Times
F_stem_ini<-0
F_soil_ini<-F_stem_ini(1)
F_soilTg_ini<-F_stem_ini(1)

# Psi_soil<-soildata(limitini:limitend,3)./10.*Neg  #MPa        soil matrix potential
# Psi_soil<-ones(Sz).*0.875                 #MPa        soil matrix potential
if isempty(Psi_soil)|| ~exist('Psi_soil','var')
Psi_soil<-10.^Pfextrap(soildata(limitini:limitend,8),4).*0.00009807.*Neg #derived from pF dataset
end
# Curve 1 <- 23_28 cm depth
# Curve 2 <- 48_53 cm depth
# Curve 3 <- 73_78 cm depth
# Curve 4 <- 98_103 cm depth
# Curve 5 <- 123_128 cm depth


# Constants
R<-ones(Sz).*8.314        #J/(mol*K)  Universal gas constant
Vw_0<-ones(Sz).*18  #m3/mol     molar volume of water

if NoPlot<-<-1
else
  figure(1)
subplot(2,2,1)
plot(ShortWave,'g')
legend({'Short wave incomming'})
ylabel('Watt m^{-2}')
xlabel('hours')
xlim([1 500])
subplot(2,2,2)
plot(T_air,'r')
legend({'Air temp (C)'})
ylabel('C^o')
xlabel('hours')
xlim([1 500])
subplot(2,2,3)
plot(VPD,'c')
legend({'VPD'})
ylabel('kPa')
xlabel('hours')
xlim([1 500])
subplot(2,2,4)
plot(smooth(Psi_soil))
ylabel('MPa')
xlabel('hours')
xlim([1 500])
legend({'\psi soil'})
latex_fig(12, 3.1, 3)


end
## Initial Resistance Guess

# Parameters
ChangeRes<-50000     ## Opt Val 1 100000
ChangeRoot<-5000 ## Opt Val 0.01 1000
ChangeRoot_Stem<-1000## Opt Val 0.02 2000
ChageStem_Crown<-5000## Opt Val 0.01 1000
if Opt<-<-1
R_X_crownair<-ones(Sz).*Pars(1)
#R_X_crownair<-FourierForResistance(limitend,Pars(1))
R_X_stemcrown<-ones(Sz).*Pars(2)
R_X_rootstem<-ones(Sz).*Pars(3)
R_X_soilrootTu<-ones(Sz).*0.4693
R_X_ShallowRootDeepRoot<-ones(Sz).*Pars(4)

#     R_X_crownair<-FourierForResistance(limitend,[Pars(1) Pars(2)])   #Mpa.s/g    xylem resistance between crown and air
#     #R_X_stemcrown<-ones(Sz).*0.5673  #Mpa.s/g    xylem resistance between stem and crown
#     R_X_stemcrown<-FourierForResistance(limitend,[Pars(3) Pars(4)])
#     # R_X_rootstem<-ones(Sz).*0.1618   #Mpa.s/g    xylem resistance between root and stem
#     #R_X_rootstem<-ones(Sz).*Pars(4)*0.095   #Mpa.s/g    xylem resistance between root and stem
#     R_X_rootstem<-FourierForResistance(limitend,[Pars(5) Pars(6)])   #Mpa.s/g    xylem resistance between root and stem
#     R_X_soilrootTu<-ones(Sz).*0.4693   #Mpa.s/g    xylem resistance between soil and root
#     #R_X_ShallowRootDeepRoot<-ones(Sz).*Pars(4).*0.15   #Mpa.s/g    xylem resistance between soil and root
#     R_X_ShallowRootDeepRoot<-FourierForResistance(limitend,[Pars(7) Pars(8)])   #Mpa.s/g    xylem resistance between soil and root


#     R_X_crownair<-FourierForResistance(limitend,ParVec(1))
#     R_X_stemcrown<-ones(Sz).*ParVec(2)
#     R_X_rootstem<-ones(Sz).*ParVec(3)
#     R_X_soilroot<-ones(Sz).*ParVec(4)
else
  #     R_X_crownair<-(NormVPD.*9)-1   #Mpa.s/g    xylem resistance between crown and air
# Previous good value R_X_crownair<-FourierForResistance(limitend,200)'.*NormVPD

#     R_X_crownair<-NormVPD.*Pars(1)   #Mpa.s/g    xylem resistance between crown and air

R_X_crownair<-NormResistance(Pars(1)-5,Pars(1),NormVPD,range)   #Mpa.s/g    xylem resistance between crown and air

#R_X_crownair<-FourierForResistance(limitend,Pars)'.*NormVPD   #Mpa.s/g    xylem resistance between crown and air
#R_X_stemcrown<-ones(Sz).*0.5673  #Mpa.s/g    xylem resistance between stem and crown
R_X_stemcrown<-NormResistance(Pars(2)-1,Pars(2),NormVPD,range)
# R_X_rootstem<-ones(Sz).*0.1618   #Mpa.s/g    xylem resistance between root and stem
#R_X_rootstem<-ones(Sz).*Pars(4)*0.095   #Mpa.s/g    xylem resistance between root and stem
R_X_rootstem<-NormResistance(Pars(3)-0.5,Pars(3),NormVPD,range)   #Mpa.s/g    xylem resistance between root and stem
R_X_soilrootTu<-ones(Sz).*0.4693   #Mpa.s/g    xylem resistance between soil and root
#R_X_ShallowRootDeepRoot<-ones(Sz).*Pars(4).*0.15   #Mpa.s/g    xylem resistance between soil and root
R_X_ShallowRootDeepRoot<-NormResistance(Pars(4)-0.5,Pars(4),NormVPD,range)   #Mpa.s/g    xylem resistance between soil and root
#     R_X_crownair<-ones(Sz).*(0.0001829.*ChangeRes)   #Mpa.s/g    xylem resistance between crown and air
#     R_X_stemcrown<-ones(Sz).*(0.0001829*ChageStem_Crown)  #Mpa.s/g    xylem resistance between stem and crown
#     R_X_rootstem<-ones(Sz).*(0.0001829*ChangeRoot_Stem)   #Mpa.s/g    xylem resistance between root and stem
#     R_X_soilroot<-ones(Sz).*(0.0001829*ChangeRoot)   #Mpa.s/g    xylem resistance between soil and root
# Psi_S_stem_p_0<-NormResistance(Pars(5)-10,Pars(5),NormVPD,range)
# Psi_S_stem_p_0<-Pars(5)
end
# figure(3)
# plot([R_X_crownair R_X_stemcrown R_X_rootstem R_X_ShallowRootDeepRoot])
## Guess from Tyree and Zimmermann
# Parameters
# R_X_crownair<-ones(Sz).*6.0*10^-9   #Mpa.s/g    xylem resistance between crown and air
# R_X_stemcrown<-ones(Sz).*6.0*10^-9*0.25  #Mpa.s/g    xylem resistance between stem and crown
# R_X_rootstem<-ones(Sz).*6.0*10^-9*0.25   #Mpa.s/g    xylem resistance between root and stem
# R_X_soilroot<-ones(Sz).*6.0*10^-9   #Mpa.s/g    xylem resistance between soil and root
# R_S_stem<-ones(Sz).*0.1829       #Mpa.s/g    xylem resistance between stem xylem and storage compartment
# R_S_roots<-ones(Sz).*(0.0001829)      #Mpa.s/g    xylem resistance between root xylem and storage compartment

# Cper<-360
# if Opt<-<-1
#     Cper<-Pars(5)
# else
  #     Cper<-Pars(5)
#     #Cper<-503 Optimized value for best results on 01 July 2012
# end
#1 Steppe
#109 Guess for Q.i.

# C<-Pars(5) ## Original value 0.2123 Cper dummy variable for calibration
# C2<-Pars(6)  ## Original value 0.12
# C3<-Pars(7)  ## Originalvalue 1.1576

# C<-0.2123 ## Original value 0.2123
# C2<-0.121  ## Original value 0.12
# C3<-1.1576  ## Originalvalue 1.1576

C<-0.21 ## Original value 0.2123
C2<-0.12  ## Original value 0.12
C3<-1.1576  ## Originalvalue 1.1576


# C<-Pars(8) ## Original value 0.2123
# C2<-Pars(9)  ## Original value 0.12
# C3<-Pars(10)  ## Originalvalue 1.1576



C_stem<-ones(Sz).*C         #g/Mpa      stem capacitance after De Pauw et al. (2008)
C_root<-ones(Sz).*C2          #g/Mpa      root capacitance Initial guess
C_crown<-ones(Sz).*C3        #g/Mpa      crown capacitance
C_rootTg<-ones(Sz).*C2.*0.25       #g/Mpa      Deep root capacitance Initial guess
# C_rootTg<-ones(Sz).*C2.*0.01       #g/Mpa      Deep root capacitance Initial guess

p_root<-ones(Sz).*0.1           #frac     percernage of stem water in roots
p_crown<-ones(Sz).*0.4          #frac     percernage of stem water in crown
p_deeproot<-ones(Sz).*0.05           #frac     percernage of stem water in deep roots

# p_root<-ones(Sz).*0.0000000001           #frac     percernage of stem water in roots
# p_crown<-ones(Sz).*0.0000000001          #frac     percernage of stem water in crown
# p_deeproot<-ones(Sz).*0.000000001           #frac     percernage of stem water in deep roots

# p_root<-ones(Sz).*Pars(8)           #frac     percernage of stem water in roots
# p_root<-ones(Sz).*Pars(7)           #frac     percernage of stem water in roots
# p_crown<-ones(Sz).*Pars(8)          #frac     percernage of stem water in crown

Psi_X_initial<-Psi_soil(1,1)    #Mpa      initial soil water potential
Psi_S_stem_initial<-Psi_soil(1,1)*0#Mpa     initial soil water potential #*1.2 initial guess (starting model from the night, should be ap one)
Psi_X_stem_initial<-Psi_soil(1,1)*0#Mpa     initial soil water potential #*1.2 initial guess (starting model from the night, should be ap one)

# p_deeproot<-ones(Sz).*Pars(9)
#Model controls
W_stem_max<-ones(Sz).*60000                     #g          Maximum water content stored in the stem storage compartment (berekend van boom)
W_root_maxTu<-ones(Sz).*W_stem_max.*p_root     #g         Maximum water content stored in the root storage compartment
W_crown_max<-ones(Sz).*W_stem_max.*p_crown     #g         Maximum water content stored in the canopy storage compartment
W_root_maxTg<-ones(Sz).*W_stem_max.*p_deeproot     #g         Maximum water content stored in the root groundwater storage compartment

pauset<-0 #niet gebruiken
update<-50 #om figuur updaten per tijdstap