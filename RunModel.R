

source("SetInputs.R")
source("StemDiam.R")
# Running Model--
##Initial Values
i=1

# STORAGE----
F_stem[i]=0


W_stem=(Psi_X_stem_initial[i]*C_stem[i])+W_stem_max[i]   #g  Water content stored in the stem storage compartment

W_rootTu=((Psi_X_stem_initial[i]-F_stem_ini[i])
             *R_X_rootstem[i]* C_root[i])+W_root_maxTu[i]  #g Water content stored in the root storage compartment

W_rootTg=((Psi_X_stem_initial[i]
              -(F_stem_ini[i]-F_soil_ini[i]
                *R_X_ShallowRootDeepRoot[i]))
             *C_rootTg[i])+W_root_maxTg[i]  #g Water content stored in the root storage compartment

W_crown=p_crown[i]*W_stem[i]

# Water Potential air and Storage----
Psi_air=(R[i]*(T_air[i]+273.15)/Vw_0[i])*log(RH[i]/100)  ##ok<*SAGROW> #Mpa    air water potential
Psi_S_root= (W_rootTu[i] - W_root_maxTu[i])/C_root[i]
Psi_S_rootTg= (W_rootTg[i] - W_root_maxTg[i])/C_rootTg[i]#Mpa    root storage water potential
Psi_S_crown=(W_crown[i] - W_crown_max[i])/C_crown[i]         #Mpa    crown storage water potential
Psi_S_stem=(W_stem[i] - W_stem_max[i])/C_stem[i] 

# STORAGE flow----
f_stem=0.4

#     f_root[i]=(Psi_X_root[i]-Psi_S_root[i])./R_S_roots[i]      #g/h        flow to root storage compartment
f_root=p_root[i]*f_stem[i]      #g/h        flow to root storage compartment

#  f_crown[i]=(Psi_X_crown[i]-Psi_X_stem[i])./R_X_stemcrown[i]#g/h        flow to crown storage compartment WRONG: tis is the formula for F_crown, not f_crown
f_crown=p_crown[i]*f_stem[i]                                     #i chose this formula as it is consequent with the above (W_crown=P_crown*W_stem)

#     f_rootTg[i]=(Psi_X_rootTg[i]-Psi_S_root[i])./R_S_roots[i]      #g/h        flow to root storage compartment
f_rootTg=p_deeproot[i]*f_stem[i]

# Water potential Xylem----
Psi_X_stem=(((R_S_stem[i]-(R_S_roots[i]*p_root[i]))/R_S_stem[i])
                 *(Psi_S_root[i]-((Psi_S_stem[i]*R_S_roots[i]*p_root[i])/R_S_stem[i])-(F_stem_ini[i]*R_X_rootstem[i])))

#     Psi_X_crown[i]=Psi_X_stem[i]-((F_stem[i]-f_stem[i]).*R_X_stemcrown[i])  #Mpa        crown xylem water potential sign error
Psi_X_crown=Psi_X_stem[i]-((F_stem_ini[i]-f_stem[i])*R_X_stemcrown[i])	##ok<AGROW> #Mpa        crown xylem water potential sign error
#     Psi_X_crown[i]=Psi_X_stem[i]-((F_stem[i]-f_stem[i]).*R_X_stemcrown[i])	#Mpa        crown xylem water potential sign error
# Original
Psi_X_root=Psi_X_stem[i]-((F_soil_ini[i]-f_root[i])*R_X_rootstem[i])              ##ok<AGROW> #Mpa        root xylem water potential

#     Psi_X_root[i]=Psi_X_stem[i]-(F_stem[i]*R_X_rootstem[i])-(F_soil[i]*R_X_ShallowRootDeepRoot[i])              ##ok<AGROW> #Mpa        root xylem water potential
  Psi_X_rootTg=Psi_X_root[i]-((F_soilTg_ini[i]-f_rootTg[i])*R_X_ShallowRootDeepRoot[i])              ##ok<AGROW> #Mpa        root xylem water potential

# Xykwm Flow (Sap flow)----

F_soil=(Psi_X_stem[i]-R_X_stemcrown[i]
           *(F_stem_ini[i]-f_stem[i])-Psi_air[i])/R_X_crownair[i]+(1+p_crown[i]+p_root[i])*f_stem[i]  #g/h       minus sign wrongly positioned flow from soil to root equation adapted (error in signs)
#     F_soil(i+1,:)=F_stem[i]-f_root[i]
#     F_soilTg(i+1,:)=F_soil(i+1,:)-f_rootTg[i]

#     F_crown[i]=(-(Psi_X_crown[i]-Psi_X_stem[i])./R_X_stemcrown[i])+f_crown[i]     ##ok<AGROW> #g/h     no need for minus sign   flow from stem to crown
  #     F_stem(i+1,:)=(-(Psi_X_stem[i]-Psi_X_root[i])./R_X_rootstem[i])+f_stem[i] #g/h        flow from root to stem # no need for the minus sign
#     F_soil[i]=(F_stem[i]-f_root[i])
#     F_soilTg(i+1,:)=F_stem(i+1,:)-F_soil(i+1,:)+f_rootTg[i]
#     F_soil[i]=(-(Psi_X_root[i]-Psi_X_rootTg[i])./R_X_ShallowRootDeepRoot[i])+f_root[i]
F_soilTg=F_stem_ini[i]-F_soil[i]-f_rootTg[i]

# Initial water resistance
f_stem=0.4  # g/h flow to stem storage compartment
DiameterOutput<-StemDiam(Psi_S_stem_initial[i],f_stem[i],i-1,NoPlot)

R_S_stem<-DiameterOutput$R_S_stem
D_inner<-DiameterOutput$D_inner_output
D_outer<-DiameterOutput$D_inner_output
V_stem<-DiameterOutput$V_stem_output


#                  R_S_stem=ones(Sz).*5.4053.*5
R_S_roots=R_S_stem[i]/2
R_S_roots_sat=R_S_stem[i]/5

# Run Loops ----
for(i in 2:200){
  
  W_stem[i]=(Psi_X_stem[i-1]* C_stem[i])+ W_stem_max[i]  
  #g  Water content stored in the stem storage compartment
  
  W_rootTu[i]=((Psi_X_stem[i-1]-(F_stem[i-1]
                                 *R_X_rootstem[i]))* C_root[i]) + W_root_maxTu[i] 
  #g Water content stored in the root storage compartment
  
  W_rootTg[i]=((Psi_X_stem[i-1]-(F_stem[i-1]-F_soil[i-1]
                                 *R_X_ShallowRootDeepRoot[i]))*C_rootTg[i]) + W_root_maxTg[i] 
  #g Water content stored in the deep root storage compartment
  W_crown[i]=p_crown[i]*W_stem[i]
  
  Psi_air[i]=(R[i]*(T_air[i]+273.15)/Vw_0[i])*log(RH[i]/100)  
  ##Mpa    air water potential
  
  Psi_S_root[i]= (W_rootTu[i] - W_root_maxTu[i])/C_root[i]
  Psi_S_rootTg[i]= (W_rootTg[i] - W_root_maxTg[i])/C_rootTg[i]
  #Mpa    root storage water potential
  
  Psi_S_crown[i]=(W_crown[i] - W_crown_max[i])/C_crown[i]         
  #Mpa    crown storage water potential
  
  Psi_S_stem[i]=(W_stem[i] - W_stem_max[i])/C_stem[i] 
  #Mpa    stem storage water potential
}
