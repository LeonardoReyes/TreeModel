

source("SetInputs.R")
source("StemDiam.R")
# Running Model--
##Initial Values
i=1

# Pool compartments----
F_stem[i]=F_stem_ini


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

# Water potential Xylem----
Psi_X_stem=(((R_S_stem[i]-(R_S_roots[i]*p_root[i]))/R_S_stem[i])
            *(Psi_S_root[i]-((Psi_S_stem[i]*R_S_roots[i]*p_root[i])/R_S_stem[i])-(F_stem_ini[i]*R_X_rootstem[i])))

f_stem=0.4

f_root=f_stem*p_root

f_rootTg=f_stem*p_deeproot


#     Psi_X_crown[i]=Psi_X_stem[i]-((F_stem[i]-f_stem[i]).*R_X_stemcrown[i])  #Mpa        crown xylem water potential sign error
Psi_X_crown=Psi_X_stem[i]-((F_stem_ini[i]-f_stem[i])*R_X_stemcrown[i])  ##ok<AGROW> #Mpa        crown xylem water potential sign error
#     Psi_X_crown[i]=Psi_X_stem[i]-((F_stem[i]-f_stem[i]).*R_X_stemcrown[i])	#Mpa        crown xylem water potential sign error
# Original
Psi_X_root=Psi_X_stem[i]-((F_soil_ini[i]-f_root[i])*R_X_rootstem[i])              ##ok<AGROW> #Mpa        root xylem water potential

#     Psi_X_root[i]=Psi_X_stem[i]-(F_stem[i]*R_X_rootstem[i])-(F_soil[i]*R_X_ShallowRootDeepRoot[i])              ##ok<AGROW> #Mpa        root xylem water potential
Psi_X_rootTg=Psi_X_root[i]-((F_soilTg_ini[i]-f_rootTg[i])*R_X_ShallowRootDeepRoot[i])              ##ok<AGROW> #Mpa        root xylem water potential

# Storage flow----


f_root=(Psi_X_root[i]-Psi_S_root[i])/R_S_roots[i]      #g/h        flow to root storage compartment
#f_root=p_root[i]*f_stem[i]      #g/h        flow to root storage compartment

#  f_crown=(Psi_X_crown[i]-Psi_X_stem[i])/R_X_stemcrown[i]#g/h        flow to crown storage compartment WRONG: tis is the formula for F_crown, not f_crown
f_crown=p_crown[i]*f_stem[i]                                     #i chose this formula as it is consequent with the above (W_crown=P_crown*W_stem)

f_rootTg=(Psi_X_rootTg[i]-Psi_S_root[i])/R_S_roots[i]      #g/h        flow to root storage compartment
#f_rootTg=p_deeproot[i]*f_stem[i]

# Initial Diameter parameters/Variables----
f_stem=0.4  # g/h flow to stem storage compartment
<<<<<<< HEAD
DiameterOutput<-StemDiam(Psi_S_stem_initial[i],0,i-1,NoPlot)
=======
DiameterOutput<-StemDiam(Psi_S_stem_initial[i],F_stem[i],i-1,NoPlot)
>>>>>>> b8bd689844bd8aae01df693c5a62b8def7fb42ac

# R_S_stem<-DiameterOutput$R_S_stem
# D_inner<<-DiameterOutput$D_inner_output
# D_outer<<-DiameterOutput$D_inner_output
# V_stem<<-DiameterOutput$V_stem_output
R_S_roots=R_S_stem[i]
R_S_roots_sat=R_S_stem[i]

# Xylem Flow (Sap flow)----

 F_soilTg=(Psi_X_stem[i]-R_X_stemcrown[i]
         *(F_stem_ini[i]-f_stem[i])-Psi_air[i])/R_X_crownair[i]+(1+p_crown[i]+p_deeproot[i])*f_stem[i]  #g/h       minus sign wrongly positioned flow from soil to root equation adapted (error in signs)
# F_soil=-((Psi_X_root[i]-Psi_X_stem[i])/R_X_rootstem[i])-f_root[i]
#     F_soil(i+1,:)=F_stem[i]-f_root[i]
#     F_soilTg(i+1,:)=F_soil(i+1,:)-f_rootTg[i]

#     F_crown[i]=(-(Psi_X_crown[i]-Psi_X_stem[i])./R_X_stemcrown[i])+f_crown[i]     ##ok<AGROW> #g/h     no need for minus sign   flow from stem to crown
#     F_stem(i+1,:)=(-(Psi_X_stem[i]-Psi_X_root[i])./R_X_rootstem[i])+f_stem[i] #g/h        flow from root to stem # no need for the minus sign
#     F_soil[i]=(F_stem[i]-f_root[i])
#     F_soilTg(i+1,:)=F_stem(i+1,:)-F_soil(i+1,:)+f_rootTg[i]
#     F_soil[i]=(-(Psi_X_root[i]-Psi_X_rootTg[i])./R_X_ShallowRootDeepRoot[i])+f_root[i]

#WORKING VERSION 18 Jul 2014
F_soil=F_stem_ini[i]-F_soilTg[i]-f_root[i]
E=((Psi_air[i]-Psi_X_crown[i])/R_X_crownair[i])*-1 #g/h        crown transpiration
F_crown=(E[i]-f_crown[i])     #g/h     no need for minus sign   flow from stem to crown
F_stem[i]=F_crown[i]-f_stem[i]

# Run Loops ----
for(i in 2:200){
  if(is.na(Psi_S_stem_p[i-1])){browser()}
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
  
  #Diameter variation Calculation
<<<<<<< HEAD
  DiameterOutput<-StemDiam(Psi_S_stem[i],W_stem[i],i-1,NoPlot)
=======
  DiameterOutput<-StemDiam(Psi_S_stem[i],F_stem[i-1]-F_stem[i],i-1,NoPlot)
>>>>>>> b8bd689844bd8aae01df693c5a62b8def7fb42ac
  
  #   R_S_stem[i]<-DiameterOutput$R_S_stem
  #   D_inner[i]<<-DiameterOutput$D_inner_output
  #   D_outer[i]<<-DiameterOutput$D_inner_output
  #   V_stem[i]<<-DiameterOutput$V_stem_output
  R_S_roots[i]=R_S_stem[i]
  R_S_roots_sat[i]=R_S_stem[i]
  
  #Storage flow
  f_stem[i]=(1/R_S_stem[i])*((((R_S_stem[i]-R_S_roots[i]
                                *p_root[i])/R_S_stem[i])*(Psi_S_root[i]-((Psi_S_stem[i]
                                                                          *R_S_roots[i]*p_root[i])/R_S_stem[i])-F_stem[i-1]
                                                          *R_X_rootstem[i]))-(Psi_S_stem[i]))       
  #g/h   flow to stem storage compartment
  
  f_root[i]=(Psi_X_root[i-1]-Psi_S_root[i])/R_S_roots[i]      #g/h        flow to root storage compartment
  #f_root[i]=p_root[i]*f_stem[i]      #g/h        flow to root storage compartment
  
  #f_crown[i]=(Psi_X_crown[i]-Psi_X_stem[i])/R_X_stemcrown[i]#g/h        flow to crown storage compartment WRONG: tis is the formula for F_crown, not f_crown
  f_crown[i]=p_crown[i]*f_stem[i]                                     #i chose this formula as it is consequent with the above (W_crown=P_crown*W_stem)
  
  f_rootTg[i]=(Psi_X_rootTg[i-1]-Psi_S_root[i])/R_S_roots[i]      #g/h        flow to root storage compartment
  #f_rootTg[i]=p_deeproot[i]*f_stem[i]
  
  # Water potential Xylem---
  Psi_X_stem[i]=(((R_S_stem[i]-(R_S_roots[i]*p_root[i]))/R_S_stem[i])
                 *(Psi_S_root[i]-((Psi_S_stem[i]*R_S_roots[i]*p_root[i])/R_S_stem[i])-(F_stem[i-1]*R_X_rootstem[i])))
  
  #     Psi_X_crown[i]=Psi_X_stem[i]-((F_stem[i]-f_stem[i]).*R_X_stemcrown[i])  #Mpa        crown xylem water potential sign error
  Psi_X_crown[i]=Psi_X_stem[i]-((F_stem[i-1]-f_stem[i])*R_X_stemcrown[i])  ##ok<AGROW> #Mpa        crown xylem water potential sign error
  #     Psi_X_crown[i]=Psi_X_stem[i]-((F_stem[i]-f_stem[i]).*R_X_stemcrown[i])	#Mpa        crown xylem water potential sign error
  # Original
  Psi_X_root[i]=Psi_X_stem[i]-((F_soil[i-1]-f_root[i])*R_X_rootstem[i])              ##ok<AGROW> #Mpa        root xylem water potential
  
  #     Psi_X_root[i]=Psi_X_stem[i]-(F_stem[i]*R_X_rootstem[i])-(F_soil[i]*R_X_ShallowRootDeepRoot[i])              ##ok<AGROW> #Mpa        root xylem water potential
  Psi_X_rootTg[i]=Psi_X_root[i]-((F_soilTg[i-1]-f_rootTg[i])*R_X_ShallowRootDeepRoot[i])              ##ok<AGROW> #Mpa        root xylem water potential
  
  
  # Xylem Flow (i.e.Sap flow) and transpiration---
  

  E[i]=((Psi_air[i]-Psi_X_crown[i])/R_X_crownair[i])*-1 #g/h         transpiration
  F_crown[i]=E[i]-f_crown[i]    #g/h     no need for minus sign
  F_stem[i]=F_crown[i]-f_stem[i]
  F_soilTg[i]=(Psi_X_stem[i]-R_X_stemcrown[i]
             *(F_stem[i-1]-f_stem[i])-Psi_air[i])/R_X_crownair[i]+(1+p_crown[i]+p_deeproot[i])*f_stem[i]  #g/h       minus sign wrongly positioned flow from soil to root equation adapted (error in signs)
#   F_soil[i]=-((Psi_X_root[i]-Psi_X_stem[i])/R_X_rootstem[i])-f_root[i]
  F_soil[i]=F_stem[i]-F_soilTg[i]-f_root[i]
#   #Previous Working Version
#   
#   F_soil[i]=(Psi_X_stem[i]-R_X_stemcrown[i]
#              *(F_stem[i-1]-f_stem[i])-Psi_air[i])/R_X_crownair[i]+(1+p_crown[i]+p_root[i])*f_stem[i]  #g/h       minus sign wrongly positioned flow from soil to root equation adapted (error in signs)
#   E[i]=((Psi_air[i]-Psi_X_crown[i])/R_X_crownair[i])*-1 #g/h        crown transpiration
#   F_crown[i]=(E[i]-f_crown[i])     #g/h     no need for minus sign   flow from stem to crown
#   F_stem[i]=(F_crown[i]-f_stem[i]) #g/h        flow from root to stem # no need for the minus sign
#   F_soilTg[i]=F_stem[i]-F_soil[i]
  
}
##Plotting Results----
<<<<<<< HEAD
=======
#ALL
Output<-data.frame(DateTime=DateTime[2:length(Psi_S_stem_p)],
                   E=E[2:length(Psi_S_stem_p)],F_crown=F_crown[2:length(Psi_S_stem_p)],F_stem=F_stem[2:length(Psi_S_stem_p)],F_soil=F_soil[2:length(Psi_S_stem_p)],
                   f_crown=f_crown[2:length(Psi_S_stem_p)],f_stem=f_stem[2:length(Psi_S_stem_p)],f_root=f_root[2:length(Psi_S_stem_p)],f_rootTg=f_rootTg[2:length(Psi_S_stem_p)],
                   Psi_air=Psi_air[2:length(Psi_S_stem_p)],Psi_X_crown=Psi_X_crown[2:length(Psi_S_stem_p)],Psi_X_stem=Psi_X_stem[2:length(Psi_S_stem_p)],Psi_X_root=Psi_X_root[2:length(Psi_S_stem_p)],Psi_X_rootTg=Psi_X_rootTg[2:length(Psi_S_stem_p)],
                   Psi_S_crown=Psi_S_crown[2:length(Psi_S_stem_p)],Psi_S_stem=Psi_S_stem[2:length(Psi_S_stem_p)],Psi_S_root=Psi_S_root[2:length(Psi_S_stem_p)],Psi_S_rootTg=Psi_S_rootTg[2:length(Psi_S_stem_p)],
                   R_X_crownair=R_X_crownair[2:length(Psi_S_stem_p)],R_X_stemcrown=R_X_stemcrown[2:length(Psi_S_stem_p)],R_X_rootstem=R_X_rootstem[2:length(Psi_S_stem_p)],R_X_soilrootTu=R_X_soilrootTu[2:length(Psi_S_stem_p)],R_X_ShallowRootDeepRoot=R_X_ShallowRootDeepRoot[2:length(Psi_S_stem_p)],
                   R_S_stem=R_S_stem[2:length(Psi_S_stem_p)],R_S_roots=R_S_roots[2:length(Psi_S_stem_p)],R_S_roots_sat=R_S_roots_sat[2:length(Psi_S_stem_p)],
                   D_outer=D_outer[2:length(Psi_S_stem_p)],D_inner=D_inner[2:length(Psi_S_stem_p)],V_stem=V_stem[2:length(Psi_S_stem_p)])

Output<-melt(na.omit(Output),id="DateTime")
Output$DateTime<-strptime(Output$DateTime,format="%m/%d/%y %H:%M")
>>>>>>> b8bd689844bd8aae01df693c5a62b8def7fb42ac


#Xylem Flow

XylemFlow<-data.frame(DateTime=DateTime[2:length(Psi_S_stem_p)],Transpiration=E[2:length(Psi_S_stem_p)],
                      F_crown=F_crown[2:length(Psi_S_stem_p)],F_stem=F_stem[2:length(Psi_S_stem_p)],
                      F_soil=F_soil[2:length(Psi_S_stem_p)],F_soilTg=F_soilTg[2:length(Psi_S_stem_p)],
                      SapFlow=SapFlow[2:length(Psi_S_stem_p)])

XylemFlow<-melt(na.omit(XylemFlow),id="DateTime")
XylemFlow$DateTime<-strptime(XylemFlow$DateTime,format="%m/%d/%y %H:%M")


ggplot(XylemFlow, aes(x=DateTime, y=value,colour=variable,group=variable)) + 
  geom_line() +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("1 hour"))+ 
  xlab("Date time")+
  ylab(expression(paste(cm^3," ",h^-1)))

#Storage Flow

StorageFlow<-data.frame(DateTime=DateTime[2:length(Psi_S_stem_p)],
                      f_crown=f_crown[2:length(Psi_S_stem_p)],f_stem=f_stem[2:length(Psi_S_stem_p)],
                      f_root=f_root[2:length(Psi_S_stem_p)],f_rootTg=f_rootTg[2:length(Psi_S_stem_p)])

StorageFlow<-melt(na.omit(StorageFlow),id="DateTime")
StorageFlow$DateTime<-strptime(StorageFlow$DateTime,format="%m/%d/%y %H:%M")


ggplot(StorageFlow, aes(x=DateTime, y=value,colour=variable,group=variable)) + 
  geom_line() +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("1 hour"))+ 
  xlab("Date time")+
<<<<<<< HEAD
  ylab(expression(paste(cm^3," ",h^-1)))


#ALL Flows
AllFlows<-data.frame(DateTime=DateTime[2:length(Psi_S_stem_p)],Transpiration=E[2:length(Psi_S_stem_p)],
                   E=E[2:length(Psi_S_stem_p)],F_crown=F_crown[2:length(Psi_S_stem_p)],F_stem=F_stem[2:length(Psi_S_stem_p)],F_soil=F_soil[2:length(Psi_S_stem_p)],
                   f_crown=f_crown[2:length(Psi_S_stem_p)],f_stem=f_stem[2:length(Psi_S_stem_p)],f_root=f_root[2:length(Psi_S_stem_p)],f_rootTg=f_rootTg[2:length(Psi_S_stem_p)],
                   SapFlow=SapFlow[2:length(Psi_S_stem_p)])

AllFlows<-melt(na.omit(AllFlows),id="DateTime")
AllFlows$DateTime<-strptime(AllFlows$DateTime,format="%m/%d/%y %H:%M")

ggplot(AllFlows, aes(x=DateTime, y=value,colour=variable)) + 
  geom_line() +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("1 hour"))+ 
  xlab("Date time")

#ALL Storage compartments
StorComp<-data.frame(DateTime=DateTime[2:length(Psi_S_stem_p)],
                     W_crown=W_crown[2:length(Psi_S_stem_p)],W_stem=W_stem[2:length(Psi_S_stem_p)],
                     W_rootTu=W_rootTu[2:length(Psi_S_stem_p)],W_rootTg=W_rootTg[2:length(Psi_S_stem_p)])

StorComp<-melt(na.omit(StorComp),id="DateTime")
StorComp$DateTime<-strptime(StorComp$DateTime,format="%m/%d/%y %H:%M")

ggplot(StorComp, aes(x=DateTime, y=value,colour=variable)) + 
  geom_line() +
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("1 hour"))+ 
  xlab("Date time")

#ALL Diameter Variaition
Diameter<-data.frame(DateTime=DateTime[2:length(Psi_S_stem_p)],
                     D_inner=D_inner[2:length(Psi_S_stem_p)],D_outer=D_outer[2:length(Psi_S_stem_p)])

Diameter<-melt(na.omit(Diameter),id="DateTime")
Diameter$DateTime<-strptime(Diameter$DateTime,format="%m/%d/%y %H:%M")

ggplot(Diameter, aes(x=DateTime, y=value,colour=variable)) + 
  geom_line() +
  facet_wrap(~variable, scale="free")+
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("1 hour"))+ 
  xlab("Date time")

#ALL
Output<-data.frame(DateTime=DateTime[2:length(Psi_S_stem_p)],
                   E=E[2:length(Psi_S_stem_p)],F_crown=F_crown[2:length(Psi_S_stem_p)],F_stem=F_stem[2:length(Psi_S_stem_p)],F_soil=F_soil[2:length(Psi_S_stem_p)],
                   f_crown=f_crown[2:length(Psi_S_stem_p)],f_stem=f_stem[2:length(Psi_S_stem_p)],f_root=f_root[2:length(Psi_S_stem_p)],f_rootTg=f_rootTg[2:length(Psi_S_stem_p)],
                   Psi_air=Psi_air[2:length(Psi_S_stem_p)],Psi_X_crown=Psi_X_crown[2:length(Psi_S_stem_p)],Psi_X_stem=Psi_X_stem[2:length(Psi_S_stem_p)],Psi_X_root=Psi_X_root[2:length(Psi_S_stem_p)],Psi_X_rootTg=Psi_X_rootTg[2:length(Psi_S_stem_p)],
                   Psi_S_crown=Psi_S_crown[2:length(Psi_S_stem_p)],Psi_S_stem=Psi_S_stem[2:length(Psi_S_stem_p)],Psi_S_root=Psi_S_root[2:length(Psi_S_stem_p)],Psi_S_rootTg=Psi_S_rootTg[2:length(Psi_S_stem_p)],
                   R_X_crownair=R_X_crownair[2:length(Psi_S_stem_p)],R_X_stemcrown=R_X_stemcrown[2:length(Psi_S_stem_p)],R_X_rootstem=R_X_rootstem[2:length(Psi_S_stem_p)],R_X_soilrootTu=R_X_soilrootTu[2:length(Psi_S_stem_p)],R_X_ShallowRootDeepRoot=R_X_ShallowRootDeepRoot[2:length(Psi_S_stem_p)],
                   R_S_stem=R_S_stem[2:length(Psi_S_stem_p)],R_S_roots=R_S_roots[2:length(Psi_S_stem_p)],R_S_roots_sat=R_S_roots_sat[2:length(Psi_S_stem_p)],
                   D_outer=D_outer[2:length(Psi_S_stem_p)],D_inner=D_inner[2:length(Psi_S_stem_p)],V_stem=V_stem[2:length(Psi_S_stem_p)])

Output<-melt(na.omit(Output),id="DateTime")
Output$DateTime<-strptime(Output$DateTime,format="%m/%d/%y %H:%M")

ggplot(Output, aes(x=DateTime, y=value,colour=variable)) + 
  geom_line() +
  facet_wrap(~variable, scale="free")+
  scale_x_datetime(breaks = date_breaks("1 day"),
                   minor_breaks = date_breaks("1 hour"))+ 
  xlab("Date time")
=======
  ylab(expression(paste(cm^3," ",h^-1)))
>>>>>>> b8bd689844bd8aae01df693c5a62b8def7fb42ac
