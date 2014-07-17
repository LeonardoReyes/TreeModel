StemDiam<-function(Psi_S_stem_initial,f_stem,t,Noplot){
  source("HighGamma.R")
  source("LowGamma.R")
  
  #Parameter----
  l=3           #m  length of the stem segment
  L=8.38*10^-8  #m/(Mpa.s)	radial hydraulic conductivity of the virtual membrane separating the stem storage compartment from the xylem compartment
  a=0.035012051	#m	allometric parameters according to Solver optimisation for Sardon data
  b=33.22633862 #1/m allometric parameters according to Solver optimisation for Sardon data
  # a=0.002968	#m	allometric parameters according to Génard et al. (2001)
  # b=32        #1/m	allometric parameters according to Génard et al. (2001)
  Gamma=0.9	    #Mpa	Critical value for the pressure component (Psi_S_stem_p) which must be exceeded to produce (positive) growth in the storage compartment
  Epsilon_0=1100	#1/m	Proportionality constant
  Phi=2.34*10^-7	#1/(Mpa.s)	Extensibility of cell walls in relation to non-reversible dimensional changes (water storage)
  
  ##variables conecting the storage model with the tree model
  # Psi_S_stem=0.093866933697054
  # W_stem_ini=3000
  
  
  # Constant----
  Rho_w=1000000	#g/m³ density of water
  
  # Variables
  ## Calc S
  
  # if t==65
  #     keyboard
  # end
  
  if(t==0){
    D_outer_ini<<-0.24
    Epsilon<<-Epsilon_0
    Psi_S_stem_p_0<<-2.9     # Mpa Apple trees well watered Needs to be calibrated
#     Psi_S_stem_p_0=-30     # Mpa Apple trees well watered Needs to be calibrated
    #Psi_S_stem_p_0=100
    
    S <<- a*(1-exp(-b*D_outer_ini))
    D_inner<<-D_outer_ini-(2*(S))
    V_stem <<- pi * S * D_inner * l                      #m³  volume of the stem storage compartment
    Psi_S_stem_p<<-0.6*(Psi_X_stem-Psi_S_stem_p_0)
    D_outer<<-D_outer_ini
    #     Psi_S_stem_o = Psi_S_stem_p - Psi_S_stem ASK MAURITS becaus it is not used!!!!!
  }

  
  if(t==0){
    timeinterval=seq(from=0,to=t+1,by=0.1)
    if(Psi_S_stem_p > Gamma){
      yini<-c(y1=0.0000001,y2=0.0000001,y3=0.0000001,y4=0.0000001)
      parms<-c(Epsilon_0=Epsilon,f_stem=f_stem,Psi_S_stem_p=Psi_S_stem_p,S=S,V_stem=V_stem,D_outer=D_outer)
      times<-timeinterval
      Diameter <- rk (times = times, y = yini, func = HighGamma, parms = parms,rtol = 1e-6, atol = 1e-6)
      
      Psi_S_stem_p<<-Psi_S_stem_p+Diameter[nrow(Diameter),2]
      D_outer[t+1]<<-D_outer+Diameter[nrow(Diameter),3]
      S[t+1]<<-S+Diameter[nrow(Diameter),4]
      D_inner[t+1]<<-D_inner+Diameter[nrow(Diameter),5]
    }else{
      yini<-c(y1=0.0000001,y2=0.0000001,y3=0.0000001,y4=0.0000001)
      parms<-c(Epsilon_0=Epsilon,f_stem=f_stem,Psi_S_stem_p=Psi_S_stem_p,S=S,V_stem=V_stem,D_outer=D_outer)
      times<-timeinterval
      Diameter <- rk (times = times, y = yini, func = LowGamma, parms = parms,rtol = 1e-6, atol = 1e-6)
      
      Psi_S_stem_p<<-Psi_S_stem_p+Diameter[nrow(Diameter),2]
      D_outer[t+1]<<-D_outer+Diameter[nrow(Diameter),3]
      S[t+1]<<-S+Diameter[nrow(Diameter),4]
      D_inner[t+1]<<-D_inner+Diameter[nrow(Diameter),5]    }
  }
  else{
    timeinterval=seq(from=t,to=t+1,by=0.1)
    if(Psi_S_stem_p[t] > Gamma){
      yini<-c(y1=0.0000001,y2=0.0000001,y3=0.0000001,y4=0.0000001)
      parms<-c(Epsilon_0=Epsilon,f_stem=f_stem,Psi_S_stem_p=Psi_S_stem_p[t],S=S[t],V_stem=V_stem[t],D_outer=D_outer[t])
      times<-timeinterval
      Diameter <- rk (times = times, y = yini, func = HighGamma, parms = parms,rtol = 1e-6, atol = 1e-6)
      
      Psi_S_stem_p[t+1]<<-Psi_S_stem_p+Diameter[nrow(Diameter),2]
      D_outer[t+1]<<-D_outer[t]+Diameter[nrow(Diameter),3]
      S[t+1]<<-S[t]+Diameter[nrow(Diameter),4]
      D_inner[t+1]<<-D_inner[t]+Diameter[nrow(Diameter),5]
      
      }else{
      yini<-c(y1=0.0000001,y2=0.0000001,y3=0.0000001,y4=0.0000001)
      parms<-c(Epsilon_0=Epsilon,f_stem=f_stem,Psi_S_stem_p=Psi_S_stem_p[t],S=S[t],V_stem=V_stem[t],D_outer=D_outer[t])
      times<-timeinterval
      Diameter <- rk (times = times, y = yini, func = LowGamma, parms = parms,rtol = 1e-6, atol = 1e-6)
      
      Psi_S_stem_p[t+1]<<-Psi_S_stem_p+Diameter[nrow(Diameter),2]
      D_outer[t+1]<<-D_outer[t]+Diameter[nrow(Diameter),3]
      S[t+1]<<-S[t]+Diameter[nrow(Diameter),4]
      D_inner[t+1]<<-D_inner[t]+Diameter[nrow(Diameter),5]
      
    }
  }

  
  # save('Diameter.mat','Diameter')
  # if Psi_S_stem_p > Gamma
  #     p=[Epsilon f_stem V_stem D_outer Psi_S_stem_p S]
  #     options=odeset('RelTol',1e-6)
  #     [timdiff,Diameter]=ode45(@HighGamma,timeinterval,[0.0000001 0.0000001 0.0000001 0.0000001],options,p)
  #     Diameter=sum(Diameter)
  #     Psi_S_stem_p<<-Psi_S_stem_p+Diameter(end,2)
  #     D_outer<-D_outer+Diameter(end,1)
  #     S=S+Diameter(end,3)
  #     D_inner=D_inner+Diameter(end,4)
  #     #     V_stem=Diameter(end,5)
  # else
    #     p=[Epsilon f_stem V_stem D_outer Psi_S_stem_p S]
  #     options=odeset('RelTol',1e-6)
  #     [timdiff,Diameter]=ode45(@LowGamma,timeinterval,[0.0000001 0.0000001 0.0000001 0.0000001],options,p)
  #     Diameter=sum(Diameter)
  #     D_outer<-D_outer+Diameter(end,2)
  #     Psi_S_stem_p<<-Psi_S_stem_p+Diameter(end,1)
  #     S=S+Diameter(end,3)
  #     D_inner=D_inner+Diameter(end,4)
  #     #    D_inner=Diameter(end,4)
  #     #     V_stem=Diameter(end,5)
  # end
  
  # Epsilon = Epsilon_0* Psi_S_stem_p * D_outer
if(t==0){
  A = pi* D_inner * l
  R_S_stem[t+1] <<- 1.0 / ( A * Rho_w * L )
  V_stem [t+1]<<-pi * S * D_inner * l
}else{
  A = pi* D_inner[t] * l
  R_S_stem[t+1] <<- 1.0 / ( A * Rho_w * L )
  V_stem [t+1]<<- pi* S[t] * D_inner[t] * l
}
  
#   if(isnan(V_stem)|| (D_outer-D_inner)<0|| V_stem<0){
#     #     keyboard
#     #     D_inner=D_outer
#     #     A = pi* D_inner * l
#     #     R_S_stem = 1.0 / ( A * Rho_w * L )
#     #     V_stem = pi * S * D_inner * l
#     D_outer=NaN
#     D_inner=NaN
#   }
  
  if(!NoPlot==1){
    # figure(3)
    #
    # # subplot(2,1,1)
    # hold on
    # plot(t,[D_inner D_outer],'*')
    # legend({'Diameter inner' 'Diameter outer'})
    # hold off
  }
  
#   D_outer_output=D_outer
#   D_inner_output=D_inner
#   V_stem_output=V_stem
#   
#   return(data.frame(R_S_stem,D_outer_output,D_inner_output,V_stem_output))
}