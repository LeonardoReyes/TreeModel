LowGamma<-function(time,y,parms){
  Rho_w=1000000
  Phi=2.34*10^-7  #1/(Mpa.s)  Extensibility of cell walls in relation to non-reversible dimensional changes (water storage)
  Gamma=0.9  #Mpa	Critical value for the pressure component (Psi_S_stem_p) which must be exceeded to produce (positive) growth in the storage compartment
  a=0.002968	#m	allometric parameters according to Génard et al. (2001)
  b=32       #1/m	allometric parameters according to Génard et al. (2001)
  l=3
  
  with(as.list(c(y, parms)), {
    #deriv(DPsi_S_stem_p)
    dy1<-Psi_S_stem_p*F_stem*y[2]/(Rho_w*V_stem)
    
    #deriv( D_outer ) 
    dy2<- 2.0 * S * y[1] /
      (Epsilon_0 *Psi_S_stem_p * D_outer )
    
    #deriv(dS)
    dy3<-a*b*(exp(-b*D_outer))*y[2]
    
    #deriv(#D_inner)
    dy4<-y[2] - (2.0 * y[3])
    list(as.numeric(c(dy1,dy2,dy3,dy4)))
  })
  
  
}