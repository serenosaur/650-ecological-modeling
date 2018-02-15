
#this line installs the deSolve package in case you don't already have it, and print() tells you that's what it's doing
if (require(deSolve) == F) {install.packages('deSolve',repos='http://cran.r-project.org');if (require(deSolve) == F) print ('Error: deSolve package is not installed on your machine')}

#the following lines create a function called model(). 
#model() requires t, Y and parameters, and it sends back a list of dNH and dSH values.

model<-function(t,Y,parameters,...) { 
  #saves the fed-in t variable as Time. The <<- makes Time available outside of the function and in the console
	Time <<- t
	#takes the fed-in Y, pulls out NH, sets it to NH inside the function
	NH <- Y['NH']
	#takes the fed-in Y, pulls out SH, sets it to SH inside the function
	SH <- Y['SH']
	#takes the fed-in parms, pulls out beta_1, sets it to beta_1 inside the function
	beta_1 <- parameters['beta_1']
	#takes the fed-in parms, pulls out beta_1, sets it to beta_1 inside the function
	alpha_1 <- parameters['alpha_1']

	
	N_outflow <-  NH * beta_1
	SN_flow <-  SH * alpha_1
	NS_flow <-  NH * alpha_1
	N_inflow <-  0.923077 * 4.33e11
	S_outflow <-  SH * beta_1
	S_inflow <-  0.076923 * 4.33e11


  ###------ The following lines have not been translated!!
	#--- NH_ethane_conc = (NH/0.9e20)*1e12

	#--- net_flow = NS_flow-SN_flow

	#--- SH_ethane_conc = (SH/0.9e20)*1e12

  ###-------------

	 dNH = SN_flow  + N_inflow  - NS_flow  - N_outflow 
	 dSH = NS_flow  + S_inflow  - SN_flow  - S_outflow 
	 list(c(dNH, dSH))
}

##############################################
##############################################
#this line brings in and assigns the values from the Stella ethane model
parms <- c(beta_1 = 2.67, alpha_1 = 0.36)
#creates a vector for the starting values of NH and SH. Saved into Y in preparation for 
#feeding these values into the model (defined above)
Y <- c(NH = 0, SH = 0)

source('emod_functions.R')
#this pulls in the emod_functions list, but doesn't actually call any of the functions from it.
#guess it's done automatically by StellaR, in case the model they have needs any of those functions?

#defines the time interval = DT, and seq() generates a list of values according to DT
DT <- 0.25
time <- seq(0.001,100,DT)
#This portion feeds in the variables that the model() function needs. Then it takes
#the output and feeds that into ode(), which is a general solver for Ordinary 
#Differential Equations. This outputs a deSolve matrix with the real values of interest!
out <- ode(func=model,y=Y,times=time,parms=parms,method='rk4')
windows(width=5, height=5)
plot(out, ylab="time")


#This plots the results on two graphs. plot() has built-in defaults when it comes to
#plotting objects of different sizes and types.
plot(out, ylab="time")

######################################


#let's try this with model2 (see above)


#let's try to rewrite the function, this time to send out the concs!
model2<-function(t,Y,parameters,...) { 
  #saves the fed-in t variable as Time. The <<- makes Time available outside of the function and in the console
  Time <<- t
  #takes the fed-in Y, pulls out NH, sets it to NH inside the function
  NH <- Y['NH']
  #takes the fed-in Y, pulls out SH, sets it to SH inside the function
  SH <- Y['SH']
  #takes the fed-in parms, pulls out beta_1, sets it to beta_1 inside the function
  beta_1 <- parameters['beta_1']
  #takes the fed-in parms, pulls out beta_1, sets it to beta_1 inside the function
  alpha_1 <- parameters['alpha_1']
  
  
  N_outflow <-  NH * beta_1
  SN_flow <-  SH * alpha_1
  NS_flow <-  NH * alpha_1
  N_inflow <-  0.923077 * 4.33e11
  S_outflow <-  SH * beta_1
  S_inflow <-  0.076923 * 4.33e11
  
  
  ###------ The following lines have not been translated!!
  #--- NH_ethane_conc = (NH/0.9e20)*1e12
  
  #--- net_flow = NS_flow-SN_flow
  
  #--- SH_ethane_conc = (SH/0.9e20)*1e12
  
  ###-------------
  NH_ethane_conc <- (SN_flow  + N_inflow  - NS_flow  - N_outflow/0.9e20)*1e12
  SH_ethane_conc <- (NS_flow  + S_inflow  - SN_flow  - S_outflow/0.9e20)*1e12
  #dNH = SN_flow  + N_inflow  - NS_flow  - N_outflow 
  #dSH = NS_flow  + S_inflow  - SN_flow  - S_outflow 
  
  list(c(NH_ethane_conc, SH_ethane_conc))
  
}

###### IT BREAKS HERE#####
out2 <- c(model2(Y=Y, t=time, parameters=parms))
#ode(func=model2,y=Y,times=time,parms=parms,method='rk4')

#can it work with our model2?
plot(out2, ylab="time")
