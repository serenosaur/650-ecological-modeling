#chapter 2 case studies
#example on page 63: making sense ot ouf mathematical formulations

#sets variables used in carrying capacity equation
#intrinsic rate of increase/day
r<-0.1
#carrying capacity
K<-10
#opens a new window
#not necessary in Rstudio
windows(width=5, height=5)
#curve() draws a curve corresponding to the expression
#a function over the interval of [from,to] 
curve (expr =r*x*(1-x/K), from=0, to=20, lwd=2, xlab="density,N", ylab="rate of change, dN/dt")
#adds a legend to the corner of the plot 
legend ("topright", c("K=10", "r=0.1"))

#"expr" can be the name of a function, a call,
#or an expression written as a function o fx

#let's try writing an easy function
#x is the intake 
f1 <- function(x){
  #this shows what we do to x once it's taken in
  y <- x*10
  #y is the output
  y
}
#now let's try writing the carrying capacity function
carrycap <- function(x,r,K){
  z <- r*x*(1-x/K)
  z
}
#call the function with different X values
#compare them to the previous graph
carrycap(20,r,K)

#now let's use that function to remake the graph. Should be exactly the same.
curve (expr=carrycap(x,r,K), from=0, to=20, lwd=2, xlab="density,N", ylab="rate of change, dN/dt")
#let's try to remake the legend so it changes when r and K change
legend ("topright", c(paste("K=",K), paste("r=",r)))

#let's test the above code with new r and K values. Run all 4 of the following lines at once.
#note: we don't have to change the carrycap function because it takes in whatever values
#are stored in the r and K variables, which we're redefining below.
r<-0.2
K<-15
curve (expr=carrycap(x,r,K), from=0, to=20, lwd=2, xlab="density,N", ylab="rate of change, dN/dt")
legend ("topright", c(paste("K=",K), paste("r=",r)))

#####################
#example on pg 64: one formula, several parameter values
#the previous example only used 1 set of parameters (0.1 and 10)
#but we want to compute and compare the dependent variable
#when there's lots of different parameter values.

#describing grazing of a consumer on its food
#start with defining values of food and parameter values
food <- seq(0,30,by=0.1)
ks <- seq(1,10,by=2)

#next, calculate Monod function for every combo of food and ks values
#R's function outer() can do that
#stored in a matrix called foodmat, each column are the food values for every ks
#inside the outer() call is shorthand for function writing
#that we practiced above
foodmat <- outer(food,ks,function(x,y) x/(y+x))
head(foodmat)
summary(foodmat)

#next, plot them all on one graph
#matplot plots the columns of one matrix against the columns of another
windows(width=5, height=5)
matplot(x=food,y=foodmat,type="l",col="black",
        xlab="food",ylab="-",lwd=2,
        #this part is the title of the graph
        main=expression(frac(food,food+ks)))
#add a legend for the plot
#making a key for the line types is tricky
legend("bottomright", as.character(ks), title="ks=",lty=1:5,lwd=2)
