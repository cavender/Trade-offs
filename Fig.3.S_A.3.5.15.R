####
#Species Area relationships
#J. Cavender-Bares, Aug. 21, 2013, updated March 5, 2015
#made progress but somehow species numbers are not adding up correctly

require(stats)
#### Define functions

Species_Area_1 <- function(A, params)

{

    a <- params$a1
    z <- params$z1
    m <- params$m1
  
    S<-a*(m*A)^z
   
    return(S)
}

Species_Area_2 <- function(A, params)

{

    a <- params$a2
    z <- params$z2
    m <- params$m2
  
    S<-a*(m*A)^z
   
    return(S)
}

Species_Area_3 <- function(A, params)

{

    a <- params$a3
    z <- params$z3
    m <- params$m3

  
    S<-a*(m*A)^z
   
    return(S)
}


Tot_Species_Area <- function(A, params)

{

    a1 <- params$a1 #intercept for region 1
    a2 <-params$a2 #intercept for region 2
    a3 <-params$a3 #intercept for region 3
    z1 <- params$z1 #slope for region 1
    z2 <- params$z2 #slope for region 2
    z3 <- params$z3 #slope for region 3
    m1 <- params$m1 #maximum land fraction for region1
    m2 <- params$m2 #maximum land fraction for region2
    m3 <- params$m3 #maximum land fraction for region3

  
    S= a1*(m1*A)^z1 + a2*(m2*A)^z2 + a3*(m3*A)^z3
   
    return(S)
}


Productivity_Area_1 <- function(A, params)

{

    m <- params$m1 #max land area
    C <- m-A # crop area
    b<-params$b1
    
      
    P <-b*C
   
    return(P)
}

Productivity_Area_2 <- function(A, params)

{

    m <- params$m2 #max land area
    C <- m-A # crop area
    b<-params$b2

      
    P <-b*C
   
    return(P)
}

Productivity_Area_3 <- function(A, params)

{

    m <- params$m3 #max land area
    C <- m-A # crop area
    b<-params$b3

      
    P <-b*C
   
    return(P)
}

Tot_Productivity_Area <- function(A, params)

{


    b1<-params$b1
    b2<-params$b2
    b3<-params$b3
    m1 <- params$m1 #max land area
    m2 <- params$m2 #max land area
    m3 <- params$m3 #max land area
    C1 <- m1-A # crop area
    C2 <- m2-A # crop area
    C3 <- m3-A # crop area

      
    P <-(b1*C1)+(b2*C2)+(b3*C3)
   
    return(P)
}



#### Define the parameters.

a <- 20
a1<-20
a2<-10
a3<-5
b <- 15
b1<-10
b2<-15
b3<-20  
m <- 1
m1<-0.25
m2<-0.35
m3 <-0.4
z1 <- 0.30
z2 <- 0.26
z3 <- 0.27

params <- list("a" = a, "a1"=a1, "a2"=a2,"a3"=a3, "b" = b, "b1"=b1, "b2"=b2, "b3"=b3, "m"= m, "m1"=m1, "m2"=m2, "m3"=m3,"z1"= z1, "z2"= z2, "z3"= z3)


#####For full area


# The area values range between 0 and 1.
A <- seq(0, m, by=0.001) 

# Get the equilibrium value(s) of S for each A for group 1.
S <- lapply(A, Tot_Species_Area, params)
x <- rep(A, unlist(lapply(S, length)))

# Store the results for species area.
s <- unlist(S)
X <- unlist(x)

SA <- cbind(X,s)   
colnames(SA) <- c("Area", "Species")

SA<- data.frame(SA)
#plot(SA$Area, SA$Species)

# Get the value(s) of P for each A for all groups.
P <- lapply(A, Tot_Productivity_Area, params)
t <- rep(A, unlist(lapply(P, length)))

# Store the results for species area.
p <- unlist(P)
t <- unlist(t)

PA <- cbind(t, p)   
colnames(PA) <- c("Area", "Productivity")

PA<- data.frame(PA)

#merge matrices
L<- merge(SA, PA, by.x="Area", by.y="Area")

#####For Groups, Change max land area parameter
#Group 1
# The area values range between 0 and 1.
A <- seq(0, m1, by=0.01) 

# Get the equilibrium value(s) of S for each A.
S <- lapply(A, Species_Area_1, params)
x <- rep(A, unlist(lapply(S, length)))

# Store the results for species area.
s <- unlist(S)
X <- unlist(x)

SA1 <- cbind(X,s)   
colnames(SA1) <- c("Area", "Species")

SA1<- data.frame(SA1)
#plot(SA$Area, SA$Species)

# Get the value(s) of P for each A.
P <- lapply(A, Productivity_Area_1, params)
t <- rep(A, unlist(lapply(P, length)))

# Store the results for species area.
p <- unlist(P)
t <- unlist(t)

PA1 <- cbind(t, p)   
colnames(PA1) <- c("Area", "Productivity")

PA1<- data.frame(PA1)


L1<- merge(SA1, PA1, by.x="Area", by.y="Area")

#Group 2

# The area values range between 0 and 1.
A <- seq(0, m2, by=0.01) 

# Get the equilibrium value(s) of S for each A.
S <- lapply(A, Species_Area_2, params)
x <- rep(A, unlist(lapply(S, length)))

# Store the results for species area.
s <- unlist(S)
X <- unlist(x)

SA2 <- cbind(X,s)   
colnames(SA2) <- c("Area", "Species")

SA2<- data.frame(SA2)

# Get the value(s) of P for each A.
P <- lapply(A, Productivity_Area_2, params)
t <- rep(A, unlist(lapply(P, length)))

# Store the results for species area.
p <- unlist(P)
t <- unlist(t)

PA2 <- cbind(t, p)   
colnames(PA2) <- c("Area", "Productivity")

PA2<- data.frame(PA2)
#plot(PA$Area, PA$Productivity)


L2<- merge(SA2, PA2, by.x="Area", by.y="Area")

#Group 3

# The area values range between 0 and 1.
A <- seq(0, m3, by=0.01) 

# Get the equilibrium value(s) of S for each A.
S <- lapply(A, Species_Area_3, params)
x <- rep(A, unlist(lapply(S, length)))

# Store the results for species area.
s <- unlist(S)
X <- unlist(x)

SA3 <- cbind(X,s)   
colnames(SA3) <- c("Area", "Species")

SA3<- data.frame(SA3)
#plot(SA$Area, SA$Species)

# Get the value(s) of P for each A.
P <- lapply(A, Productivity_Area_3, params)
t <- rep(A, unlist(lapply(P, length)))

# Store the results for species area.
p <- unlist(P)
t <- unlist(t)

PA3 <- cbind(t, p)   
colnames(PA3) <- c("Area", "Productivity")

PA3<- data.frame(PA3)
#plot(PA$Area, PA$Productivity)


L3<- merge(SA3, PA3, by.x="Area", by.y="Area")


#FIGURE 3
#efficiency frontier
dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))
plot(L$Productivity, L$Species, pch=1, cex=0.1, cex.lab = 1.5, font=2, pin = c(4,4), xlim = c(0.5,16), ylim = c(0.5, 18), xlab = "Crop Productivity",  ylab= "Biodiversity")
axis(lty = "solid",lwd = 2)
lines(L$Productivity, L$Species, type = "l", lwd =3, xaxt='n', yaxt='n' )
box(which = "plot", lty = "solid", lwd=3)

#Group1 - dotted
par(new = TRUE)
plot(L1$Productivity, L1$Species, xaxt='n', yaxt='n', pch=1, col="white", cex=0.1, cex.lab = 1.5, font=2, pin = c(4,4),  xlim = c(0.5,16), ylim = c(0.5, 18), xlab = " ",  ylab= " ")
axis(lty = "2",lwd = 2)
lines(L1$Productivity, L1$Species, type = "l", lwd =3, xaxt='n', yaxt='n', lty= 3)

#Group2 - dashed
par(new = TRUE)
plot(L2$Productivity, L2$Species, xaxt='n', yaxt='n', pch=1, col="white", cex=0.1, cex.lab = 1.5, font=2, pin = c(4,4), xlim = c(0.5,16), ylim = c(0.5, 18), xlab = " ",  ylab= " ")
axis(lty = "solid",lwd = 2)
lines(L2$Productivity, L2$Species, type = "l", lwd =3, xaxt='n', yaxt='n', lty= 5)

#Group3 - dashed
par(new = TRUE)
plot(L3$Productivity, L3$Species, xaxt='n', yaxt='n', pch=1, col="white", cex=0.1, cex.lab = 1.5, font=2, pin = c(4,4), xlim = c(0.5,16), ylim = c(0.5, 18), xlab = " ",  ylab= " ")
lines(L3$Productivity, L3$Species, type = "l", lwd =3, xaxt='n', yaxt='n', lty= "dotted")

