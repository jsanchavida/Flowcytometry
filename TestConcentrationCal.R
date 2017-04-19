library("dplyr")
beads <- 50
x <- c(1,2,3,4,5,6,7,8,9,10)
y <- c(2,3,2,3,2,3,2,3,2,3)
z <- c(100,200,300,400,500,600,700,800,900,1000)
f <- c(100,200,300,400,500,600,700,800,900,1000)
g <- c(0,2,3,400,500,600,700,800,900,1000)
df <- data.frame(x, y, z, f, g)
df_1<-do.call(cbind,lapply(df,function(x)(x/y) * 50))
as.data.frame(df_2)

data_conc <- read.csv("/Users/josesancho/Downloads/13-03659CELL_CONC.csv", header = TRUE, sep = ",")
BEADS <-50000
data_conc2 <- data_conc[,1:9]
str(data_conc2)
data_conc2 [,-c(5:7)]
df_2 <-do.call(cbind,lapply(data_conc2 [,-(1:7)],function(x)(data_conc2$Beads.Count/x) * (BEADS/data_conc2$Sample.Vol.uL.)))
df_3 <- cbind((data_conc2 [,c(1:7)]), df_2)
data_conc
?read.csv