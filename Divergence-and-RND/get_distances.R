require(seqinr)
require(ape)
require(gdata)
require(boot)

# Calculate pairwise genetic distances from fasta alignments

input <-readLines("input_list.txt") 

fit <- for(i in 1:length(input)){
  alignment <- read.dna(file=input[[i]],format="fasta")
  dist <- dist.dna(alignment,model="raw",pairwise.deletion=TRUE)
  dist <- as.matrix(dist)
  out <- t(unmatrix(dist))
  write.table(data.frame(out),col.names=F,file='output.txt',quote=F,row.names=F,append=TRUE,sep='\t')
  }

data_all = read.table('output.txt',header=F)
data_all = data_all[complete.cases(data_all), ]
data_all[data_all == 0] <- 0.001

# basic statistics for entire distance matrix

error <- function(data) {
  sd <- apply(data, 2, sd)
  qnorm(0.975)*sd/sqrt(1142)
}

dist_means <- apply(data, 2, mean)
dist_sd <- apply(data, 2, sd)
dist_error <- apply(data, 2, error)

# calculate relative node depth (RND) statistic

RND <- function(x) {
  x[4]/((0.0436+0.0439)/2) # change for the comparison being performed
}

boot.rnd <- function(x,i) {
  i <- sample(1:nrow(data),replace=T)
  bootdata <- x[i,]
  out <- apply(bootdata, 1, RND)
  mean(out)
}

# perform bootstrap resampling

boot.out <- boot(data, 
                 statistic=boot.rnd,
                 R=50000,
                 sim='ordinary')

plot(boot.out)

mean(boot.out$t)

# calculate 95% confidence intervals

boot.ci(boot.out, conf=0.95, type=c('norm','basic','perc','bca'))

# AA-AE sister clades, i.e. "AA-AE"
data <- read.table('AA-AE_output.txt',header=F, row.names=1) # N=281
data <- data[complete.cases(data), ]
data[data == 0] <- 0.001

RND <- function(x) {
  x[4]/((0.0436+0.0439)/2)
}

boot.rnd <- function(x,i) {
  i <- sample(1:nrow(x),replace=T)
  bootdata <- x[i,]
  out <- apply(bootdata, 1, RND)
  mean(out)
}

boot.out1 <- boot(data, boot.rnd, R=10000, sim='ordinary')

plot(boot.out1)
mean(boot.out1$t)   # 0.2457472

boot.ci(boot.out1, conf=0.95, type=c('norm','basic','perc','bca'))
    # ( 0.2150, 0.2801 )

# AA-AE clades plus AA-AC-AL clades, i.e. "Both"
data2 <- read.table('AA-AC-AL_output.txt',header=F) # N=155
data2 <- data2[complete.cases(data2), ]
data2[data2 == 0] <- 0.001

dataCombined <- rbind(data, data2) # N=387
dataCombined <- dataCombined[complete.cases(dataCombined), ]

RND <- function(x) {
  x[4]/((0.0436+0.0439)/2)
}

boot.rnd <- function(x,i) {
  i <- sample(1:nrow(x),replace=T)
  bootdata <- x[i,]
  out <- apply(bootdata, 1, RND)
  mean(out)
}

boot.out2 <- boot(dataCombined, boot.rnd, R=10000, sim='ordinary')

plot(boot.out2)
mean(boot.out2$t)   # 0.296

boot.ci(boot.out2, conf=0.95, type=c('norm','basic','perc','bca'))
    # ( 0.2701, 0.3245 )

rndAA_AE <- RND(data)
rndAA_AC_AL <- RND(data2)
rndAA_AE_both <- RND(dataCombined)

t.test(rndAA_AE$V4, rndAA_AC_AL$V4)
# t = -4.7854   df = 296.03   p = 2.697e-06

t.test(rndAA_AE$V4, rndAA_AE_both$V4)
# t = -2.3055    df = 526.88   p = 0.02152

# AD-AM sister clades, i.e. "AD-AM"
data3 <- read.table('AD-AM_output.txt', header=F)
data3 <- data3[complete.cases(data3), ]
data3[data3 == 0] <- 0.001

RND <- function(x) {
  x[31]/((0.0422+0.0429)/2)
}

boot.rnd <- function(x,i) {
  i <- sample(1:nrow(x),replace=T)
  bootdata <- x[i,]
  out <- apply(bootdata, 1, RND)
  mean(out)
}

boot.out3 <- boot(data3, boot.rnd, R=10000, sim='ordinary')

plot(boot.out3)
mean(boot.out3$t)   # 0.3308909

boot.ci(boot.out3, conf=0.95, type=c('norm','basic','perc','bca'))
    # ( 0.3010, 0.3647 )

# AD-AM clades plus AD-clade2, i.e. "Both"
data4 <- read.table('AD-clade2_output.txt',header=F)
data4 <- data4[complete.cases(data4), ]
data4[data4 == 0] <- 0.001

dataCombined2 <- rbind(data3, data4)
dataCombined2 <- dataCombined2[complete.cases(dataCombined2), ]

RND <- function(x) {
  x[31]/((0.0422+0.0429)/2)
}

boot.rnd <- function(x,i) {
  i <- sample(1:nrow(x),replace=T)
  bootdata <- x[i,]
  out <- apply(bootdata, 1, RND)
  mean(out)
}

boot.out4 <- boot(dataCombined2, boot.rnd, R=10000, sim='ordinary')

plot(boot.out4)
mean(boot.out4$t)   # 0.5821716

boot.ci(boot.out4, conf=0.95, type=c('norm','basic','perc','bca'))
    # ( 0.5392, 0.6335 )

rndAD_AM <- RND(data3)
#rndAD_clade2 <- RND(data4)
rndAD_AM_both <- RND(dataCombined2)

t.test(rndAD_clade2$V31, rndAD_AM$V31)
#   t = 11.36    df = 394.5   p < 2.2e-16

t.test(rndAD_AM_both$V31, rndAD_AM$V31)
#   t = 8.6824    df = 800.99   p < 2.2e-16

# AG-AP sister clades, i.e. "AG-AP"
data5 <- read.table('AG-AP_output.txt',header=F)
data5 <- data5[complete.cases(data5), ]
data5[data5 == 0] <- 0.001

RND <- function(x) {
  x[57]/((0.0421+0.0419)/2)
}

boot.rnd <- function(x,i) {
  i <- sample(1:nrow(x), replace=T)
  bootdata <- x[i,]
  out <- apply(bootdata, 1, RND)
  mean(out)
}

boot.out5 <- boot(data5, boot.rnd, R=10000, sim='ordinary')

plot(boot.out5)
mean(boot.out5$t)   # 0.2140948

boot.ci(boot.out5, conf=0.95, type=c('norm','basic','perc','bca'))
    # ( 0.2008, 0.2286 )

# AP-AT - "AG-AP both"
data6 <- read.table('AP-AT_output.txt',header=F)
data6 <- data6[complete.cases(data6), ]
data6[data6 == 0] <- 0.001

#data7 <- read.table('AP-AN_output.txt',header=F)
#data7 <- data7[complete.cases(data7), ]
#data7[data7 == 0] <- 0.001

dataCombined3 <- rbind(data5, data6)
dataCombined3 <- dataCombined3[complete.cases(dataCombined3), ]

RND <- function(x) {
  x[57]/((0.0421+0.0419)/2)
}

boot.rnd <- function(x,i) {
  i <- sample(1:nrow(x), replace=T)
  bootdata <- x[i,]
  out <- apply(bootdata, 1, RND)
  mean(out)
}

boot.out6 <- boot(dataCombined3, boot.rnd, R=10000, sim='ordinary')

plot(boot.out6)
mean(boot.out6$t)   # 0.2834788

boot.ci(boot.out6, conf=0.95, type=c('norm','basic','perc','bca'))
    #   ( 0.2610, 0.3115 )


rndAG_AP <- RND(data5)
rndAG_AP_all <- RND(dataCombined3)
#rndAG_AP_avg <- RND(data_all)

t.test(rndAG_AP$V57, rndAG_AP_all$V57)
#   t = 4.727    df = 1250   p = 2.543e-06

