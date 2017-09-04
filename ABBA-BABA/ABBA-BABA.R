# calculate D statistics for sets of 4 populations
# using HybridCheck

require(HybridCheck)

# Example 1: test for admixture between AA and AE
# (AC, AL), (AA), (AE), (AM)
Analysis1 <- HC$new("MO_cds_filter300-12_NOGAPS.fa")

populations1 <- list(
  AA = c('AA'),
  AC = c('AC','AL'),
  AE = c('AE'),
  clade2 = c('AM','AD','AP','AT','AN','AG')
)

Analysis1$setPopulations(populations1)

Analysis1$prepareFourTaxonTests()

popCombos <- list(
  c(P1 = "AC", P2 = "AA", P3 = "AE", A = "clade2")
)

Analysis1$prepareFourTaxonTests(popCombos)

Analysis1$runFourTaxonTests(selections = 'ALL', 
                            blockLength = 1000L)

fttResults1 <- Analysis1$tabulateFourTaxonTests(selections='TESTED',
                                                neat=T,
                                                global=T)

split_list <- split(fttResults1, fttResults1$P2)
set1 <- as.data.frame(split_list[[1]])
set2 <- as.data.frame(split_list[[2]])

# set1 = AA,AE,clade1,clade2
# set2 = clade1,AA,AE,clade2

set1$ABBA   # 88.75
set1$BABA   # 894.8333

set2$ABBA  # 894.3333
set2$BABA  # 88.75

set1$X2_P # 1.649833e-27
set2$X2_P # 3.599485e-31

set1$D_jEstimate  # -0.8195374
set2$D_jEstimate  # 0.8194456

set1$Fd_1DD4_jEstimate  # 0
set1$Fd_D2D4_jEstimate  # 0.2038362

set2$Fd_1DD4_jEstimate  # 0.1713703
set2$Fd_D2D4_jEstimate  # 0

# repeat steps for other sets of 4 you wish to test