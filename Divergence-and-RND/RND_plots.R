require(ggplot2)

ds <- data.frame(comparison=c('AA AE','AA AE both','AD AM','AD AM both','AG AP','AG AP both'), 
                 mean=c(0.246,0.296,0.331,0.582,0.214,0.284),
                 lower=c(0.2150,0.2701,0.3010,0.54,0.2004,0.2610),
                 upper=c(0.2801,0.3245,0.3647,0.6337,0.2287,0.3110),
                 group=c('AA and AE','AA and AE','AD and AM','AD and AM','AG and AP','AG and AP'),
                 number=c(242,387,255,548,580,826))

ds$comparison <- factor(ds$comparison, levels=ds$comparison)

ann_text1 <- data.frame(comparison='AA AE',mean=0,
                        group='AA and AE', lab='text',
                        t = factor('AA AE',levels=c('AA AE','AD AM','AG AP')))
ann_text2 <- data.frame(comparison='AD AM',mean=0,
                        group='AD and AM', lab='text',
                        t = factor('AD AM',levels=c('AA AE','AD AM','AG AP')))
ann_text3 <- data.frame(comparison='AG AP',mean=0,
                        group='AG and AP', lab='text',
                        t = factor('AG AP',levels=c('AA AE','AD AM','AG AP')))
#ann_text4 <- data.frame(comparison='AP,AT|AG',mean=0,
#                        group='AP and AT', lab='text',
#                        t = factor('AP,AT|AG',levels=c('AA,AE|AC,AL','AD,AM|Clade2','AG,AP|AT','AP,AT|AG')))


ggplot(ds,aes(x=comparison,y=mean,ymin=0,ymax=0.9)) + 
  geom_point(size=1) + 
  geom_errorbar(aes(ymin=lower,ymax=upper),width=0.1) + 
  theme_linedraw() + 
  labs(x='',y='RND') + 
  facet_grid(~group,scales='free') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=10, angle=45, hjust=1)) +
  scale_y_continuous(breaks=seq(0,0.9,0.15)) +
  geom_text(data = ds[ds$group == 'AA and AE', ], 
            aes(label= paste('N =',number)), vjust=-8, size=2) +
  geom_text(data = ds[ds$group == 'AD and AM', ],
            aes(label= paste('N = ',number)), vjust=-8, size=2) +
  geom_text(data = ds[ds$group == 'AG and AP', ],
            aes(label= paste('N = ',number)), vjust=-8, size=2) +
  #geom_text(data = ds[ds$group == 'AP and AT', ],
  #          aes(label= paste('N = ',number)), vjust=-8,size=2) +
  geom_text(data=ann_text1,size=2,label='italic(t)==2.306  ~df==527  ~italic(P)==0.022',parse=T,nudge_x=0.5) +
  geom_text(data=ann_text2,size=2,label='italic(t)==8.682  ~df==801  ~italic(P)<0.001',parse=T,nudge_x=0.5) +
  geom_text(data=ann_text3,size=2,label='italic(t)==4.727  ~df==1250  ~italic(P)<0.001',parse=T,nudge_x=0.5)
  #geom_text(data=ann_text4,size=2,label='italic(t)==1.3259 ~italic(p)==0.1855',parse=T,nudge_x=0.5)

ggsave("Figure3.png", device="png", dpi=600,
       width=14, height=12, units='cm')


