require(ape)
require(ggtree)

tree <- read.tree('RAxML_bestTree.MO_filter300-12_cds2')

tree2 <- read.tree('net3.nwk')

ggtree(tree2,branch.length='none',ladderize=FALSE,size=1) +
  geom_tiplab(size=5,fontface='italic') +
  #theme_tree2() +
  scale_x_continuous(limits=c(-1,10)) +
  #geom_text2(aes(subset=!isTip, label=node), hjust=-.3) +
  geom_cladelabel(node=16, label="Clade 2",hjust='center',
                  angle=270,offset=2.5,offset.text=0.25,
                  color='black',barsize=3,fontsize=6) +
  geom_cladelabel(node=20, label="Clade 1", hjust='center',
                  angle=270,offset=2.5,offset.text=0.25,
                  color='black',barsize=3,fontsize=6) +
  #xlab("MYA") +
  #theme_bw() +
  geom_segment(x=4.5,y=10,xend=4.5,yend=5.1,size=1.5,color='blue',
               arrow=arrow(length=unit(0.25,'cm'),
                           type='closed',angle=30)) +
  #geom_segment(x=6.5,y=7,xend=6.5,yend=4.1,size=2,color='darkgreen',
  #             arrow=arrow(length=unit(0.25,'cm'),
  #                         type='closed',angle=30)) +
  geom_segment(x=5.7,y=8,xend=5.7,yend=8.9,size=1.5,color='red',
               arrow=arrow(length=unit(0.25,'cm'),
                           type='closed',angle=30)) +
  geom_segment(x=6.5,y=4,xend=6.5,yend=1.1,size=1.5,color='darkgreen',
               arrow=arrow(length=unit(0.25,'cm'),
                           type='closed',angle=30)) +
  #geom_segment(x=6,y=4.1,xend=6,yend=4.9,size=1,color='green',
  #             arrow=arrow(length=unit(0.25,'cm'),ends='both',
  #                         type='closed',angle=30)) +
  annotate("text",x=5.25,y=6,label="γ = 0.24",color='blue',size=4) +
  #annotate("text",x=7,y=5.5,label="γ = 0.40",color='darkgreen',size=5) +
  annotate("text",x=6.45,y=8.5,label="γ = 0.47",color='red',size=4) +
  annotate("text",x=7.25,y=2.5,label="γ = 0.40",color='darkgreen',size=4) +
  annotate("text",x=5.25,y=8.5,label="46",color='red',size=4) +
  annotate("text",x=4.05,y=6,label="100",color='blue',size=4) +
  annotate("text",x=6.05,y=2.5,label="100",color='darkgreen',size=4) +
  annotate("text",x=5.5,y=3.8,label='90',color='black',size=4) + 
  annotate("text",x=5.5,y=1.8,label='90',color='black',size=4) +
  annotate("text",x=3.5,y=8.4,label='54',color='black',size=4)

ggsave('phylo3.png',device='png',dpi=600,
       height=12,width=18.4,units='cm')


# plot RAxML tree
raxml <- read.tree('RAxML_bipartitions.MO_cds_filter300-12.tre')

raxml2 <- groupClade(raxml, node=c(20,16))

ggtree(raxml,size=1) +
  geom_treescale(offset=-0.3,linesize=1) +
  geom_tiplab(fontface='italic',size=5) +
  scale_x_continuous(limits=c(0,0.07)) +
  #geom_text2(aes(subset=!isTip, label=node), hjust=-.3) +
  geom_cladelabel(node=16, label="Clade 2",hjust='center',
                  angle=270,offset=0.012,offset.text=0.005,
                  color='black',barsize=1.5,fontsize=6,
                  align=T) +
  geom_cladelabel(node=20, label="Clade 1", hjust='center',
                  angle=270,offset=0.012,offset.text=0.005,
                  color='black',barsize=1.5,fontsize=6,
                  align=T)

ggsave('RAxML_tree.png',device='png',dpi=600,
       height=20, width=25, units='cm')

# plot gene flow from D-statistics

ggtree(tree,branch.length='none') +
  geom_tiplab(size=5,fontface='italic') +
  #theme_tree2() +
  scale_x_continuous(limits=c(-1,9)) +
  #geom_text2(aes(subset=!isTip, label=node), hjust=-.3) +
  geom_cladelabel(node=16, label="Clade 2",hjust='center',
                  angle=270,offset=1.5,offset.text=0.25,
                  color='black',barsize=1.5,fontsize=6) +
  geom_cladelabel(node=20, label="Clade 1", hjust='center',
                  angle=270,offset=1.5,offset.text=0.25,
                  color='black',barsize=1.5,fontsize=6) +
  #xlab("MYA") +
  #theme_bw() +
  geom_segment(x=4.5,y=7.9,xend=4.5,yend=3.1,size=2,color='blue',
               arrow=arrow(length=unit(0.25,'cm'),ends='both',
                           type='closed',angle=30)) +
  geom_segment(x=6.5,y=6.1,xend=6.5,yend=6.9,size=2,color='darkgreen',
               arrow=arrow(length=unit(0.25,'cm'),ends='both',
                           type='closed',angle=30)) +
  geom_segment(x=6.5,y=9.1,xend=6.5,yend=9.9,size=2,color='red',
               arrow=arrow(length=unit(0.25,'cm'),ends='both',
                           type='closed',angle=30)) +
  #geom_segment(x=3.5,y=4.8,xend=3.5,yend=3.1,size=2,color='purple',
  #             arrow=arrow(length=unit(0.25,'cm'),ends='both',
  #                         type='closed',angle=30)) +
  geom_segment(x=5.8,y=4.1,xend=5.8,yend=6.4,size=2,color='orange',
               arrow=arrow(length=unit(0.25,'cm'),ends='both',
                           type='closed',angle=30)) +
  geom_segment(x=5.5,y=5.1,xend=5.5,yend=6.4,size=2,color='orchid',
               arrow=arrow(length=unit(0.25,'cm'),ends='both',
                           type='closed',angle=30)) +
  #geom_segment(x=6,y=4.1,xend=6,yend=4.9,size=1,color='green',
  #             arrow=arrow(length=unit(0.25,'cm'),ends='both',
  #                         type='closed',angle=30)) +
  annotate("text",x=5,y=7.5,label="D = 0.85",color='blue',size=5) +
  annotate("text",x=7,y=6.5,label="D = 0.68",color='darkgreen',size=5) +
  annotate("text",x=7,y=9.5,label="D = 0.82",color='red',size=5) +
  annotate("text",x=6.25,y=4.5,label="D = 0.12",color='orange',size=5) +
  #annotate("text",x=4,y=3.5,label="D = -0.53",color='purple',size=5) +
  annotate("text",x=6.25,y=5.5,label="D = 0.17",color='orchid',size=5)

ggsave('phylo_D.png',device='png',dpi=600,
       height=20,width=30,units='cm')
