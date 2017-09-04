require(ggplot2)
require(gridExtra)
require(ggtree)

# AA-AE quartet tree proportions
data <- data.frame(number=c(0.48, 0.47, 0.05),
                   tree = c('p1','p2','p3'))

label <- c('N=631','N=609','N=66')

p1 <- ggplot(data, aes(x=tree, y=number,ymin=0, ymax=0.7)) +
    annotation_custom(grob=g2, xmin=0.4,xmax=1.5,ymin=0.14,ymax=0.34) + 
    annotation_custom(grob=g1, xmin=1.4, xmax=2.5, ymin=0.13, ymax=0.33) + 
    annotation_custom(grob=g3, xmin=2.4, xmax=3.5, ymin=0.25, ymax=0.45) +
    geom_bar(stat='identity',fill=NA,color='black',
            width=0.7,size=1) +
    labs(y='Proportion',title="Node 5") +
    theme_bw() +
    theme(#axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_rect(size=2),
          text=element_text(size=15),
          axis.ticks=element_line(size=1),
          axis.ticks.length=unit(.25,'cm'),
          plot.title=element_text(hjust=0.5)) +
    scale_x_discrete(breaks=c('p1','p2','p3'), labels=c('q1','q2','q3')) + 
    scale_y_continuous(breaks=seq(0,0.7,0.1)) +
    geom_text(aes(label=label),vjust=-1,size=5) +
    geom_segment(x=3,y=0.25,xend=3,yend=0.1,size=1,
                arrow=arrow(length=unit(0.5,'cm'),type='closed'))

# AT-Clade2 quartet tree proportions
data2 <- data.frame(number=c(0.40, 0.35, 0.25),
                    tree = c('p1','p2','p3'))

label2 <- c('N=515','N=462','N=330')

p2 <- ggplot(data2, aes(x=tree, y=number,ymax=0.7)) +
    annotation_custom(grob=g4, xmin=0.4,xmax=1.5,ymin=0.1,ymax=0.3) +
    annotation_custom(grob=g5, xmin=1.4,xmax=2.5,ymin=0.07,ymax=0.27) +
    annotation_custom(grob=g6, xmin=2.4,xmax=3.5,ymin=0.02,ymax=0.22) +
    geom_bar(stat='identity',fill=NA,color='black',
             width=0.7,size=1) +
    labs(y='Proportion',title='Node 2') +
    theme_bw() +
    theme(#axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_rect(size=2),
          text=element_text(size=15),
          axis.ticks=element_line(size=1),
          axis.ticks.length=unit(0.25,'cm'),
          plot.title=element_text(hjust=0.5)) +
    scale_x_discrete(breaks=c('p1','p2','p3'), labels=c('q1','q2','q3')) + 
    scale_y_continuous(breaks=seq(0,0.7,0.1)) +
    geom_text(aes(label=label2), vjust=-1,size=5)
    #geom_segment(x=3,y=0.25,xend=3,yend=0.06,size=1,
    #            arrow=arrow(length=unit(0.5,'cm'),type='closed'))

# AG-AP-AN quartet tree proportions
data3 <- data.frame(number=c(0.62, 0.25, 0.13),
                    tree = c('p1','p2','p3'))

label3 <- c('N=810','N=332','N=164')

p3 <- ggplot(data3, aes(x=tree, y=number,ymin=0,ymax=0.7)) +
  annotation_custom(grob=g7, xmin=0.4,xmax=1.5,ymin=0.21,ymax=0.41) +
  annotation_custom(grob=g8, xmin=1.4,xmax=2.5,ymin=0.02,ymax=0.22) +
  annotation_custom(grob=g9, xmin=2.4,xmax=3.5,ymin=0.25,ymax=0.45) +
  geom_bar(stat='identity',fill=NA,color='black',
           width=0.7,size=1) +
  labs(y='Proportion',title='Node 1') +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(size=2),
        text=element_text(size=15),
        axis.ticks=element_line(size=1),
        axis.ticks.length=unit(0.25,'cm'),
        plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,0.70,0.1)) +
  geom_text(aes(label=label3), vjust=-1, size=5) +
  geom_segment(x=3,y=0.25,xend=3,yend=0.18,size=1,
               arrow=arrow(length=unit(0.5,'cm'),type='closed'))

# AD-AM trees
data4 <- data.frame(number=c(0.526, 0.469, 0.05),
                    tree=c('p1','p2','p3'))

label4 <- c('N=330','N=294','N=3')

p4 <- ggplot(data4, aes(x=tree, y=number,ymin=0,ymax=0.7)) +
  annotation_custom(grob=g10,xmin=0.4,xmax=1.5,ymin=0.16,ymax=0.36) +
  annotation_custom(grob=g11,xmin=1.4,xmax=2.5,ymin=0.13,ymax=0.33) +
  annotation_custom(grob=g12,xmin=2.4,xmax=3.5,ymin=0.25,ymax=0.45) +
  geom_bar(stat='identity',fill=NA,color='black',
           width=0.7,size=1) +
  labs(y='Proportion',title='AD and AM') +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(size=2),
        text=element_text(size=15),
        axis.ticks=element_line(size=1),
        axis.ticks.length=unit(0.25,'cm'),
        plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(breaks=seq(0,0.70,0.1)) +
  geom_text(aes(label=label4), vjust=-1, size=5) +
  geom_segment(x=3,y=0.25,xend=3,yend=0.1,size=1,
               arrow=arrow(length=unit(0.5,'cm'),type='closed'))


p3 <- textGrob(expression(paste("Roberts and Roalson - ", italic("American Journal of Botany"), " 2017 - Appendix S5. Figure S2. Gene tree patterns")))
g <- grid.arrange(p3,p2,p1,ncol=1, heights=c(0.1,1,1))
ggsave("AppendixS5.png", g, device='png', dpi=300,
       height=40, width=20, units='cm')

# Node 5 trees
tree1 <- read.tree('tree1.nwk')
tree2 <- read.tree('tree2.nwk')
tree3 <- read.tree('tree3.nwk')

g1 <- ggplotGrob(
  ggtree(tree1, layout='slanted',size=1,
                       branch.length='none') +
  geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
  scale_x_continuous(expand=c(1,1),limits=c(0,4)) +
  scale_size_manual(values=c(1,1)) +
  theme_transparent())

g2 <- ggplotGrob(
  ggtree(tree2, layout='slanted',size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1),limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent())

g3 <- ggplotGrob(
  ggtree(tree3, layout='slanted',size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1),limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent())

# Node 2 trees
tree4 = read.tree('tree4.nwk')
tree5 = read.tree('tree5.nwk')
tree6 = read.tree('tree6.nwk')

g4 <- ggplotGrob(
  ggtree(tree4, layout='slanted', size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1),limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent()
)

g5 <- ggplotGrob(
  ggtree(tree5, layout='slanted',size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15, align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1),limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent()
)

g6 <- ggplotGrob(
  ggtree(tree6, layout='slanted',size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1), limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent()
)

# Node 1 trees
tree7 <- read.tree('tree7.nwk')
tree8 <- read.tree('tree8.nwk')
tree9 <- read.tree('tree9.nwk')

g7 <- ggplotGrob(
  ggtree(tree7, layout='slanted', size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1), limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent()
)

g8 <- ggplotGrob(
  ggtree(tree8, layout='slanted', size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1), limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent()
)

g9 <- ggplotGrob(
  ggtree(tree9, layout='slanted', size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1), limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent()
)

# AD-AM trees
tree10 <- read.tree('tree10.nwk')
tree11 <- read.tree('tree11.nwk')
tree12 <- read.tree('tree12.nwk')

g10 <- ggplotGrob(
  ggtree(tree10, layout='slanted', size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1), limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent()
)

g11 <- ggplotGrob(
  ggtree(tree11, layout='slanted', size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1), limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent()
)

g12 <- ggplotGrob(
  ggtree(tree12, layout='slanted', size=1,
         branch.length='none') +
    geom_tiplab(hjust=-0.15,align=F,linesize=4,fontface='bold') +
    scale_x_continuous(expand=c(1,1), limits=c(0,4)) +
    scale_size_manual(values=c(1,1)) +
    theme_transparent()
)

ggsave("tree1.png", device='png', dpi=300,
       width=4, height=6, units='cm')

