# create line graph of
# negative pseudolikelihood scores
# from SNAQ analyses
library(ggplot2)

df <- data.frame(hybrids=c(0,1,2,3,4,5),
                 score=c(-1202.4708, -598.2896, -364.9688, 
                         -311.2240, -311.1150, -311.1135))
my_title <- expression(paste("Roberts and Roalson - ", italic("American Journal of Botany"), " 2017 - Appendix S6. Figure S3. Model selection for SNAQ"))

ggplot(data=df, aes(x=hybrids, y=score)) +
  geom_line(linetype='dashed') +
  geom_point(size=4,
             shape=21,
             color='black',
             fill='white',
             stroke=2) +
  geom_label(aes(label=score), vjust=-1) +
  theme_bw() +
  labs(x='Number of hybridizations',
       y='Negative log pseudolikelihood',
       title=my_title) +
  theme(plot.title=element_text(hjust=0.5),
        panel.grid.minor=element_blank()) +
  scale_y_continuous(limits=c(-1250, -250),
                     breaks=c(-1250, -1000, -750, -500, -250))
ggsave(filename='AppendixS6.png', device='png', 
       width=10, height=7.5, dpi=300)

