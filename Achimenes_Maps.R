library(ggplot2)
library(rgbif)

# misera and candida

key <- name_backbone(name='Achimenes misera')$speciesKey
key2 <- name_backbone(name='Achimenes candida')$speciesKey

dat <- occ_search(taxonKey=key,return='data',limit=300)
dat2 <- occ_search(taxonKey=key2,return='data',limit=300)

total <- merge(dat,dat2,all=T)

gbifmap(dat,mapdatabase='world',
        region=c('Mexico','Guatemala','Belize','Honduras',
                 'Nicaragua','Costa Rica','Panama',
                 'Cuba','Jamaica','Haiti','Dominican Republic',
                 'El Salvador'),
        customize=borders(database='world',
                          regions=c('Mexico','Guatemala','Belize','Honduras',
                                    'Nicaragua','Costa Rica','Panama','Cuba',
                                    'Jamaica','Haiti','Dominican Republic',
                                    'El Salvador'),
                          size=1))
ggsave('misera_candida_distribution.png',device='png',dpi=600)

# admirabilis and erecta
key3 <- name_backbone(name='Achimenes erecta')$speciesKey
key4 <- name_backbone(name='Achimenes admirabilis')$speciesKey

dat3 <- occ_search(taxonKey=key3,return='data',limit=500)
dat4 <- occ_search(taxonKey=key4,return='data',limit=300)

total2 <- merge(dat3,dat4,all=T)

gbifmap(total2,mapdatabase='world',
        region=c('Mexico','Guatemala','Belize','Honduras',
                 'Nicaragua','Costa Rica','Panama','Cuba',
                 'Jamaica','Haiti','Dominican Republic',
                 'El Salvador'),
        customize=borders(database='world',
                          regions=c('Mexico','Guatemala','Belize','Honduras',
                                    'Nicaragua','Costa Rica','Panama','Cuba',
                                    'Jamaica','Haiti','Dominican Republic',
                                    'El Salvador'),
                          size=1))
ggsave('erecta_admirabilis_distribution.png',device='png',dpi=600)

# patens, grandiflora, antirrhina
key5 <- name_backbone(name='Achimenes patens')$speciesKey
key6 <- name_backbone(name='Achimenes grandiflora')$speciesKey
key7 <- name_backbone(name='Achimenes antirrhina')$speciesKey

dat5 <- occ_search(taxonKey=key5,return='data',limit=300)
dat6 <- occ_search(taxonKey=key6,return='data',limit=300)
dat7 <- occ_search(taxonKey=key7,return='data',limit=300)

total3 <- merge(dat5,dat6,all=T)
total4 <- merge(total3,dat7,all=T)

gbifmap(total3,mapdatabase='world',
        region=c('Mexico','Guatemala','Belize','Honduras',
                 'Nicaragua','Costa Rica','Panama','Cuba',
                 'Jamaica','Haiti','Dominican Republic',
                 'El Salvador'),
        customize=borders(database='world',
                  regions=c('Mexico','Guatemala','Belize','Honduras',
                            'Nicaragua','Costa Rica','Panama','Cuba',
                            'Jamaica','Haiti','Dominican Republic',
                            'El Salvador'),
                  size=1))
ggsave('patens_grandiflora_distribution.png',device='png',dpi=600)

gbifmap(total4,mapdatabase='world',
        region=c('Mexico','Guatemala','Belize','Honduras',
                 'Nicaragua','Costa Rica','Panama','Cuba',
                 'Jamaica','Haiti','Dominican Republic',
                 'El Salvador'),
        customize=borders(database='world',
                          regions=c('Mexico','Guatemala','Belize','Honduras',
                                    'Nicaragua','Costa Rica','Panama','Cuba',
                                    'Jamaica','Haiti','Dominican Republic',
                                    'El Salvador'),
                          size=1))
ggsave('patens_grandiflora_antirrhina_distribution.png',device='png',dpi=600)

# ingroup Achimenes
key1 <- name_backbone(name='Achimenes misera')$speciesKey
key2 <- name_backbone(name='Achimenes erecta')$speciesKey
key3 <- name_backbone(name='Achimenes longiflora')$speciesKey
key4 <- name_backbone(name='Achimenes cettoana')$speciesKey
key5 <- name_backbone(name='Achimenes grandiflora')$speciesKey
key6 <- name_backbone(name='Achimenes patens')$speciesKey
key7 <- name_backbone(name='Achimenes pedunculata')$speciesKey
key8 <- name_backbone(name='Achimenes antirrhina')$speciesKey
key9 <- name_backbone(name='Achimenes candida')$speciesKey

dat1 <- occ_search(taxonKey=key1, return='data', limit=300)
dat2 <- occ_search(taxonKey=key2, return='data', limit=300)
dat3 <- occ_search(taxonKey=key3, return='data', limit=300)
dat4 <- occ_search(taxonKey=key4, return='data', limit=300)
dat5 <- occ_search(taxonKey=key5, return='data', limit=300)
dat6 <- occ_search(taxonKey=key6, return='data', limit=300)
dat7 <- occ_search(taxonKey=key7, return='data', limit=200)
dat8 <- occ_search(taxonKey=key8, return='data', limit=300)
dat9 <- occ_search(taxonKey=key9, return='data', limit=300)

total1 <- merge(dat1, dat2, all=T)
total2 <- merge(total1, dat3, all=T)
total3 <- merge(total2, dat4, all=T)
total4 <- merge(total3, dat5, all=T)
total5 <- merge(total4, dat6, all=T)
total6 <- merge(total5, dat7, all=T)
total7 <- merge(total6, dat8, all=T)
total8 <- merge(total7, dat9, all=T)

gbifmap2(total8,mapdatabase='world',
        region=c('Mexico','Guatemala','Belize','Honduras',
                 'Nicaragua','Costa Rica','Panama','Cuba',
                 'Jamaica','Haiti','Dominican Republic',
                 'El Salvador'),
        customize=borders(database='world',
                          regions=c('Mexico','Guatemala','Belize','Honduras',
                                    'Nicaragua','Costa Rica','Panama','Cuba',
                                    'Jamaica','Haiti','Dominican Republic',
                                    'El Salvador'),
                          size=1))

ggsave('achimenes_distribution.png', device='png', dpi=600)

gbifmap2 <- function (input = NULL, mapdatabase = "world", region = ".", 
          geom = geom_point, jitter = NULL, customize = NULL) 
{
  check_for_a_pkg("maps")
  tomap <- input[stats::complete.cases(input$decimalLatitude, 
                                       input$decimalLatitude), ]
  tomap <- tomap[!tomap$decimalLongitude == 0 & !tomap$decimalLatitude == 
                   0, ]
  tomap <- tomap[-(which(tomap$decimalLatitude <= 90 || tomap$decimalLongitude <= 
                           180)), ]
  tomap$name <- as.factor(gbif_capwords(tomap$name, onlyfirst = TRUE))
  if (is.null(jitter)) {
    jitter <- position_jitter()
  }
  if (length(unique(tomap$name)) == 1) {
    theme2 <- theme(legend.position = "none")
  }
  else {
    theme2 <- NULL
  }
  world <- map_data(map = mapdatabase, region = region)
  message(paste("Rendering map...plotting ", nrow(tomap), " points", 
                sep = ""))
  ggplot(world, aes(long, lat)) + 
    geom_polygon(aes(group = group), fill = "white", color = "gray40", size = 0.2) + 
    geom(data = tomap, aes(decimalLongitude, decimalLatitude, colour = name), alpha = 0.4, size = 3, position = jitter) + 
    scale_color_brewer("", type = "qual", palette = 'Reds') + 
    labs(x = "", y = "") + 
    theme_bw(base_size = 14) + 
    theme(legend.position = "bottom", legend.key = element_blank()) + 
    guides(col = guide_legend(nrow = 2)) + 
    coord_fixed(ratio = 1) + 
    blanktheme() + 
    theme2 + 
    customize
}

check_for_a_pkg <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  } else {
    invisible(TRUE)
  }
}

gbif_capwords <- function(s, strict = FALSE, onlyfirst = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
                           {s <- substring(s,2); if(strict) tolower(s) else s}, sep = "", collapse = " " )
  if(!onlyfirst){
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  } else {
    sapply(s, function(x)
      paste(toupper(substring(x,1,1)),
            tolower(substring(x,2)),
            sep="", collapse=" "), USE.NAMES=F)
  }
}