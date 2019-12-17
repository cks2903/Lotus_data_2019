#### R - plots and analyses ####


## Plot geographic, PCA and structure analysis data
{
  setwd("")
  d <- read.delim("20181102_136LjAccessionData.txt")

  ## load libraries
  library(ggplot2)

  # set graphics parameters
  alpha = 0.8
  font.size = 24
  point.size = 5



  ### simple theme with no gridlines
  theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=font.size), legend.text=element_text(size=font.size), axis.title=element_text(size=font.size,face="bold"), legend.title=element_text(size=font.size, face="bold"), legend.background = element_rect(fill="transparent"), legend.position = c(0.2,0.8) )


  ## plot PCA colored by population structure groups

  #pop1
  filename <- paste(Sys.Date(),"_pca_pop1.pdf", sep="")
  pdf(filename)
  pv <- ggplot(data=d,aes(x=-PC1,y=-PC2))
  pv + geom_point(aes(x=-PC1,y=-PC2,col=Pop1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="PC1",y="PC2",color="Pop1") + scale_color_gradient(low="white", high="#FF3300") + coord_flip() + theme
  dev.off()

  # pop2
  filename <- paste(Sys.Date(),"_pca_pop2.pdf", sep="")
  pdf(filename)
  pv <- ggplot(data=d,aes(x=-PC1,y=-PC2))
  pv + geom_point(aes(x=-PC1,y=-PC2,col=Pop2),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="PC1",y="PC2",color="Pop2") + scale_color_gradient(low="white", high="#33FF00") + coord_flip() + theme
  dev.off()

  #pop3
  filename <- paste(Sys.Date(),"_pca_pop3.pdf", sep="")
  pdf(filename)
  pv <- ggplot(data=d,aes(x=-PC1,y=-PC2))
  pv + geom_point(aes(x=-PC1,y=-PC2,col=Pop3),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="PC1",y="PC2",color="Pop3") + scale_color_gradient(low="white", high="#0099FF") + coord_flip() + theme
  dev.off()



  ## plot PCA colored by latitude and longitude
  #Latitude
  filename <- paste(Sys.Date(),"_pca_latitude.pdf", sep="")
  pdf(filename)
  pv <- ggplot(data=d,aes(x=-PC1,y=-PC2))
  pv + geom_point(aes(x=-PC1,y=-PC2,col=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="PC1",y="PC2",color="Latitude") + scale_color_gradientn(colours = rainbow(5)) + coord_flip() + theme
  dev.off()

  #Longitude
  filename <- paste(Sys.Date(),"_pca_longitude.pdf", sep="")
  pdf(filename)
  pv <- ggplot(data=d,aes(x=-PC1,y=-PC2))
  pv + geom_point(aes(x=-PC1,y=-PC2,col=Longitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="PC1",y="PC2",color="Longitude") + scale_color_gradientn(colours = rainbow(5)) + coord_flip() + theme
  #+geom_text(aes(label=V4), size =3, hjust=1, vjust=0)
  dev.off()



  ## plot PCA colored by PC1 and PC2
  #PC1
  filename <- paste(Sys.Date(),"_pca_PC1.pdf", sep="")
  pdf(filename)
  pv <- ggplot(data=d,aes(x=PC1,y=PC2))
  pv + geom_point(aes(x=-PC1,y=-PC2,col=-PC1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="PC1",y="PC2",color="PC1") + scale_color_gradientn(colours = rainbow(2)) + coord_flip() + theme
  dev.off()

  #PC2
  filename <- paste(Sys.Date(),"_pca_PC2.pdf", sep="")
  pdf(filename)
  pv <- ggplot(data=d,aes(x=PC1,y=PC2))
  pv + geom_point(aes(x=-PC1,y=-PC2,col=-PC2),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="PC1",y="PC2",color="PC2") + scale_color_gradientn(colours = rainbow(2)) + coord_flip() + theme
  #+geom_text(aes(label=V4), size =3, hjust=1, vjust=0)
  dev.off()





  ## Plot maps
  library(ggplot2)
  #library(ggmap)
  library(maps)
  library(mapdata)

  ## Prepare map of Japan
  japan <- map_data("japan")
  dim(japan)
  head(japan)

  gg1 <- ggplot() +
  geom_polygon(data = japan, aes(x=long, y = lat, group = group), fill = "grey", color = "grey") +
  coord_fixed(1.3)



  ## Plot map colored by latitude and longitude (not used in paper)
  filename <- paste(Sys.Date(),"_map_longitude.pdf", sep="")
  pdf(filename)
  gg.long <- gg1 + geom_point(data=d, aes(x=Longitude,y=Latitude,col=Longitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="Longitude",y="Latitude",color="Longitude") + scale_color_gradientn(colours = rainbow(5)) + theme
  gg.long
  dev.off()

  filename <- paste(Sys.Date(),"_map_latitude.pdf", sep="")
  pdf(filename)
  gg.lat <- gg1 + geom_point(data=d, aes(x=Longitude,y=Latitude,col=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="Longitude",y="Latitude",color="Latitude") + scale_color_gradientn(colours = rainbow(5)) + theme
  gg.lat
  dev.off()


  ## plot map colored by population structure groups
  filename <- paste(Sys.Date(),"_map_pop1.pdf", sep="")
  pdf(filename)
  gg.pop1 <- gg1 + geom_point(data=d, aes(x=Longitude,y=Latitude,col=Pop1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="Longitude",y="Latitude",color="Pop1") + scale_color_gradient(low="white", high="#FF3300") + theme
  gg.pop1
  dev.off()

  filename <- paste(Sys.Date(),"_map_pop2.pdf", sep="")
  pdf(filename)
  gg.pop2 <- gg1 + geom_point(data=d, aes(x=Longitude,y=Latitude,col=Pop2),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="Longitude",y="Latitude",color="Pop2") + scale_color_gradient(low="white", high="#33FF00") + theme
  gg.pop2
  dev.off()

  filename <- paste(Sys.Date(),"_map_pop3.pdf", sep="")
  pdf(filename)
  gg.pop3 <- gg1 + geom_point(data=d, aes(x=Longitude,y=Latitude,col=Pop3),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="Longitude",y="Latitude",color="Pop3") + scale_color_gradient(low="white", high="#0099FF") + theme
  gg.pop3
  dev.off()



  ## plot map colored by PC1 and PC2

  filename <- paste(Sys.Date(),"_map_PC1.pdf", sep="")
  pdf(filename)
  gg.pc1 <- gg1 + geom_point(data=d, aes(x=Longitude,y=Latitude,col=-PC1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="Longitude",y="Latitude",color="PC1") + scale_color_gradientn(colours = rainbow(2)) + theme
  gg.pc1
  dev.off()

  filename <- paste(Sys.Date(),"_map_PC2.pdf", sep="")
  pdf(filename)
  gg.pc2 <- gg1 + geom_point(data=d, aes(x=Longitude,y=Latitude,col=-PC2),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="Longitude",y="Latitude",color="PC2") + scale_color_gradientn(colours = rainbow(2)) + theme
  gg.pc2
  dev.off()


  filename <- paste(Sys.Date(),"_map_PC2.zoom.pdf", sep="")
  pdf(filename, 10, 10)
  gg.pc2.zoom <- gg1 + geom_point(data=d, aes(x=Longitude,y=Latitude,col=-PC2),na.rm=TRUE,size=3,alpha=alpha, shape=16)  +
    labs(x="Longitude",y="Latitude",color="PC2") + scale_color_gradientn(colours = rainbow(2)) + theme
  gg.pc2.zoom
  dev.off()


  # Make interactive plots
  library(plotly)
  i=15
  d.sub <- subset(d, is.finite(d[,i]))
  gwas <- d.sub[,i]
  gwas.name <-colnames(d)[i]
  p.map <- plot_ly(d, x = ~Longitude, y = ~Latitude, type = 'scatter', mode = 'markers',
                   text = ~paste('ID: ', Accession), color = d$Pop2)
  p.map

  p.pca <- plot_ly(d, x = ~-PC2, y = ~-PC1, type = 'scatter', mode = 'markers',
                   text = ~paste('ID: ', Accession), color=d$Pop2)
  p.pca


  ###visualise the geographic - genetic relationships
  ### plot the different groups on the map and in the PCA plot
  #group1: pop1 far south
  group1 <- c("MG022","MG024","MG025", "MG027", "MG028", "MG068", "MG132")
  d.sub1 <- subset(d, Accession %in% group1)
  #group2: pop1 south
  group2 <- c("MG060","MG004","MG067", "MG061")
  d.sub2 <- subset(d, Accession %in% group2)
  # group3: Kyushu east coast
  group3 <- c("MG059","MG133","MG136", "MG135","MG145", "MG058","MG142","MG143","MG144","MG056","MG057","MG063", "MG134","MG138","MG139","MG140","MG105", "MG066", "MG106", "MG104", "MG141", "MG003", "MG065")
  d.sub3 <- subset(d, Accession %in% group3)
  #group4: central Kyushu
  group4 <- c("MG071","MG073","MG110", "MG111", "MG146")
  d.sub4 <- subset(d, Accession %in% group4)
  #group5: Tshushima
  group5 <- c("MG149","MG150","MG152", "MG153", "MG154", "MG155", "MG156")
  d.sub5 <- subset(d, Accession %in% group5)
  #group6: Central Japan
  group6 <- c("MG074", "MG075", "MG008", "MG001", "MG052", "MG131", "MG069", "MG107", "MG115", "MG062", "MG053", "MG070", "MG050", "MG049", "MG130")
  d.sub6 <- subset(d, Accession %in% group6)
  #group7: Northern Japan
  group7 <- c("MG030", "MG032", "MG040", "MG033", "MG120", "MG125", "MG085", "MG044", "MG034", "MG039", "MG045", "MG007", "MG042", "MG096", "MG091", "MG124", "MG046", "MG078", "MG084", "MG094", "MG126", "MG089", "MG093", "MG082", "MG086", "MG103", "MG129", "MG035", "MG127", "MG095", "MG102", "MG038", "MG097", "MG128", "MG023", "MG090", "MG092", "MG083", "MG017", "MG010", "MG088", "MG018", "MG099", "MG098", "MG101", "MG109", "MG100", "MG121", "MG119", "MG011", "MG112", "MG118", "MG081", "MG041", "MG123", "MG080", "MG019", "MG005", "MG113", "MG021", "MG009", "MG026", "MG013", "MG036", "MG076", "Gifu", "MG016", "MG014", "MG116", "MG117", "MG077", "MG051", "MG072", "MG012")
  d.sub7 <- subset(d, Accession %in% group7)

  ## set colors
  col1="#8c510a"
  col2="#bf812d"
  col3="#dfc27d"
  col4="#f6e8c3"
  col5="#35978f"
  col6="#80cdc1"
  col7="#4393c3"


  setwd("")
  gg1 <- ggplot() +
    geom_polygon(data = japan, aes(x=long, y = lat, group = group), fill = "grey", color = "grey") +
    coord_fixed(1.3)

  gg.map <- gg1 +   geom_point(data=d.sub6, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col6) +
                    geom_point(data=d.sub5, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col5) +
                    geom_point(data=d.sub4, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col4) +
                    geom_point(data=d.sub7, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col7) +
                    geom_point(data=d.sub3, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col3) +
                    geom_point(data=d.sub2, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col2) +
                    geom_point(data=d.sub1, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col1)  +
                    labs(x="Longitude",y="Latitude",color="Latitude") + theme
  gg.map
  filename <- paste(Sys.Date(),"_map_groups.png", sep="")
  png(filename, 7, 7, units = "in", res = 200)
  gg.map
  dev.off()


  gg.pca <- ggplot(data=d,aes(x=PC1,y=PC2))
  gg.pca <- gg.pca +
    geom_point(data=d.sub1, aes(x=-PC2,y=-PC1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col1)  +
    geom_point(data=d.sub2, aes(x=-PC2,y=-PC1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col2) +
    geom_point(data=d.sub3, aes(x=-PC2,y=-PC1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col3) +
    geom_point(data=d.sub4, aes(x=-PC2,y=-PC1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col4) +
    geom_point(data=d.sub5, aes(x=-PC2,y=-PC1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col5) +
    geom_point(data=d.sub6, aes(x=-PC2,y=-PC1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col6) +
    geom_point(data=d.sub7, aes(x=-PC2,y=-PC1),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col7) +
    labs(x="-PC1",y="-PC2",color="Latitude") + theme + theme(panel.background = element_rect(fill="#d9d9d9"))
  gg.pca
  filename <- paste(Sys.Date(),"_pca_groups.png", sep="")
  png(filename, 7, 7, units = "in", res = 200)
  gg.pca
  dev.off()



}


## Plot genetic distance graphs
{
  # load library
  library(reshape2)

  # get data
  setwd("")
  gen.dist <- read.delim("20180813_psmc_matrix_list.txt", header=FALSE)
  nrow(gen.dist)

  # eliminate NAs in the diagonal
  gen.dist <- subset(gen.dist, !(V3 == "NA") )
  nrow(gen.dist)

  # cast combination data into a matrix
  gendist.heatmap <- acast(gen.dist, V1 ~ V2, value.var="V3")
  head(gendist.heatmap)
  filename <- paste (Sys.Date(), "gendist_matrix.txt", sep="_")
  write.table(gendist.heatmap, file=filename, row.names =TRUE, col.names=TRUE, sep="\t", quote = FALSE)
  colnames(gendist.heatmap)
  row.names(gendist.heatmap)

  # draw a heatmap
  gendist.heatmap.rev <- gendist.heatmap[dim(gendist.heatmap)[1]:1,]
  heatmap(gendist.heatmap)
  gendist.heatmap.out <- heatmap(gendist.heatmap)

}


## Plot PSMC graphs
{

  ### Plot examples of PSMC curves
  {
  # set graphics parameters
  alpha = 0.8
  font.size = 24
  point.size = 5

  # load libraries
  library(ggplot2)

  ### simple theme with no gridlines
  theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=font.size), legend.text=element_text(size=font.size), axis.title=element_text(size=font.size,face="bold"), legend.title=element_text(size=font.size, face="bold"), legend.position = "none" )

  # list of accessions analysed
  #accessions <- c("Gifu", "MG-001", "MG-005", "MG-008", "MG-010", "MG-019", "MG-027", "MG-036", "MG-042", "MG-049", "MG-053", "MG-063", "MG-068", "MG-076", "MG-081", "MG-085", "MG-089", "MG-101", "MG-111", "MG-113", "MG-128", "MG-130", "MG-146", "MG-154", "MG017", "MG024", "MG028", "MG035", "MG039", "MG069", "MG078", "MG106", "MG116", "MG121", "MG125", "MG138", "MG141", "MG144", "MG-003", "MG-007", "MG-009", "MG-018", "MG-023", "MG-030", "MG-040", "MG-044", "MG-051", "MG-056", "MG-066", "MG-073", "MG-080", "MG-082", "MG-086", "MG-096", "MG-107", "MG-112", "MG-120", "MG-129", "MG-142", "MG-152", "MG013", "MG022", "MG025", "MG034", "MG038", "MG045", "MG074", "MG084", "MG110", "MG117", "MG124", "MG126", "MG140", "MG143")
  accessions <- c("MG-001", "MG-005", "MG-008", "MG-010", "MG-019", "MG-027", "MG-036", "MG-042", "MG-053", "MG-063", "MG-068", "MG-076", "MG-081", "MG-085", "MG-089", "MG-101", "MG-111", "MG-113", "MG-128", "MG-130", "MG-146", "MG-154", "MG017", "MG024", "MG028", "MG035", "MG039", "MG069", "MG078", "MG106", "MG116", "MG121", "MG125", "MG138", "MG141", "MG144", "MG-003", "MG-007", "MG-009", "MG-018", "MG-030", "MG-040", "MG-044", "MG-051", "MG-056", "MG-066", "MG-073", "MG-082", "MG-086", "MG-096", "MG-107", "MG-112", "MG-120", "MG-129", "MG-142", "MG-152", "MG013", "MG022", "MG025", "MG034", "MG038", "MG045", "MG074", "MG084", "MG110", "MG117", "MG124", "MG126", "MG140", "MG143")
  #accessions excluded because of diverging population histories
  excluded <- c("Gifu", "MG-080", "MG-023", "MG-049")
  #excluded <- "test"
  #make regular expression to match excluded accssions
  excluded.pattern <- paste(excluded,collapse="|")
  #get the PSMC output file names
  setwd("")
  file.names <- dir(pattern="*.0.txt")


  ### plot PSMC graphs for each accession against all others
  plots = list()
  #loop over all accessions used
  for (i in 1: length(accessions)) {
  #Subset filenames to get the one versus all comparison
  file.names.sub <- grep(accessions[i],file.names, fixed=TRUE, value =TRUE)
  #Remove excluded accessions
  file.names.excluded <- grep(excluded.pattern, file.names.sub, fixed=FALSE, value=TRUE)
  '%ni%' <- Negate('%in%')
  file.names.sub <- file.names.sub[file.names.sub %ni% file.names.excluded]
  #Get the PSMC data for the filenames in the list
  setwd("")
  j=1
  my_list <- list()
  for (file in file.names.sub) {
    d <- read.delim(file, header=FALSE)
    d$file <- rep(file, nrow(d))
    my_list[[j]] <- d
    j=j+1
  }
  #combine in one big data frame
  big_data = do.call(what = rbind, args = my_list)
  nrow(big_data)
  d.sub <- big_data
  #produce the PSMC curve plot
  gg <- ggplot(data=d.sub, aes(x=V1, y=V2, color=file)) + geom_line() +
    scale_x_continuous(breaks=c(1e1,1e2,1e3,1e4,1e5,1e6),trans='log10') + scale_y_continuous(trans='log10')+ geom_hline(yintercept = 5) + annotation_logticks(sides = "lb") + theme
  plots[[i]] <- gg
  #save the plot
  setwd("")
  filename <- paste(Sys.Date(), "_", accessions[i], "_PSMC_curves.pdf", sep="")
  pdf(filename, 12, 8)
  print(plots[[i]])
  dev.off()
  }

}

#   ## plot PSMC graphs for the main figure
  {
    ## set colors
    col1="#8c510a"
    col2="#bf812d"
    col3="#dfc27d"
    col4="#f6e8c3"
    col5="#35978f"
    col6="#80cdc1"



  # First a custom plot for MG-063
  {
  # list of accessions analysed
  accessions <- c("MG-063")
  #accessions excluded because of diverging population histories
  excluded <- c("Gifu", "MG-080", "MG-023", "MG-049")
  #make regular expression to match excluded accssions
  excluded.pattern <- paste(excluded,collapse="|")
  setwd("")
  file.names <- dir(pattern="*.0.txt")


  ### plot PSMC graphs for each accession against all others
  plots = list()
  #loop over all accessions used

    #Subset filenames to get the one versus all comparison
    file.names.sub <- grep(accessions,file.names, fixed=TRUE, value =TRUE)
    #Remove excluded accessions
    file.names.excluded <- grep(excluded.pattern, file.names.sub, fixed=FALSE, value=TRUE)
    '%ni%' <- Negate('%in%')
    file.names.sub <- file.names.sub[file.names.sub %ni% file.names.excluded]
    #Get the PSMC data for the filenames in the list
    setwd("")
    j=1
    my_list <- list()
    for (file in file.names.sub) {
      d <- read.delim(file, header=FALSE)
      d$file <- rep(file, nrow(d))
      my_list[[j]] <- d
      j=j+1
    }
    #combine in one big data frame
    big_data = do.call(what = rbind, args = my_list)
    nrow(big_data)
    d.sub <- big_data
    #produce the PSMC curve plot
    group1 <- c("MG024", "MG-027", "MG-068", "MG025", "MG028", "MG022")
    group1.pattern <- paste(group1,collapse="|")
    d.sub1 <- d.sub[grep(group1.pattern, d.sub$file),]
    group2 <- c("MG138", "MG056", "MG142", "MG144", "MG143", "MG063", "MG140", "MG141", "MG106", "MG066", "MG074", "MG003")
    group2.pattern <- paste(group2,collapse="|")
    d.sub2 <- d.sub[grep(group2.pattern, d.sub$file),]
    group3 <- c("MG126", "MG085", "MG125", "MG124", "MG045", "MG084", "MG082", "MG044", "MG086", "MG007", "MG010", "MG129", "MG042", "MG009", "MG039", "MG040", "MG038", "MG035", "MG096", "MG034", "MG030")
    group3.pattern <- paste(group3,collapse="|")
    d.sub3 <- d.sub[grep(group3.pattern, d.sub$file),]

    color_scale <- c("MG-063.sub_sampling.MG-027.sub_sampling.fq.gz.0.txt" = col1,
                     "MG-063.sub_sampling.MG-027.sub_sampling.fq.gz.0.txt" = col1,
                     "MG-063.sub_sampling.MG-068.sub_sampling.fq.gz.0.txt"  = col1,
                     "MG-063.sub_sampling.MG022.fq.gz.0.txt" = col1,
                     "MG-063.sub_sampling.MG024.fq.gz.0.txt" = col1,
                     "MG-063.sub_sampling.MG025.fq.gz.0.txt" = col1,
                     "MG-063.sub_sampling.MG028.fq.gz.0.txt" = col1,
                     "MG-063.sub_sampling.MG074.fq.gz.0.txt" = col3,
                     "MG-063.sub_sampling.MG106.fq.gz.0.txt" = col3,
                     "MG-063.sub_sampling.MG138.fq.gz.0.txt" = col3,
                     "MG-063.sub_sampling.MG140.fq.gz.0.txt" = col3,
                     "MG-063.sub_sampling.MG141.fq.gz.0.txt" = col3,
                     "MG-063.sub_sampling.MG143.fq.gz.0.txt" = col3,
                     "MG-063.sub_sampling.MG144.fq.gz.0.txt" = col3,
                     "MG-063.sub_sampling.MG035.fq.gz.0.txt" = "light blue",
                     "MG-063.sub_sampling.MG038.fq.gz.0.txt" = "light blue",
                     "MG-063.sub_sampling.MG039.fq.gz.0.txt" = "light blue",
                     "MG-063.sub_sampling.MG045.fq.gz.0.txt" = "light blue",
                     "MG-063.sub_sampling.MG084.fq.gz.0.txt" = "light blue",
                     "MG-063.sub_sampling.MG124.fq.gz.0.txt" = "light blue",
                     "MG-063.sub_sampling.MG125.fq.gz.0.txt" = "light blue",
                     "MG-063.sub_sampling.MG126.fq.gz.0.txt" = "light blue",
                     "MG034.sub_sampling.MG-063.sub_sampling.fq.gz.0.txt" = "light blue"
                     )

    gg <- ggplot() + geom_line(data=d.sub1, aes(x=V1, y=V2, color=file)) + geom_line(data=d.sub2, aes(x=V1, y=V2, color=file)) + geom_line(data=d.sub3, aes(x=V1, y=V2, color=file)) +
      scale_color_manual(values=color_scale) +
      scale_x_continuous(breaks=c(1e1,1e2,1e3,1e4,1e5,1e6),trans='log10') + scale_y_continuous(trans='log10')+ geom_hline(yintercept = 5) + annotation_logticks(sides = "lb") + theme +
      theme(legend.position="none") + xlim(500,1e6)
    gg

    #save the plot
    setwd("")
    filename <- paste(Sys.Date(), "_", accessions, "_PSMC_curves.pdf", sep="")
    pdf(filename, 12, 8)
    gg
    dev.off()
  }

  # Then for  for MG-126
  {
      # list of accessions analysed
      accessions <- c("MG126")
      #accessions excluded because of diverging population histories
      excluded <- c("Gifu", "MG-080", "MG-023", "MG-049")
      #make regular expression to match excluded accssions
      excluded.pattern <- paste(excluded,collapse="|")
      setwd("")
      file.names <- dir(pattern="*.0.txt")


      ### plot PSMC graphs for each accession against all others
      plots = list()

      #Subset filenames to get the one versus all comparison
      file.names.sub <- grep(accessions,file.names, fixed=TRUE, value =TRUE)
      #Remove excluded accessions
      file.names.excluded <- grep(excluded.pattern, file.names.sub, fixed=FALSE, value=TRUE)
      '%ni%' <- Negate('%in%')
      file.names.sub <- file.names.sub[file.names.sub %ni% file.names.excluded]
      #Get the PSMC data for the filenames in the list
      setwd("")
      j=1
      my_list <- list()
      for (file in file.names.sub) {
        d <- read.delim(file, header=FALSE)
        d$file <- rep(file, nrow(d))
        my_list[[j]] <- d
        j=j+1
      }
      #combine in one big data frame
      big_data = do.call(what = rbind, args = my_list)
      nrow(big_data)
      d.sub <- big_data
      #produce the PSMC curve plot
      group1 <- c("MG024", "MG-027", "MG-068", "MG025", "MG028", "MG022")
      group1.pattern <- paste(group1,collapse="|")
      d.sub1 <- d.sub[grep(group1.pattern, d.sub$file),]
      group2 <- c("MG138", "MG056", "MG142", "MG144", "MG143", "MG063", "MG140", "MG141", "MG106", "MG066", "MG074", "MG003")
      group2.pattern <- paste(group2,collapse="|")
      d.sub2 <- d.sub[grep(group2.pattern, d.sub$file),]
      group3 <- c("MG085", "MG125", "MG124", "MG045", "MG084", "MG082", "MG044", "MG086", "MG007", "MG010", "MG129", "MG042", "MG009", "MG039", "MG040", "MG038", "MG035", "MG096", "MG034", "MG030")
      group3.pattern <- paste(group3,collapse="|")
      d.sub3 <- d.sub[grep(group3.pattern, d.sub$file),]

      color_scale <- c("MG-027.sub_sampling.MG126.fq.gz.0.txt" = col1,
                       "MG-068.sub_sampling.MG126.fq.gz.0.txt" = col1,
                       "MG022.MG126.fq.gz.0.txt" = col1,
                       "MG024.MG126.fq.gz.0.txt" = col1,
                       "MG025.MG126.fq.gz.0.txt" = col1,
                       "MG028.MG126.fq.gz.0.txt" = col1,
                       "MG074.MG126.fq.gz.0.txt" = col3,
                       "MG106.MG126.fq.gz.0.txt" = col3,
                       "MG138.MG126.fq.gz.0.txt" = col3,
                       "MG140.MG126.fq.gz.0.txt" = col3,
                       "MG141.MG126.fq.gz.0.txt" = col3,
                       "MG143.MG126.fq.gz.0.txt" = col3,
                       "MG144.MG126.fq.gz.0.txt" = col3,
                       "MG034.sub_sampling.MG126.fq.gz.0.txt" = "light blue",
                       "MG035.MG126.fq.gz.0.txt" = "light blue",
                       "MG038.MG126.fq.gz.0.txt" = "light blue",
                       "MG039.MG126.fq.gz.0.txt" = "light blue",
                       "MG045.MG126.fq.gz.0.txt" = "light blue",
                       "MG084.MG126.fq.gz.0.txt" = "light blue",
                       "MG124.MG126.fq.gz.0.txt" = "light blue",
                       "MG125.MG126.fq.gz.0.txt" = "light blue"
      )

      gg <- ggplot() + geom_line(data=d.sub1, aes(x=V1, y=V2, color=file)) + geom_line(data=d.sub2, aes(x=V1, y=V2, color=file)) + geom_line(data=d.sub3, aes(x=V1, y=V2, color=file)) +
        scale_color_manual(values=color_scale) +
        scale_x_continuous(breaks=c(1e1,1e2,1e3,1e4,1e5,1e6),trans='log10') + scale_y_continuous(trans='log10')+ geom_hline(yintercept = 5) + annotation_logticks(sides = "lb") + theme +
        theme(legend.position="none")
      gg

      #save the plot
      setwd("")
      filename <- paste(Sys.Date(), "_", accessions, "_PSMC_curves.pdf", sep="")
      pdf(filename, 12, 8)
      gg
      dev.off()
    }

  }



#   ## Generate a big matrix of all the pairwise comparisons
  {
  # load library
  library(reshape2)

  # get data
  setwd("")
  psmc.comb <- read.delim("20180831_all_psmc_comb_ids_double.txt", header=FALSE)
  nrow(psmc.comb)

  # cast combination data into a matrix
  psmc.heatmap <- acast(psmc.comb, V1 ~ V2, value.var="V3")
  filename <- paste (Sys.Date(), "psmc_matrix.txt", sep="_")
  write.table(psmc.heatmap, file=filename, row.names =TRUE, col.names=TRUE, sep="\t", quote = FALSE)
  colnames(psmc.heatmap)
  row.names(psmc.heatmap)

  # draw a heatmap
  heatmap(psmc.heatmap)

  # calculate a distance matrix and cluster
  psmc.dist <- as.matrix(dist(psmc.heatmap))
  filename <- paste (Sys.Date(), "dist_matrix.txt", sep="_")
  write.table(psmc.dist, file=filename, row.names =TRUE, col.names=TRUE, sep="\t", quote = FALSE)
  clustered <-hclust(as.dist(psmc.dist), method = "single")

  ## plot divergence time versus geographic distance
  head(psmc.comb)
  setwd("")
  g.dist <- read.delim("20180813_psmc_matrix_list.txt", header=FALSE)
  nrow(g.dist)
  head(g.dist)
  g.dist$key <- paste(g.dist$V1, g.dist$V2, sep="_")
  psmc.comb$key <- paste(psmc.comb$V1, psmc.comb$V2, sep="_")
  psmc.geo.merge <- merge(psmc.comb, g.dist, by="key")
  head(psmc.geo.merge, 100)
  plot(psmc.geo.merge$V3.x,psmc.geo.merge$V3.y, log="x")
  psmc.geo.merge.sub <- subset(psmc.geo.merge, V1.x %in% c("MG152","MG154") | V2.x %in% c("MG152","MG154"))
  points(psmc.geo.merge.sub$V3.x, psmc.geo.merge.sub$V3.y, col="red")
  }

  ## Plot the relevant accessions on the map colored by pop structure
  {
  ## read accession metadata
    setwd("")
    d <- read.delim("20181102_136LjAccessionData.txt")

  # set graphics parameters
  alpha = 0.8
  font.size = 24
  point.size = 7

  # load libraries
  library(ggplot2)
  #library(ggmap)
  library(maps)
  library(mapdata)

  ### simple theme with no gridlines
  theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=font.size), legend.text=element_text(size=font.size), axis.title=element_text(size=font.size,face="bold"), legend.title=element_text(size=font.size, face="bold"), legend.position = "none" )

  ## generate that map of Japan
  japan <- map_data("japan")
  dim(japan)
  head(japan)

  gg1 <- ggplot() +
    geom_polygon(data = japan, aes(x=long, y = lat, group = group), fill = "grey", color = "grey") +
    coord_fixed(1.3)
  gg1


  ### plot the different groups on the map
  pop1 <- c("MG024","MG027","MG068","MG025","MG028","MG022")
  d.sub <- subset(d, Accession %in% pop1)
  gg.pop1 <- gg1 + geom_point(data=d.sub, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col1)  +
    labs(x="Longitude",y="Latitude",color="Pop1") + scale_color_gradient(low="white", high="#FF3300", limits=c(0,1)) + theme
  gg.pop1
  filename <- paste(Sys.Date(),"_pop1_psmc.png", sep="")
  setwd("")
  png(filename, 5, 5, units = "in", res = 200)
  gg.pop1
  dev.off()


  group3 <- c("MG138","MG056","MG142","MG144","MG143","MG063", "MG140", "MG141", "MG106", "MG066", "MG003")
  group6 <- c("MG074")
  d.sub3 <- subset(d, Accession %in% group3)
  d.sub6 <- subset(d, Accession %in% group6)
  gg.south.east <- gg1 +
                  geom_point(data=d.sub3, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col3)  +
                  geom_point(data=d.sub6, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col6)  +
                  labs(x="Longitude",y="Latitude",color="Pop2") + scale_color_gradient(low="white", high="#29cc00",limits=c(0,1)) + theme
  gg.south.east
  filename <- paste(Sys.Date(),"_south.east_psmc.png", sep="")
  setwd("")
  png(filename, 5, 5, units = "in", res = 200)
  gg.south.east
  dev.off()


  south <- c("MG001","MG069","MG053","MG008","MG130","MG053")
  d.sub <- subset(d, Accession %in% south)
  gg.south <- gg1 + geom_point(data=d.sub, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col6)  +
    labs(x="Longitude",y="Latitude",color="Pop2") + scale_color_gradient(low="white", high="#29cc00",limits=c(0,1)) + theme
  gg.south
  filename <- paste(Sys.Date(),"_south_psmc.png", sep="")
  setwd("")
  png(filename, 5, 5, units = "in", res = 200)
  gg.south
  dev.off()


  central.pop2 <- c("MG110","MG111")
  central.pop3 <- c("MG112")
  d.sub.pop2 <- subset(d, Accession %in% central.pop2)
  d.sub.pop3 <- subset(d, Accession %in% central.pop3)
  gg.central <- gg1 + geom_point(data=d.sub.pop2, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col4)  +
                      geom_point(data=d.sub.pop3, aes(x=Longitude,y=Latitude), col="#0099FF",na.rm=TRUE,size=point.size,alpha=alpha, shape=16) +
                      labs(x="Longitude",y="Latitude",color="Pop2") + scale_color_gradient(low="white", high="#29cc00",limits=c(0,1))  + theme
  gg.central
  filename <- paste(Sys.Date(),"_central_psmc.png", sep="")
  setwd("")
  png(filename, 5, 5, units = "in", res = 200)
  gg.central
  dev.off()


  tsushima <- c("MG152","MG154")
  d.sub <- subset(d, Accession %in% tsushima)
  gg.tsushima <- gg1 + geom_point(data=d.sub, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col5)  +
    labs(x="Longitude",y="Latitude",color="Pop2") + scale_color_gradient(low="white", high="#FF3300", limits=c(0,1)) + theme
  gg.tsushima
  filename <- paste(Sys.Date(),"_tsushima_psmc.png", sep="")
  setwd("")
  png(filename, 5, 5, units = "in", res = 200)
  gg.tsushima
  dev.off()

  #pop3
  pop3 <- c("MG076" , "MG017" , "MG018" , "MG019" , "MG013" , "MG005" , "MG078" , "MG116" , "MG128" , "MG121" , "MG120" , "MG081" , "MG126" , "MG085" , "MG125" , "MG124" , "MG045" , "MG084" , "MG082" , "MG044" , "MG086" , "MG007" , "MG010" , "MG129" , "MG042" , "MG009" , "MG039" , "MG040" , "MG038" , "MG035" , "MG096" , "MG034" , "MG030")
  d.sub <- subset(d, Accession %in% pop3)
  gg.pop3 <- gg1 + geom_point(data=d.sub, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col="#0099FF")  +
    labs(x="Longitude",y="Latitude",color="Pop2") + scale_color_gradient(low="white", high="#0099FF", limits=c(0,1)) + theme
  gg.pop3
  filename <- paste(Sys.Date(),"_pop3_psmc.png", sep="")
  setwd("")
  png(filename, 5, 5, units = "in", res = 200)
  gg.pop3
  dev.off()


  }

#   ## Plot the relevant accessions on the PCA chart colored by pop structure
  {
  setwd("")
  d <- read.delim("20180612_137LjAcessionData.txt")

  # set graphics parameters
  alpha = 0.8
  font.size = 24
  point.size = 7

  # load libraries
  library(ggplot2)

  # prepare the basic PCA plot with white dots
  gg1 <- ggplot(data=d,aes(x=-PC1,y=-PC2))
  gg1 <- gg1 + geom_point(aes(x=-PC1,y=-PC2,col=Pop1), col="white",na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="PC1",y="PC2") + coord_flip() + theme
  gg1

  # Add the central Kyushu and Tshushima lines to the plot
  central.pop2 <- c("MG110","MG111")
  central.pop3 <- c("MG112")
  tsushima <- c("MG152","MG154")
  d.sub.pop2 <- subset(d, Accession %in% central.pop2)
  d.sub.pop3 <- subset(d, Accession %in% central.pop3)
  d.tsushima <- subset(d, Accession %in% tsushima)
  gg.central <- gg1 + geom_point(data=d.sub.pop2, aes(x=-PC1,y=-PC2, col=Pop2),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
    labs(x="PC1",y="PC2",color="Pop2") + scale_color_gradient(low="white", high="#29cc00",limits=c(0,1)) + geom_point(data=d.sub.pop3, aes(x=-PC1,y=-PC2), col="#0099FF",na.rm=TRUE,size=point.size,alpha=alpha, shape=16) + geom_point(data=d.tsushima, aes(x=-PC1,y=-PC2), col="#ff9980",na.rm=TRUE,size=point.size,alpha=alpha, shape=16) + theme
  gg.central
  filename <- paste(Sys.Date(),"_central_tsushima_PCA.png", sep="")
  setwd("")
  png(filename, 5, 5, units = "in", res = 200)
  gg.central
  dev.off()
  }



}


### Plot and compare GWAS phenotype data

{
  ## read data
  setwd("")
  #d <- read.delim("20180718_metadata.txt")
  d <- read.delim("20190410_metadata.txt")
  colnames(d)
  
  ## load libraries
  library(ggplot2)
  
  # set graphics parameters
  alpha=0.6
  font.size = 20
  point.size = 5
  
  
  ### simple theme with no gridlines
  theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=font.size), legend.text=element_text(size=font.size), axis.title=element_text(size=font.size,face="bold"), legend.title=element_text(size=font.size, face="bold"), legend.background = element_rect(fill="transparent"), legend.position = c(0.2,0.8) )
  
  ## Plot map colored by gwas
  library(ggplot2)
  library(maps)
  library(mapdata)
  
  japan <- map_data("japan")
  dim(japan)
  head(japan)
  
  gg1 <- ggplot() +
    geom_polygon(data = japan, aes(x=long, y = lat, group = group), fill = "grey", color = "grey") + 
    coord_fixed(1.3)
  
  
  
  ## plot PCA colored by GWAS data
  plots.pca = list()
  plots.map = list()
  
  for (i in 22:ncol(d) ){
    d.sub <- subset(d, is.finite(d[,i]))
    gwas <- d.sub[,i]
    gwas.name <-colnames(d)[i]
    
    pv <- ggplot(data=d.sub,aes(x=PC1,y=PC2))
    pv <- pv + geom_point(aes(x=-PC1,y=-PC2,col=gwas),na.rm=TRUE,size=point.size,alpha=alpha, shape=16)  +
      labs(x="PC1",y="PC2",color=gwas.name) + scale_color_gradientn(colours = rainbow(2),na.value="light grey") + coord_flip() + theme
    plots.pca[[i]] = pv
    
    
    filename <- paste(Sys.Date(),"_",gwas.name,"_pca.pdf", sep="")
    pdf(filename)
    print(plots.pca[[i]])
    dev.off()
    
    filename <- paste(Sys.Date(),"_", gwas.name, "_map.pdf", sep="")
    gg.long <- gg1 + geom_point(data=d.sub, aes(x=Longitude,y=Latitude,col=d.sub[,i]),size=point.size,alpha=alpha, shape=16)  +
      labs(x="Longitude",y="Latitude",color=gwas.name) + scale_color_gradientn(colours = rainbow(2), na.value="light grey")  + theme
    plots.map[[i]] <- gg.long
    pdf(filename)
    print(plots.map[[i]])
    dev.off()  
    
  }
  
  # Make interactive plots
  library(plotly)
  i=15
  d.sub <- subset(d, is.finite(d[,i]))
  gwas <- d.sub[,i]
  gwas.name <-colnames(d)[i]
  p.map <- plot_ly(d.sub, x = ~Longitude, y = ~Latitude, type = 'scatter', mode = 'markers',
                   text = ~paste('ID: ', Accession, d.sub[,i]), color = d.sub[,i])
  p.map
  
  p.pca <- plot_ly(d.sub, x = ~-PC2, y = ~-PC1, type = 'scatter', mode = 'markers',
                   text = ~paste('ID: ', Accession, d.sub[,i]), color = d.sub[,i])
  p.pca
  
  
  ## check the correlations
  library(corrgram)
  setwd("")
  filename <- paste(Sys.Date(),"_", "_corrgram.png", sep="")
  png(filename,18,18, units="in", res=200)
  corrgram(d.sub[,c(11,12,16:18,22:27,29:ncol(d.sub))], lower.panel=panel.pts, upper.panel=panel.cor)
  dev.off()
  
  
  ### plot winter survival by year 
  {
    # subset to get only non-admixed individuals
    d.sub <- subset(d, (pop1==1 | pop2==1 | pop3==1))
    
    # generate new pop column
    pop=NULL
    for (i in 1:nrow(d.sub)) {
      if(d.sub$pop1[i]>d.sub$pop2[i] & d.sub$pop1[i]>d.sub$pop3[i]) {pop[i] <- 1} else if (d.sub$pop2[i]>d.sub$pop1[i] & d.sub$pop2[i]>d.sub$pop3[i]) {
        pop[i] <- 2} else if (d.sub$pop3[i]>d.sub$pop1[i] & d.sub$pop3[i]>d.sub$pop2[i]) {pop[i] <- 3}
    }
    
    d.sub$pop <- pop
    head(d.sub)
    
    ## theme
    library(ggplot2)
    {
      t <- theme(
        # Text
        text = element_text(
          #family = "Arial",
        ),
        
        # Plot title
        plot.title = element_text(size=rel(1.5), face="bold", vjust=2),
        plot.margin = unit(c(1.5,1.5,1.5,1.5),"lines"),
        
        # Panel
        panel.background = element_rect(fill = "#ffffff"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.margin = unit(1, "lines"),
        
        # Legend
        legend.position = "bottom",
        legend.key = element_rect(colour = "#000000"),
        
        # Facet
        strip.background = element_rect(fill = "#ffffff"),
        strip.text = element_text(
          size = rel(1)
        ),
        
        # Axis
        axis.text = element_text(
          color = "#000000",
          size = rel(1)
        ),
        #axis.ticks.x = element_blank(),
        #axis.ticks.y = element_blank(),
        axis.title = element_text(
          size = rel(1.2)
          #family = "Arial",
        )
        #axis.line.x = element_line(color="black", size = .5)
      )
    }
    
    
    # generate boxplots overlaid with jittered dots
    {
      size=6
      space=5
      box.width=.7
      alpha.jitter=0.8
      alpha.box=0.3
      
      # p1
      p1 <- ggplot(
        data = d.sub,
        aes(
          y = OW_2014,
          x = pop,
          group = pop
        )
      )  +
        geom_jitter(
          aes(
            color = factor(pop)
          ),
          size=size,
          alpha=alpha.jitter,
          position = position_jitter(width = .05),
          shape=16
        ) +
        geom_boxplot(
          outlier.shape = NA,
          width=box.width,
          alpha=alpha.box,
          fill=NA,
          notch=FALSE,
          color="dark grey",
          coef=0 #removes whiskers
        ) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(
          y = 'Survival',
          x = 'pop'
        )
      
      p1 <- p1 + t + 
        xlim(0.5,18.5) +
        ylim(-0.05,1.05)
      
      
      p2 <- p1  + 
        geom_jitter(data = d.sub,
                    aes(
                      y = OW_2015,
                      x = pop+space,
                      group = pop,
                      color = factor(pop)
                    ),
                    shape=16,
                    size=size,
                    alpha=alpha.jitter,
                    position = position_jitter(width = .05)
                    #width=0.2
        ) +
        geom_boxplot(data = d.sub,
                     aes(
                       y = OW_2015,
                       x = pop+space),
                     outlier.shape = NA,
                     width=box.width,
                     alpha=alpha.box,
                     fill=NA,
                     notch=FALSE,
                     color="dark grey",
                     coef=0 #removes whiskers
        )
      
      p3 <- p2  + 
        geom_jitter(data = d.sub,
                    aes(
                      y = OW_2016,
                      x = pop+2*space,
                      group = pop,
                      color = factor(pop)
                    ),
                    shape=16,
                    size=size,
                    alpha=alpha.jitter,
                    position = position_jitter(width = .05)
        ) +
        geom_boxplot(data = d.sub,
                     aes(
                       y = OW_2016,
                       x = pop+2*space),
                     outlier.shape = NA,
                     width=box.width,
                     alpha=alpha.box,
                     fill=NA,
                     notch=FALSE,
                     color="dark grey",
                     coef=0 #removes whiskers
        )
      
      
      p4 <- p3  + 
        geom_jitter(data = d.sub,
                    aes(
                      y = OW_2017,
                      x = pop+3*space,
                      group = pop,
                      color = factor(pop)
                    ),
                    shape=16,
                    size=size,
                    alpha=alpha.jitter,
                    position = position_jitter(width = .05)
        ) +
        geom_boxplot(data = d.sub,
                     aes(
                       y = OW_2017,
                       x = pop+3*space),
                     outlier.shape = NA,
                     width=box.width,
                     alpha=alpha.box,
                     fill=NA,
                     notch=FALSE,
                     color="dark grey",
                     coef=0 #removes whiskers
        )
      
    }  
    
    ## save the plot
    ## mean on top, without whiskers! Wider, field pic as insert on map.
    setwd("/Users/au27857/Dropbox/NirajShah/AccessionPaper/31082016/Manuscript/Figures/_20180614/miyazaki_phenotypes")
    filename <- paste(Sys.Date(),"survivalByPopAndYear.pdf", sep="_")
    pdf(filename,
        width=12,
        height=6)
    print(p4)
    dev.off()
    
  }
  
  
    
  ### Test for significant differences in winter survival between the populations
  ### taking into account field placement effects
   {
    ## read the data
    #setwd("/Users/au27857/Dropbox/NirajShah/AccessionPaper/31082016/Manuscript/20190821_shusei_data_request/20190829")
    #d.ind <- read.delim("20190829_all_data_including_field_placement.txt")
    
    setwd("/Users/au27857/Dropbox/NirajShah/AccessionPaper/31082016/Manuscript/20190821_shusei_data_request/20191111")
    d.ind <- read.delim("20191111_all_data_including_field_placement.txt")
    
      
    # add a survival column
    colnames(d.ind)
    d.ind$surv <- d.ind$Year.1.status + d.ind$Year.2.status
    head(d.ind)
    d.ind.filt <- subset(d.ind, Year.1.status ==1)
    head(d.ind.filt)
    nrow(d.ind)
    nrow(d.ind.filt)
    
    
    library(lme4)
    library(lmerTest)
    library(multcomp)
    
    
    
    ## generalized linear model for all accessions
    {

  
    # Test for significance of population membership
    ## read data
    setwd("/Users/au27857/Dropbox/NirajShah/AccessionPaper/31082016/Manuscript/20180605_stig_analysis/20180718_phenotype_plots")
    d <- read.delim("20190410_metadata.txt")
    colnames(d)
    setwd("/Users/au27857/Dropbox/NirajShah/AccessionPaper/31082016/Manuscript/20190821_shusei_data_request/20190829")
    
    # subset to get only non-admixed individuals
    d.sub <- subset(d, (pop1==1 | pop2==1 | pop3==1))
    
    # generate new pop column
    pop=NULL
    for (i in 1:nrow(d.sub)) {
      if(d.sub$pop1[i]>d.sub$pop2[i] & d.sub$pop1[i]>d.sub$pop3[i]) {pop[i] <- 1} else if (d.sub$pop2[i]>d.sub$pop1[i] & d.sub$pop2[i]>d.sub$pop3[i]) {
        pop[i] <- 2} else if (d.sub$pop3[i]>d.sub$pop1[i] & d.sub$pop3[i]>d.sub$pop2[i]) {pop[i] <- 3}
    }
    
    d.sub$pop <- pop
    head(d.sub)
    
    
    
    #merge
    d.ind.merged.1 <- merge(d.ind.filt, d.sub, by="Accession")
    head(d.ind.merged.1)
    colnames(d.ind.merged.1)
    d.ind.merged <- d.ind.merged.1[,c(1:11,56)]
    head(d.ind.merged)
    
    # add a column with blocks - one block (grid+year) for each of the thre plant blocks
    d.ind.merged$grid_year <- paste(d.ind.merged$Planting.year, d.ind.merged$Field.Column, d.ind.merged$Field.row, sep="_") 
    head(d.ind.merged)
    
    
    # add a column with anonymized accession nested by population
    
    # first order by population, accession, and field position
    d.ind.merged.sorted <- d.ind.merged[order(d.ind.merged$pop, d.ind.merged$Accession, d.ind.merged$Field.Column, d.ind.merged$Field.row),]
    head(d.ind.merged.sorted)
    tail(d.ind.merged.sorted)
    
    
    # loop over populations
    accession.number <- list(list(), list(), list())
    
    for (p in 1:3) {
    
      # loop over accessions
      d.pop <- subset(d.ind.merged, pop==p)
      accession.list <- levels(factor(d.pop$Accession))
      n=1
      
      for (i in accession.list) {
        subset(d.pop, Accession == i)
        accession.number[[p]] <- c(accession.number[[p]], list(rep(n, nrow(subset(d.pop, Accession == i)))))
        #accession.number[[n]] <- rep(n, nrow(subset(d.pop1, Accession == i)))
        n=n+1
      }
    
    }
    
    #checks
    accession.number
    length(unlist(accession.number))
    nrow(d.ind.merged.sorted)
    
    # add the accession by population column
    d.ind.merged.sorted$acc.by.pop <- unlist(accession.number)
    head(d.ind.merged.sorted, 20)
    
    
## add a block by accession column
#first sort by accession and grid    
d.ind.merged.sorted <- d.ind.merged.sorted[order(d.ind.merged.sorted$Accession, d.ind.merged.sorted$grid_year),]
head(d.ind.merged.sorted)    


# loop over accessions
accession.list <- levels(factor(d.ind.merged.sorted$Accession))

#block.number <- list(rep(list(NA), length(accession.list)))

block.number <- NULL

for (a in accession.list) {
  
  # loop over accessions
  d.acc <- subset(d.ind.merged.sorted, Accession==a)
  block.list <- levels(factor(d.acc$grid_year))
  n=1
  
  for (i in block.list) {
    subset(d.acc, grid_year == i)
    block.number[[a]] <- c(block.number[[a]], list(rep(n, nrow(subset(d.acc, grid_year == i)))))
    #accession.number[[n]] <- rep(n, nrow(subset(d.pop1, Accession == i)))
    n=n+1
  }
  
}
    
    #checks
    head(block.number)
    head(unlist(block.number))
    length(unlist(block.number))
    nrow(d.ind.merged.sorted)
    
    # add the block by accession column
    d.ind.merged.sorted$block.by.acc <- unlist(block.number)
    head(d.ind.merged.sorted, 20)
    
    
    # rename data frame
    d.ind.merged <- d.ind.merged.sorted 
    
    
    d.ind.merged.tohoku <- subset(d.ind.merged, Location =="Tohoku")
    nrow(d.ind.merged.tohoku)
    head(d.ind.merged.tohoku)
    length(table(droplevels(d.ind.merged.tohoku$Accession)))
    
    #factorize  
    d.ind.merged.tohoku$pop <- factor(d.ind.merged.tohoku$pop)
    d.ind.merged.tohoku$Field.row <- factor(d.ind.merged.tohoku$Field.row)
    d.ind.merged.tohoku$Field.Column <- factor(d.ind.merged.tohoku$Field.Column)
    d.ind.merged.tohoku$grid_year <- factor(d.ind.merged.tohoku$grid_year)
    d.ind.merged.tohoku$Block <- factor(d.ind.merged.tohoku$Block)
    d.ind.merged.tohoku$acc.by.pop <- factor(d.ind.merged.tohoku$acc.by.pop)
    d.ind.merged.tohoku$block.by.acc <- factor(d.ind.merged.tohoku$block.by.acc)
    str(d.ind.merged.tohoku)
    
    #check mean survival rates
    survival.table.tohoku <-table(d.ind.merged.tohoku$surv, d.ind.merged.tohoku$pop)
    pop3.survival.rate <- survival.table.tohoku[2,3] / (survival.table.tohoku[2,3] + survival.table.tohoku[1,3])
    non_pop3.survival.rate <- (survival.table.tohoku[2,1] + survival.table.tohoku[2,2] ) /
                            (survival.table.tohoku[2,1] + survival.table.tohoku[2,2] + survival.table.tohoku[1,1] + survival.table.tohoku[1,2])
    
    #check effect of row and column placements
    {
      # model with random effects
      mod.lmer <- lmer(surv ~ (1|pop) + (1|Field.row) + (1|Field.Column) + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc), data=d.ind.merged.tohoku)
          
      #extract the summary data and calculate variance explained
      var.expl <- as.data.frame(VarCorr(mod.lmer))
      var.all <- var.expl$vcov
      var.fractions <- var.all / sum(var.expl$vcov)
      her.summary <- as.data.frame(cbind(var.expl$grp, round(var.fractions, 8)))
      colnames(her.summary) <- c("variable", "var.explained")
      her.summary
      
      # save the table
      filename <- paste(Sys.Date(), "row_column_effect_Tohoku.txt", sep="_")
      write.table(her.summary, file = filename, row.names = FALSE, quote = FALSE, sep="\t")
    }
    
    
    # lmer model - all years
    mod.lmer <- lmer(surv ~ pop + (1|Field.row) + (1|Field.Column) + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc), data=d.ind.merged.tohoku)
    summary(mod.lmer)
    anova(mod.lmer)
    summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    lmer.all.tohoku.2014_16 <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    
    # lmer model - 2014
    d.ind.merged.tohoku.2014 <- subset(d.ind.merged.tohoku, Planting.year==2014)
    mod.lmer <- lmer(surv ~ pop + (1|Field.row) + (1|Field.Column) + (1|acc.by.pop) + (1|acc.by.pop:block.by.acc), data=d.ind.merged.tohoku.2014)
    summary(mod.lmer)
    anova(mod.lmer)
    summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    lmer.all.tohoku.2014 <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
     
    # lmer model - 2015
    d.ind.merged.tohoku.2015 <- subset(d.ind.merged.tohoku, Planting.year==2015)
    mod.lmer <- lmer(surv ~ pop + (1|Field.row) + (1|Field.Column) + (1|acc.by.pop) + (1|acc.by.pop:block.by.acc), data=d.ind.merged.tohoku.2015)
    summary(mod.lmer)
    anova(mod.lmer)
    summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    lmer.all.tohoku.2015 <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    
    # lmer model - 2016
    d.ind.merged.tohoku.2016 <- subset(d.ind.merged.tohoku, Planting.year==2016)
    mod.lmer <- lmer(surv ~ pop + (1|Field.row) + (1|Field.Column) + (1|acc.by.pop) + (1|acc.by.pop:block.by.acc), data=d.ind.merged.tohoku.2016)
    summary(mod.lmer)
    anova(mod.lmer)
    summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    lmer.all.tohoku.2016 <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    
    # save the test results
    filename <- paste(Sys.Date(), "significance_test.txt", sep ="_")
    sink(filename)
    "lmer.all.tohoku.2014_16"
    lmer.all.tohoku.2014_16
    "lmer.all.tohoku.2014"
    lmer.all.tohoku.2014
    "lmer.all.tohoku.2015"
    lmer.all.tohoku.2015
    "lmer.all.tohoku.2016"
    lmer.all.tohoku.2016
    #summary.pop.glm
    sink()
    }
    
    ## check it out for the subset of the accessions used at Miyazaki
    {
    d.ind.merged.2 <- merge(d.ind.filt, d, by="Accession")
    head(d.ind.merged.2)
    
    ## generate a new pop column
    pop=NULL
    for (i in 1:nrow(d.ind.merged.2)) {
      if(d.ind.merged.2$pop3[i]>.95) {pop[i] <- "pop3"} else if (d.ind.merged.2$pop3[i]<.05) {pop[i] <- "non_pop3"}
    }
    
    d.ind.merged.2$pop <- pop
    head(d.ind.merged.2)
    
    colnames(d.ind.merged.2)
    head(d.ind.merged.2)
    
    
    d.ind.merged.all <- d.ind.merged.2[,c(1:11,56)]
    head(d.ind.merged.all)
    d.ind.merged.all$grid_year <- paste(d.ind.merged.all$Field.Column, d.ind.merged.all$Field.row, d.ind.merged.all$Planting.year, sep="_")
    d.ind.merged.all$grid_year <- factor(d.ind.merged.all$grid_year)
    
    d.ind.merged.sub <- subset(d.ind.merged.all, Accession %in% c("MG030", "MG007", "MG008", "MG066", "MG020"))
    d.ind.merged.sub$pop <- factor(d.ind.merged.sub$pop)
    
    # save the subset
    filename <- paste(Sys.Date(), "tradeoff_table.txt")
    write.table(d.ind.merged.sub, file=filename, quote = FALSE, row.names = FALSE, sep = "\t")
    
    d.ind.sub.blocks <- d.ind.merged.sub
    str(d.ind.sub.blocks)
    
    
    
    # add a column with anonymized accession nested by population
    
    # first order by population, accession, and field position
    d.ind.sub.blocks.sorted <- d.ind.sub.blocks[order(d.ind.sub.blocks$pop, d.ind.sub.blocks$Accession, d.ind.sub.blocks$Field.Column, d.ind.sub.blocks$Field.row),]
    head(d.ind.sub.blocks.sorted)
    tail(d.ind.sub.blocks.sorted)
    
    
    # loop over populations
    accession.number <- NULL
    pop.list <- levels(factor(d.ind.sub.blocks.sorted$pop))
    for (p in pop.list) {
      
      # loop over accessions
      d.pop <- subset(d.ind.sub.blocks, pop==p)
      accession.list <- levels(factor(d.pop$Accession))
      n=1
      
      for (i in accession.list) {
        subset(d.pop, Accession == i)
        accession.number[[p]] <- c(accession.number[[p]], list(rep(n, nrow(subset(d.pop, Accession == i)))))
        #accession.number[[n]] <- rep(n, nrow(subset(d.pop1, Accession == i)))
        n=n+1
      }
      
    }
    
    #checks
    accession.number
    length(unlist(accession.number))
    nrow(d.ind.sub.blocks.sorted)
    
    # add the accession by population column
    d.ind.sub.blocks.sorted$acc.by.pop <- unlist(accession.number)
    head(d.ind.sub.blocks.sorted, 20)
    
    
    ## add a block by accession column
    #first sort by accession and grid    
    d.ind.sub.blocks.sorted <- d.ind.sub.blocks.sorted[order(d.ind.sub.blocks.sorted$Accession, d.ind.sub.blocks.sorted$grid_year),]
    head(d.ind.sub.blocks.sorted)    
    
    
    # loop over accessions
    accession.list <- levels(factor(d.ind.sub.blocks.sorted$Accession))
    
    block.number <- NULL
    
    for (a in accession.list) {
      
      # loop over accessions
      d.acc <- subset(d.ind.sub.blocks.sorted, Accession==a)
      block.list <- levels(factor(d.acc$grid_year))
      n=1
      
      for (i in block.list) {
        subset(d.acc, grid_year == i)
        block.number[[a]] <- c(block.number[[a]], list(rep(n, nrow(subset(d.acc, grid_year == i)))))
        #accession.number[[n]] <- rep(n, nrow(subset(d.pop1, Accession == i)))
        n=n+1
      }
      
    }
    
    
    #checks
    head(block.number)
    head(unlist(block.number))
    length(unlist(block.number))
    nrow(d.ind.sub.blocks.sorted)
    
    # add the block by accession column
    d.ind.sub.blocks.sorted$block.by.acc <- unlist(block.number)
    head(d.ind.merged.sorted, 20)
    
    #factorize
    str(d.ind.sub.blocks.sorted)
    d.ind.sub.blocks.sorted$acc.by.pop <- factor(d.ind.sub.blocks.sorted$acc.by.pop)
    d.ind.sub.blocks.sorted$block.by.acc <- factor(d.ind.sub.blocks.sorted$block.by.acc)
    str(d.ind.sub.blocks.sorted)
    
    
    # rename data frame
    d.ind.sub.blocks <- d.ind.sub.blocks.sorted
    
    
    
    # Test for location x population interactions
    mod.lmer <- lmer(surv ~ pop + Location + Location:pop  + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks)
    summary(mod.lmer)
    anova(mod.lmer)
    filename <- paste(Sys.Date(), "location_pop_interaction_test.txt", sep="_")
    sink(filename)
    anova(mod.lmer)
    sink()
    
    
    ## all years
    {
    ### test for pop difference in Tohoku
    d.ind.sub.blocks.tohoku <- subset(d.ind.sub.blocks, Location =="Tohoku")
    table(d.ind.sub.blocks.tohoku$surv, d.ind.sub.blocks.tohoku$pop)
    mod.lmer <- lmer(surv ~ pop + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.tohoku)
    #mod.lmer <- lmer(surv ~ pop + (1|Accession) + (1|grid_year) , data=d.ind.sub.blocks.tohoku)
    summary(mod.lmer)
    anova(mod.lmer)
    summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    tohoku.pop.test <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    
    ### test for pop difference in Miyazaki
    d.ind.sub.blocks.miyazaki <- subset(d.ind.sub.blocks, Location =="Miyazaki")
    table(d.ind.sub.blocks.miyazaki$surv, d.ind.sub.blocks.miyazaki$pop)
    mod.lmer <- lmer(surv ~ pop + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc)  , data=d.ind.sub.blocks.miyazaki)
    summary(mod.lmer)
    anova(mod.lmer)
    summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    miyazaki.pop.test <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
    
    ### Test for Miyazaki-Tohoku difference for pop3
    d.ind.sub.blocks.pop3 <- subset(d.ind.sub.blocks, pop =="pop3")
    lmer.mod <- lmer(surv ~ Location + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc), data=d.ind.sub.blocks.pop3)
    summary(lmer.mod)
    anova(lmer.mod)
    summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
    pop3.location.test <- summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
    
    ### Test for Miyazaki-Tohoku difference for non-pop3
    d.ind.sub.blocks.non_pop3 <- subset(d.ind.sub.blocks, pop =="non_pop3")
    lmer.mod <- lmer(surv ~ Location  + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.non_pop3)
    summary(lmer.mod)
    anova(lmer.mod)
    summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
    non_pop3.location.test <- summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
    
    str(d.ind.sub.blocks.non_pop3)
    
    # save the test results
    filename <- paste(Sys.Date(), "significance_tests_miyazaki_tohoku.txt", sep ="_")
    sink(filename)
    "tohoku.pop.test"
    tohoku.pop.test
    "miyazaki.pop.test"
    miyazaki.pop.test
    "pop3.location.test"
    pop3.location.test
    "non_pop3.location.test"
    non_pop3.location.test
    sink()
    }
    
    
    ## 2014
    {
      ### test for pop difference in Tohoku
      d.ind.sub.blocks.2014 <- subset(d.ind.sub.blocks, Planting.year=="2014")
      d.ind.sub.blocks.2014.tohoku <- subset(d.ind.sub.blocks.2014, Location =="Tohoku")
      table(d.ind.sub.blocks.2014.tohoku$surv, d.ind.sub.blocks.2014.tohoku$pop)
      mod.lmer <- lmer(surv ~ pop  +  (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.2014.tohoku)
      summary(mod.lmer)
      anova(mod.lmer)
      summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      tohoku.pop.test.2014 <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      
      ### test for pop difference in Miyazaki
      d.ind.sub.blocks.2014 <- subset(d.ind.sub.blocks, Planting.year=="2017")
      d.ind.sub.blocks.2014.miyazaki <- subset(d.ind.sub.blocks.2014, Location =="Miyazaki")
      mod.lmer <- lmer(surv ~ pop + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.2014.miyazaki)
      summary(mod.lmer)
      anova(mod.lmer)
      summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      miyazaki.pop.test.2014 <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      
      ### Test for Miyazaki-Tohoku difference for pop3
      d.ind.sub.blocks.2014 <- subset(d.ind.sub.blocks, ((Location=="Tohoku" & Planting.year=="2014") | (Location =="Miyazaki")) ) 
      d.ind.sub.blocks.2014.pop3 <- subset(d.ind.sub.blocks.2014, pop =="pop3")
      lmer.mod <- lmer(surv ~ Location + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc), data=d.ind.sub.blocks.2014.pop3)
      summary(lmer.mod)
      anova(lmer.mod)
      summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      pop3.location.test.2014 <- summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      
      ### Test for Miyazaki-Tohoku difference for non-pop3
      d.ind.sub.blocks.2014 <- subset(d.ind.sub.blocks, ((Location=="Tohoku" & Planting.year=="2014") | (Location =="Miyazaki")) ) 
      d.ind.sub.blocks.2014.non_pop3 <- subset(d.ind.sub.blocks.2014, pop =="non_pop3")
      lmer.mod <- lmer(surv ~ Location  + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.2014.non_pop3)
      summary(lmer.mod)
      anova(lmer.mod)
      summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      non_pop3.location.test.2014 <- summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      
      str(d.ind.sub.blocks.2014.non_pop3)
      
      # save the test results
      filename <- paste(Sys.Date(), "significance_tests_miyazaki_tohoku_2014.txt", sep ="_")
      sink(filename)
      "tohoku.pop.test.2014"
      tohoku.pop.test.2014
      "miyazaki.pop.test.2014"
      miyazaki.pop.test.2014
      "pop3.location.test.2014"
      pop3.location.test.2014
      "non_pop3.location.test.2014"
      non_pop3.location.test.2014
      sink()
    }
    
    ## 2015
    {
      ### test for pop difference in Tohoku
      d.ind.sub.blocks.2015 <- subset(d.ind.sub.blocks, Planting.year=="2015")
      d.ind.sub.blocks.2015.tohoku <- subset(d.ind.sub.blocks.2015, Location =="Tohoku")
      table(d.ind.sub.blocks.2015.tohoku$surv, d.ind.sub.blocks.2015.tohoku$pop)
      mod.lmer <- lmer(surv ~ pop  +  (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.2015.tohoku)
      summary(mod.lmer)
      anova(mod.lmer)
      summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      tohoku.pop.test.2015 <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      
      ### test for pop difference in Miyazaki
      d.ind.sub.blocks.2015 <- subset(d.ind.sub.blocks, Planting.year=="2017")
      d.ind.sub.blocks.2015.miyazaki <- subset(d.ind.sub.blocks.2015, Location =="Miyazaki")
      mod.lmer <- lmer(surv ~ pop + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.2015.miyazaki)
      summary(mod.lmer)
      anova(mod.lmer)
      summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      miyazaki.pop.test.2015 <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      
      ### Test for Miyazaki-Tohoku difference for pop3
      d.ind.sub.blocks.2015 <- subset(d.ind.sub.blocks, ((Location=="Tohoku" & Planting.year=="2015") | (Location =="Miyazaki")) ) 
      d.ind.sub.blocks.2015.pop3 <- subset(d.ind.sub.blocks.2015, pop =="pop3")
      lmer.mod <- lmer(surv ~ Location + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc), data=d.ind.sub.blocks.2015.pop3)
      summary(lmer.mod)
      anova(lmer.mod)
      summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      pop3.location.test.2015 <- summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      
      ### Test for Miyazaki-Tohoku difference for non-pop3
      d.ind.sub.blocks.2015 <- subset(d.ind.sub.blocks, ((Location=="Tohoku" & Planting.year=="2015") | (Location =="Miyazaki")) ) 
      d.ind.sub.blocks.2015.non_pop3 <- subset(d.ind.sub.blocks.2015, pop =="non_pop3")
      lmer.mod <- lmer(surv ~ Location  + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.2015.non_pop3)
      summary(lmer.mod)
      anova(lmer.mod)
      summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      non_pop3.location.test.2015 <- summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      
      str(d.ind.sub.blocks.2015.non_pop3)
      
      # save the test results
      filename <- paste(Sys.Date(), "significance_tests_miyazaki_tohoku_2015.txt", sep ="_")
      sink(filename)
      "tohoku.pop.test.2015"
      tohoku.pop.test.2015
      "miyazaki.pop.test.2015"
      miyazaki.pop.test.2015
      "pop3.location.test.2015"
      pop3.location.test.2015
      "non_pop3.location.test.2015"
      non_pop3.location.test.2015
      sink()
    }
    
    ## 2016
    {
      ### test for pop difference in Tohoku
      d.ind.sub.blocks.2016 <- subset(d.ind.sub.blocks, Planting.year=="2016")
      d.ind.sub.blocks.2016.tohoku <- subset(d.ind.sub.blocks.2016, Location =="Tohoku")
      table(d.ind.sub.blocks.2016.tohoku$surv, d.ind.sub.blocks.2016.tohoku$pop)
      mod.lmer <- lmer(surv ~ pop  +  (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.2016.tohoku)
      summary(mod.lmer)
      anova(mod.lmer)
      summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      tohoku.pop.test.2016 <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      
      ### test for pop difference in Miyazaki
      d.ind.sub.blocks.2016 <- subset(d.ind.sub.blocks, Planting.year=="2017")
      d.ind.sub.blocks.2016.miyazaki <- subset(d.ind.sub.blocks.2016, Location =="Miyazaki")
      mod.lmer <- lmer(surv ~ pop + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.2016.miyazaki)
      summary(mod.lmer)
      anova(mod.lmer)
      summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      miyazaki.pop.test.2016 <- summary(glht(mod.lmer, linfct= mcp(pop="Tukey")))
      
      ### Test for Miyazaki-Tohoku difference for pop3
      d.ind.sub.blocks.2016 <- subset(d.ind.sub.blocks, ((Location=="Tohoku" & Planting.year=="2016") | (Location =="Miyazaki")) ) 
      d.ind.sub.blocks.2016.pop3 <- subset(d.ind.sub.blocks.2016, pop =="pop3")
      lmer.mod <- lmer(surv ~ Location + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc), data=d.ind.sub.blocks.2016.pop3)
      summary(lmer.mod)
      anova(lmer.mod)
      summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      pop3.location.test.2016 <- summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      
      ### Test for Miyazaki-Tohoku difference for non-pop3
      d.ind.sub.blocks.2016 <- subset(d.ind.sub.blocks, ((Location=="Tohoku" & Planting.year=="2016") | (Location =="Miyazaki")) ) 
      d.ind.sub.blocks.2016.non_pop3 <- subset(d.ind.sub.blocks.2016, pop =="non_pop3")
      lmer.mod <- lmer(surv ~ Location  + (1|pop:acc.by.pop) + (1|pop:acc.by.pop:block.by.acc) , data=d.ind.sub.blocks.2016.non_pop3)
      summary(lmer.mod)
      anova(lmer.mod)
      summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      non_pop3.location.test.2016 <- summary(glht(lmer.mod, linfct= mcp(Location="Tukey")))
      
      str(d.ind.sub.blocks.2016.non_pop3)
      
      # save the test results
      filename <- paste(Sys.Date(), "significance_tests_miyazaki_tohoku_2016.txt", sep ="_")
      sink(filename)
      "tohoku.pop.test.2016"
      tohoku.pop.test.2016
      "miyazaki.pop.test.2016"
      miyazaki.pop.test.2016
      "pop3.location.test.2016"
      pop3.location.test.2016
      "non_pop3.location.test.2016"
      non_pop3.location.test.2016
      sink()
    }
    
    
    
    }
    
    
  
  }
  
  
  
  ### Make plots for the Miyazaki phenotype data
  {
    
    library(ggplot2)
    library(maps)
    library(mapdata)
    
    ### simple theme with no gridlines
    theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=font.size), legend.text=element_text(size=font.size), axis.title=element_text(size=font.size,face="bold"), legend.title=element_text(size=font.size, face="bold"), legend.background = element_rect(fill="transparent"), legend.position = c(0.2,0.8) )
    
    
    # set graphics parameters
    alpha=0.6
    font.size = 20
    point.size = 5
    
    ## set colors
    col1="#8c510a"
    col2="#bf812d"
    col3="#dfc27d"
    col4="#f6e8c3"
    col5="#35978f"
    col6="#80cdc1"
    col7="#4393c3"
    
    
    ## generate that map of Japan
    japan <- map_data("japan")
    dim(japan)
    head(japan)
    
    gg1 <- ggplot() +
      geom_polygon(data = japan, aes(x=long, y = lat, group = group), fill = "grey", color = "grey") + 
      coord_fixed(1.3)
    gg1
    
    
    ### plot the different groups on the map
    d.sub1 <- subset(d, Accession %in% c("Gifu","MG005","MG007","MG008","MG020","MG030","MG066","MG076"))
    colnames(d.sub1)
    d.sub <- d.sub1[,c(1,10:11)]
    Accession <- c("Tohoku","Miyazaki")
    Latitude <- c(38.46,31.831)
    Longitude <- c(141.09,131.412)
    sites <- data.frame(Accession, Latitude, Longitude)
    str(sites)
    str(d.sub)
    d.sub <- rbind(d.sub,sites)
    gg.pop1 <- gg1 + geom_point(data=d.sub, aes(x=Longitude,y=Latitude),na.rm=TRUE,size=point.size,alpha=alpha, shape=16, col=col1) + geom_text(data=d.sub,aes(x=Longitude,y=Latitude,label=Accession)) +
      labs(x="Longitude",y="Latitude",color="Pop1") + scale_color_gradient(low="white", high="#FF3300", limits=c(0,1)) + theme
    gg.pop1
    filename <- paste(Sys.Date(),"Miyazaki_locations.pdf", sep="")
    setwd("/Users/au27857/Dropbox/NirajShah/AccessionPaper/31082016/Manuscript/Figures/_20180614/miyazaki_phenotypes")
    pdf(filename, 5, 5)
    gg.pop1
    dev.off()
    
  }
  

}


## read in GWAS and Fst data
{
## Fst results
fst.dir <-""
setwd(fst.dir)
#fst.file <- "20180622_notPop3vsPop3_fst_scan.txt.weir.fst"
#fst.file <- "20180622_Pop2vsPop3_fst_scan.txt.weir.fst"
#fst.file <- "20180622_Pop1vsPop3_fst_scan.txt.weir.fst"
fst.file <- "20180622_Pop1vsPop2_fst_scan.txt.weir.fst"

d.fst <- read.delim(fst.file)
colnames(d.fst) <- c("chr","pos","fst")
d.fst$key <- paste(d.fst$chr,d.fst$pos,sep="_")
head(d.fst)
nrow(d.fst)


## GWAS results
gwas.dir <- ""
setwd(gwas.dir)
## filenames have to be adjusted to pid01 etc, to get the right order
dataFiles.in <- lapply(Sys.glob("*.pvals"), read.csv)
fileList <- Sys.glob("*.pvals")
fileList
}

## produce GWAS Manhattan plots
{
  #install.packages("qqman")
    library("qqman")
  

# plot for all traits
bon_threshold=0.05/nrow(dataFiles.in[[1]])
mac.cutoff=8
filename <- paste(Sys.Date(),"_","manhat.png", sep="")
png(filename, 8, length(fileList)*3, units="in", res=200)
par(mfrow=c(length(fileList),1),mar=c(5,4,4,2))
for (i in 1:length(fileList)) {
  main <- fileList[i]
  gwas.plot <- dataFiles.in[[i]][,c(1,2,3,5)]
  gwas.plot <- subset(gwas.plot, macs > mac.cutoff & chromosomes != 0)
  colnames(gwas.plot) <- c("CHR","BP","P", "MAC")
  head(gwas.plot)
  yaxis=max(-log10(gwas.plot$P))+1
  manhattan(gwas.plot, main=main, cex=2,suggestiveline = -log10(bon_threshold),genomewideline = F,ylim=c(0,yaxis))
}
dev.off()

#plot for selected traits
filename <- paste(Sys.Date(),"_","manhat_select.png", sep="")
col.indices <- c(4,5,6,10,11,9,16,23)
png(filename, 8, length(col.indices)*3, units="in", res=200)
par(mfrow=c(length(col.indices),1), mar=c(5,4,4,2))
for (i in col.indices) {
  main <- fileList[i]
  gwas.plot <- dataFiles.in[[i]][,c(1,2,3,5)]
  gwas.plot <- subset(gwas.plot, macs > mac.cutoff & chromosomes != 0)
  colnames(gwas.plot) <- c("CHR","BP","P", "MAC")
  head(gwas.plot)
  yaxis=max(-log10(gwas.plot$P))+1
  manhattan(gwas.plot, main=main, cex=2,suggestiveline = -log10(bon_threshold),genomewideline = F,ylim=c(0,yaxis))
}
dev.off()
}


## Analyse overlaps in GWAS and Fst results at the SNP level
{

## loop through all GWAS p-value files and get the Fst overlapping SNPs
setwd(gwas.dir)
fst.gwas.overlap <- list()
fst.gwas.overlap.filt <- list()
dataFiles <- list()
top.snps=200
mac.cutoff=10

for (i in 1:length(fileList)) {
  dataFiles.in[[i]]$p <- -log(dataFiles.in[[i]]$scores,10)
  dataFiles[[i]] <- dataFiles.in[[i]][with(dataFiles.in,order(-dataFiles.in[[i]]$p)),]
  dataFiles[[i]] <- subset(dataFiles[[i]], macs> mac.cutoff)
  dataFiles[[i]] <- dataFiles[[i]][1:top.snps,]
  dataFiles[[i]]$chr <- paste("chr", dataFiles[[i]]$chromosomes, sep="")
  dataFiles[[i]]$key <- paste(dataFiles[[i]]$chr, dataFiles[[i]]$positions, sep="_")
  head(dataFiles[[i]])
  bedgraph <- as.data.frame(cbind(dataFiles[[i]]$chr,dataFiles[[i]]$positions,dataFiles[[i]]$positions,dataFiles[[i]]$p))
  #filename <- paste(Sys.Date(), fileList[i], ".bedgraph", sep="_")
  filename <- paste(Sys.Date(), fileList[i], "txt", sep="_")
  #write.table(bedgraph, filename, col.names=FALSE, row.names=FALSE, quote=FALSE, sep="\t")
  fst.gwas.overlap[[i]] <- merge(dataFiles[[i]], d.fst, by="key", all.x=TRUE)
  head(fst.gwas.overlap[[i]])
  write.table(fst.gwas.overlap[[i]],filename,col.names=T,row.names=F,quote=FALSE, sep="\t")
  
}

# Summarise GWAS and Fst overlaps
overlap.counts <- lapply(fst.gwas.overlap, nrow)
overlap.summary <- NULL
for (i in 1:length(fileList)) {
  overlap.summary[i] <- overlap.counts[[i]]
}
overlap.summary <- as.data.frame(cbind(fileList,overlap.summary))
filename <- paste(Sys.Date(), "gwas_fst_overlap", fst.file, top.snps, "M", mac.cutoff, "P", ".txt", sep="_")
write.table(overlap.summary, filename, col.names=FALSE, row.names=FALSE, quote=FALSE, sep="\t")
overlap.summary

# summarise overlaps between different GWAS runs
gwas.overlap <- data.frame(matrix(ncol = length(fileList), nrow = length(fileList)))
for (n in 1:length(fileList)) {
  for (m in 1:length(fileList)) {
    gwas.overlap[n,m] <- nrow(merge(fst.gwas.overlap[[n]], fst.gwas.overlap[[m]], by="key"))
  }
}
gwas.overlap$file <- fileList
filename <- paste(Sys.Date(), "gwas_overlap", fst.file, top.snps, "M", mac.cutoff, "P",".txt", sep="_")
write.table(gwas.overlap, filename, col.names=FALSE, row.names=FALSE, quote=FALSE, sep="\t")
gwas.overlap

# Do boxplot of Fst values for the top 100 GWAS SNPs for each phenotype
fst.boxplot <- data.frame(matrix(ncol = length(fileList), nrow = top.snps))
p.boxplot <- data.frame(matrix(ncol = length(fileList), nrow = top.snps))
for (i in 1:length(fileList)) {
  fst.boxplot[,i] <- fst.gwas.overlap[[i]]$fst
  p.boxplot[,i] <- dataFiles[[i]]$p
}
colnames(fst.boxplot) <- fileList
boxplot(fst.boxplot)
colnames(p.boxplot) <- fileList
boxplot(p.boxplot)
fst.means <- as.data.frame(colMeans(fst.boxplot, na.rm = TRUE))
p.means <- as.data.frame(colMeans(p.boxplot, na.rm = TRUE))
p.medians <- as.data.frame(apply(p.boxplot, 2, median,na.rm = TRUE))
filename <- paste(Sys.Date(), "fst.means", fst.file,top.snps, "M", mac.cutoff, "P", ".txt", sep="_")
write.table(fst.means, file=filename, fst.file, col.names=FALSE, row.names=TRUE, quote=FALSE, sep="\t")


## check distribution of all Fst values
head(d.fst)
hist(d.fst$fst, breaks=50)

#compare the Fst distributions of all genes to those of the top GWAS SNPs
  {
    
    ## histogram of fst distribution for all SNPs
     filename <- paste(Sys.Date(), "all_SNPS_fst.distribution.png", sep ="_")
    cex=2
    png(filename, 3, 3, units="in", res=150)
    par(mfcol=c(1,1), mar=c(3,3,3,3))
    hist(d.fst$fst, xlim=c(0,1), main="", breaks=25, xlab="", ylab="", cex.axis=cex, cex.lab=cex)
    dev.off()
    
}
}

## permutation analysis
{
  
  #### import data
  {
    
    # import permuted data
    perm.dir <- ""
    setwd(perm.dir)
    d.perm <- read.csv("perm_OW_2014_15_perm.csv") #On trait at a time
    
    # add merge key to permuted data
    d.perm$chr <- paste("chr", d.perm$Chromosome, sep="")
    d.perm$key <- paste(d.perm$chr, d.perm$Position, sep="_")
    head(d.perm)
    
    # merge permuted p-value data with the fst data 
    fst.gwas.overlap.perm <- merge(d.perm, d.fst, by="key", all.x=TRUE)
    head(fst.gwas.overlap.perm)
    
  }
  
  ### look at fst distributions of permuted data
  {
    
    # set alpha (transparency level)
    alpha = 0.002  #0.02 was used for plotting results of 100 permutations
    
    # set the trait name
    trait.name <- "OW2014"  #Change if given permutation file belongs to another trait
    
    
    # prepare lists to be populated
    top.permuted.fst <- list()
    top.permuted.p <- list()
    top.permuted.cor <-list()
    
    ks.test.summary.fst <- data.frame(matrix(ncol = 3, nrow = 0))
    x <- c("Two-sided", "Greater", "Less")
    colnames(ks.test.summary.fst) <- x
    
    ks.test.summary.p <- data.frame(matrix(ncol = 3, nrow = 0))
    x <- c("Two-sided", "Greater", "Less")
    colnames(ks.test.summary.p) <- x
    
    ## save the top observed SNPs
    top.observed <- fst.gwas.overlap.perm[with(fst.gwas.overlap.perm,order(fst.gwas.overlap.perm$Obs_pval)),]
    top.observed <- head(top.observed, top.snps)
    top.observed <- top.observed[,c(1:4,ncol(top.observed))]
    filename <- paste(Sys.Date(),trait.name, top.snps , "observed.RData", sep="_")
    saveRDS(top.observed, filename)
    
    
    # loop across all permutations  
    for (i in 5:1004) {
      
      # subset top permuted SNPs
      top.permuted <- fst.gwas.overlap.perm[with(fst.gwas.overlap.perm,order(fst.gwas.overlap.perm[,i])),]
      #top.permuted <- top.permuted[start:end,]
      top.permuted <- head(top.permuted, top.snps)
      
      # add p-values and fst values for top snps to lists
      top.permuted.fst[[(i-4)]] <- top.permuted$fst
      top.permuted.p[[(i-4)]] <- top.permuted[,i]
      
      # Spearman correlation
      top.permuted.cor[[(i-4)]] <- cor(top.permuted$fst, -log(top.permuted[,i],10) , use="complete.obs" , method="spearman")
      
      ##Kolmogorov-Smirnov tests - fst values
      ks.two.sided <- ks.test(top.permuted$fst, top.observed$fst, alternative="two.sided")
      ks.greater <- ks.test(top.permuted$fst, top.observed$fst, alternative="greater")
      ks.less <- ks.test(top.permuted$fst, top.observed$fst, alternative="less")
      ks.test.summary.fst[i-4,] <- c(ks.two.sided$p.value, ks.greater$p.value, ks.less$p.value)
      
      ##Kolmogorov-Smirnov tests - fst values
      ks.two.sided <- ks.test(top.permuted[,i], top.observed$Obs_pval, alternative="two.sided")
      ks.greater <- ks.test(top.permuted[,i], top.observed$Obs_pval, alternative="greater")
      ks.less <- ks.test(top.permuted[,i], top.observed$Obs_pval, alternative="less")
      ks.test.summary.p[i-4,] <- c(ks.two.sided$p.value, ks.greater$p.value, ks.less$p.value)
      
      
      
    }
  }
    
  ## save p-values and fst values from top permuted SNPs
  { 
  #setwd(perm.dir)
    # fst values
    filename <- paste(Sys.Date(), trait.name, top.snps , "fst.RData", sep="_")
    saveRDS(top.permuted.fst, filename)
    # p-values
    filename <- paste(Sys.Date(),trait.name, top.snps , "p.RData", sep="_")
    saveRDS(top.permuted.p, filename)
    
    ## save results of Kolmogorov-Sminov tests - fst values
    filename <- paste(Sys.Date(),trait.name, top.snps , "KStest_fst.text", sep="_")
    write.table(ks.test.summary.fst, filename, quote=FALSE, row.names = FALSE, sep="\t")
    
    ## save results of Kolmogorov-Sminov tests - p-values
    filename <- paste(Sys.Date(),trait.name, top.snps , "KStest_p.text", sep="_")
    write.table(ks.test.summary.p, filename, quote=FALSE, row.names = FALSE, sep="\t")
    
    # check ks.test.results - fst values
    ks.test.direction.fst.true <- subset(ks.test.summary.fst, Greater/Less < 1e-3)
    true.fst <- nrow(ks.test.direction.fst.true)
    ks.test.direction.fst.false <- subset(ks.test.summary.fst, Less/Greater < 1e-3)
    false.fst <- nrow(ks.test.direction.fst.false)
    fdr.fst <- as.data.frame(c(true.fst, false.fst, false.fst/(true.fst+false.fst)*100))
    colnames(fdr.fst) <- "Fst"
    fdr.fst$Labels <- c("true","false","fdr")
    filename <- paste(Sys.Date(),trait.name, top.snps , "KStest_fst_fdr.text", sep="_")
    write.table(fdr.fst, filename, quote=FALSE, row.names = FALSE, sep="\t")
    
    # check ks.test.results - p values
    ks.test.direction.p.true <- subset(ks.test.summary.p, Less/Greater < 1e-3)
    true.p <- nrow(ks.test.direction.p.true)
    ks.test.direction.p.false <- subset(ks.test.summary.p, Greater/Less < 1e-3)
    false.p <- nrow(ks.test.direction.p.false)
    fdr.p <- as.data.frame(c(true.p, false.p, false.p/(true.p+false.p)*100))
    colnames(fdr.p) <- "P"
    fdr.p$Labels <- c("true","false","fdr")
    filename <- paste(Sys.Date(),trait.name, top.snps , "KStest_p_fdr.text", sep="_")
    write.table(fdr.p, filename, quote=FALSE, row.names = FALSE, sep="\t")
    
    ## Spearman correlation fdr
    top.permuted.all.cor <- unlist(top.permuted.cor)
    cor.observed <- cor(top.observed$fst, -log(top.observed$Obs_pval,10), use="complete.obs", method="spearman")
    cor.fdr <- table(top.permuted.all.cor < cor.observed)
    filename <- paste(Sys.Date(),trait.name, top.snps , "cor_fdr.text", sep="_")
    write.table(cor.fdr, filename, quote=FALSE, row.names = FALSE, sep="\t")
    
    ## save results of Spearman correlations
    filename <- paste(Sys.Date(),trait.name, top.snps , "cor.text", sep="_")
    write.table(top.permuted.all.cor, filename, quote=FALSE, row.names = FALSE, sep="\t")
    
    # save histogram of Spearman correlations
    filename <- paste(Sys.Date(),trait.name, top.snps , "cor.hist.pdf", sep="_")
    pdf(filename)
    hist(top.permuted.all.cor, breaks=50, main="Spearman", xlim=c(-1,1))
    abline(v=cor.observed, lwd=2, col="red")
    dev.off()
    
    ## unlist the fst and p-values to get all values in one vector
    top.permuted.all.fst <- unlist(top.permuted.fst)
    top.permuted.all.p <- unlist(top.permuted.p)
    
  }
  
  ### produce the permutation plot - fst values
  {
      #setwd(perm.dir)
      filename <- paste(Sys.Date(), trait.name, "M", fst.file, top.snps , "fst.permutation.png", sep ="_")
      png(filename, 9, 9, units="in", res=150, type="cairo") # cairo required for running on cluster
      #png(filename, 9, 9, units="in", res=150)
      
      # prepare plot
      
      par(mfcol=c(1,1))
      cex=2.7
      
      #fst, all genes
      ecdf1 <- ecdf(fst.gwas.overlap.perm$fst)
      plot(ecdf1, verticals=TRUE, do.points=FALSE, col="blue", xlim=c(0,1), main=trait.name, xlab="", ylab="", lwd=3, cex.axis=cex, cex.lab=cex )
      #top observed SNPs
      ecdf2 <- ecdf(top.observed$fst)     #fst, top gwas SNPs
      plot(ecdf2, verticals=TRUE, do.points=FALSE, add=TRUE, col='orange', xlim=c(0,1), lwd=3, cex.axis=cex, cex.lab=cex)
      
      ## permutation results
      ## individual permutations
      for (i in 1:length(top.permuted.fst)) {
        ecdf3 <- ecdf(top.permuted.fst[[i]])
        plot(ecdf3, verticals=TRUE, do.points=FALSE, add=TRUE, col = rgb(red = 0, green = 0, blue = 0, alpha = alpha), xlim=c(0,1), lwd=3, cex.axis=cex, cex.lab=cex)
      }
      # all permutation data
      ecdf4 <- ecdf(top.permuted.all.fst)
      plot(ecdf4, verticals=TRUE, do.points=FALSE, add=TRUE, col = "dark grey", xlim=c(0,1), lwd=3, cex.axis=cex, cex.lab=cex)
      
      dev.off()
    }
    
  ### produce the permutation plot - p values
  {
      #setwd(perm.dir)
      filename <- paste(Sys.Date(), trait.name, "M", fst.file, top.snps , "p.permutation.png", sep ="_")
      png(filename, 9, 9, units="in", res=150, type="cairo") # cairo required for running on cluster
      #png(filename, 9, 9, units="in", res=150)
      
      # prepare plot
      
      par(mfcol=c(1,1))
      cex=2.7
      
      #p, all genes
      ecdf1 <- ecdf(-log(fst.gwas.overlap.perm$Obs_pval,10))
      plot(ecdf1, verticals=TRUE, do.points=FALSE, col="blue", xlim=c(0,16), main=trait.name, xlab="", ylab="", lwd=3, cex.axis=cex, cex.lab=cex )
      
      
      #top observed SNPs
      ecdf2 <- ecdf(-log(top.observed$Obs_pval,10))     #fst, top gwas SNPs
      plot(ecdf2, verticals=TRUE, do.points=FALSE, add=TRUE, col='orange', xlim=c(0,16), lwd=3, cex.axis=cex, cex.lab=cex)
      
      ## permutation results
      # individual permutations
      for (i in 1:length(top.permuted.fst)) {
        ecdf3 <- ecdf(-log(top.permuted.p[[i]],10))
        plot(ecdf3, verticals=TRUE, do.points=FALSE, add=TRUE, col = rgb(red = 0, green = 0, blue = 0, alpha = alpha), xlim=c(0,16), lwd=3, cex.axis=cex, cex.lab=cex)
      }
      # all permutation data combined
      ecdf4 <- ecdf(-log(top.permuted.all.p,10))
      plot(ecdf4, verticals=TRUE, do.points=FALSE, add=TRUE, col = "dark grey", xlim=c(0,16), lwd=3, cex.axis=cex, cex.lab=cex)
      
      dev.off()
    }
  
  #Produce histograms, ecdfsand (including permutation shading) gwas/fst scatter plots used for figure 5.
  {
    
    ## loop through all traits and plot histograms, ecdfs, and gwas/fst scatter plots
    setwd("")
    fstpermfileList <- Sys.glob("*fst.RData")
    filename <- fstpermfileList
    #reorder so in the same order as fileList file
    ord=c("2019-04-03_Altitude_200_fst.RData","2019-04-03_MeanTemp_200_fst.RData","2019-04-03_MinTemp_200_fst.RData","2019-04-03_OW2014_200_fst.RData","2019-04-03_OW2015_200_fst.RData","2019-04-03_OW2016_200_fst.RData","2019-04-03_1stFT_200_fst.RData", "2019-04-03_2ndFT_200_fst.RData", "2019-04-03_2ndFTperiod_200_fst.RData","2019-04-03_FTDK_200_fst.RData","2019-04-03_FT2014_200_fst.RData","2019-04-03_2015_2ndyearFT_200_fst.RData","2019-04-03_2017FT_200_fst.RData","2019-04-03_2018FT_200_fst.RData","2019-04-03_Kcontr_200_fst.RData","2019-04-03_Ncontr_200_fst.RData","2019-04-04_Ksalt_200_fst.RData","2019-04-04_Nsalt_200_fst.RData","2019-04-04_Weight_200_fst.RData","2019-04-04_Size_200_fst.RData","2019-04-04_Perimeter_200_fst.RData","2019-04-04_Length_200_fst.RData","2019-04-04_Width_200_fst.RData","2019-04-04_LWR_200_fst.RData","2019-04-04_Circularity_200_fst.RData")
    f1=factor(filename,levels=ord)
    filenameFST=sort(f1)
    filenameFST
    
    for (i in 1: length(fileList)) {
      
      ecdf1 <- ecdf(d.fst$fst)  #fst, all genes
      ecdf2 <- ecdf(fst.gwas.overlap[[i]]$fst)     #fst, top gwas SNPs
  
      #setwd(gwas.dir)
      filename <- paste(Sys.Date(), fileList[i], "M", mac.cutoff, "P", fst.file, "fst.distributions.png", sep ="_")
      png(filename, 9, 3, units="in", res=150)
      par(mfcol=c(1,3), mar=c(5,5.1,5,5))
      cex=2.7
      hist(fst.gwas.overlap[[i]]$fst, xlim=c(0,1), main=fileList[i], breaks=25, xlab="", ylab="", cex.axis=cex, cex.lab=cex)
      
      ## permutation results
      ## individual permutations
      top.permuted.fst <- readRDS(as.character(filenameFST[i])) #loading top 200 SNPs for all 1000 perm for the ith trait
      top.permuted.all.fst <- unlist(top.permuted.fst)
      plot(ecdf1, verticals=TRUE, do.points=FALSE, col="blue", xlim=c(0,1), main=fileList[i], xlab="", ylab="", lwd=3, cex.axis=cex, cex.lab=cex )
      for (j in 1:length(top.permuted.fst)) {
        ecdf3 <- ecdf(top.permuted.fst[[j]])
        plot(ecdf3, verticals=TRUE, do.points=FALSE, add=TRUE, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.0019999), xlim=c(0,1), lwd=2, cex.axis=cex, cex.lab=cex)
      }
      plot(ecdf2, verticals=TRUE, do.points=FALSE, add=TRUE, col='orange', xlim=c(0,1), lwd=3, cex.axis=cex, cex.lab=cex)
      
      # all permutation data
      ecdf4 <- ecdf(top.permuted.all.fst)
      plot(ecdf4, verticals=TRUE, do.points=FALSE, add=TRUE, col = "green", xlim=c(0,1), lwd=3, cex.axis=cex, cex.lab=cex)
      
      plot(fst.gwas.overlap[[i]]$fst, fst.gwas.overlap[[i]]$p, main = fileList[i], lwd=2, xlab="", ylab="", cex.axis=cex, cex.lab=cex, xlim=c(0,1))
      
      dev.off()
    }
    
    
    
    ##Kolmogorov-Smirnov tests for all traits
    {
      ## Kolmogorov-Smirnov - Fst, all SNPs versus GWA SNPs for each trait
      ks.test.summary <- data.frame(matrix(ncol = 3, nrow = 0))
      x <- c("Two-sided", "Greater", "Less")
      colnames(ks.test.summary) <- x

      for (i in 1:length(fileList)) {
      ks.two.sided <- ks.test(d.fst$fst, fst.gwas.overlap[[i]]$fst, alternative="two.sided")
      ks.greater <- ks.test(d.fst$fst, fst.gwas.overlap[[i]]$fst, alternative="greater")
      ks.less <- ks.test(d.fst$fst, fst.gwas.overlap[[i]]$fst, alternative="less")
      ks.test.summary[i,] <- c(ks.two.sided$p.value, ks.greater$p.value, ks.less$p.value)
      }
      rownames(ks.test.summary) <- fileList

      ks.test.summary
      setwd(gwas.dir)
      filename <- paste(Sys.Date(), "ks_summary", fst.file, sep="_")
      write.table(ks.test.summary, file = filename, quote=FALSE, sep="\t", col.names=NA)


    }
    
    
  }

}

## Save supplemental files with GWAS and FST results
{
  # set working directory
  setwd("")
  # select traits and set trait names
  traits.list <- c(1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 15, 8, 9, 10, 16, 17, 19, 20, 39:45)
  trait.names <- c("Altitude","Mean_temperature","Minimum_temperature","Overwintering_2014", "Overwintering_2015", "Overwintering_2016", "FT_greenhouse", "FT_2014", "FT_2015", "FT_2017", "FT_2018", "FT_1st", "FT_2nd", "FT_dur", "K_ctrl", "Na_ctrl", "K_salt", "Na_salt", "Seed_weight", "Seed_size", "Seed_perimeter", "Seed_length", "Seed_width", "Seed_length_width ratio", "Seed_circularity")
  # save supplemental files
  n=1
  for (i in traits.list) {
    fst.gwas.overlap.filt <- NULL
    fst.gwas.overlap.filt <- subset(fst.gwas.overlap[[i]], p>3)
    fst.gwas.overlap.filt.table <- fst.gwas.overlap.filt[,c(9,3,4,8,5:7,12)]
    filename <- paste(Sys.Date(), trait.names[n], fst.file,"gwas_results.txt", sep="_")
    write.table(fst.gwas.overlap.filt.table, file = filename, quote=FALSE, sep="\t", col.names=NA)
    n=n+1
  }
  
}


## Analyse overlaps in GWAS and Fst results at the gene level
{
  
  ## check the original GFF file for gene model length distribution
  setwd("")
  gff.original <- read.delim("20130802_Lj30.sorted.igv.gff3", header = FALSE)
  gff.original.genes <- subset(gff.original,V2=="protein_coding" & V3== "gene")[,c(1:5)]
  gff.original.genes$length <- gff.original.genes$V5 - gff.original.genes$V4
  hist(gff.original.genes$length)
  nrow(gff.original.genes)
  gff <- subset(gff.original.genes, length>200 & length < 20000)
  nrow(gff)
  hist(gff$length)
  
  # average fst and gwas results by gene in R
  setwd("")
  fst.by.gene.filename <- paste(fst.file, ".bedgraph.genes.txt", sep="")
  d <- read.delim(fst.by.gene.filename, header=FALSE)
  head(d)
  fst.gene <- aggregate(abs(d$V13), list(d$V14), mean)
  fst.gene.count <- aggregate(abs(d$V13), list(d$V14), NROW)
  fst.gene$count <- fst.gene.count$x
  head(fst.gene)
  fst.gene.filt <- subset(fst.gene, count >3)
  nrow(fst.gene)
  nrow(fst.gene.filt)
  median(fst.gene.filt$x)
  
  fst.gene.filt.order <- fst.gene.filt[order(-fst.gene.filt$x),]
  subset(fst.gene.filt.order, x> 0.65)
  filename <- paste(Sys.Date(), "_", fst.file, "_fst_by_gene.txt", sep="")
  write.table(fst.gene.filt.order, filename, quote=FALSE, sep="\t")
  #check candidates
  subset(d, V14=="Lj6g2130160")
  subset(d, V14=="Lj6g1887780")
  subset(d, V14=="Lj1g2533770")
  top.100.fst.genes <-subset(fst.gene.filt.order, x> 0.65)
  top.100.fst.genes.positions <- merge(top.100.fst.genes, d, by.x="Group.1", by.y="V14")
  top.100.fst.genes.positions.unique <- unique(top.100.fst.genes.positions[,c(4,7,8)])
  nrow(top.100.fst.genes)
  
  filename <- paste(Sys.Date(), "_", fst.file, "_fst_by_gene_hist.pdf", sep="")
  setwd("")
  pdf(filename)
  hist(fst.gene.filt$x, breaks=50)
  abline(v=median(fst.gene.filt$x), col="red", lwd=4 )
  abline(v=0.65, col="blue", lwd=4 )
  dev.off()
  
  # no correlation between count and fst score
  smoothScatter(fst.gene.filt$x,fst.gene.filt$count, log="y")
  

}


## Produce gwas and Fst charts for the main figure
{
  # Read in libraries  
  library(zoo)
  library(ggplot2)
  #install.packages("viridis")
  library(viridis)
  library(grid)
  library(dplyr)
  
  # set averaging parameters for rolling mean applied to Fst data
  by=1
  width=10
  
  # set number of decimals for y-axis
  scaleFUN <- function(x) sprintf("%.2f", x)
  
  # set plot theme
  font.size=20
  #theme <- theme(panel.grid.major = element_blank(), legend.position="none", panel.grid.minor = element_blank(), axis.text=element_text(size=font.size), axis.title=element_text(size=font.size,face="bold") )
  theme <- theme(axis.text=element_text(size=font.size), axis.title=element_text(size=font.size,face="bold") )
  
  
  ## plot Fst and GWAS results in the same graph
  
  # plots for the figure
  # chr1: OW_2014 (4)
  # chr2: FT_gh (11), FT_2014 (12), FT_dur (10)
  # chr3: OW_2016 (6)
  # chr4: FT_gh (11)
  # chr5: NA+ control (17), Seed width (43)
  # chr6: OW_2014 (4), OW_2015 (5)
  
  plots = list()
  plots.fst = list()
  traits.list <- list(c(4), c(10,11,9), c(6),c(1), c(16,23),c(4,5)) 
  n=1
  gwas.dot.size=3
  gwas.circle.size=5
  
  # set plot directory
  #setwd("")
  
  # generate the plots
    for (chr in c(1:6)) {
    current.chr <- paste("chr",chr, sep="")
    n=1
    
    for (i in traits.list[[chr]]) {
      
      #gwas
      gwas.chr <- subset(dataFiles[[i]], chr == current.chr)
      x.gwas <- gwas.chr$positions
      y.gwas <- gwas.chr$p
      d.gwas.chr <- as.data.frame(cbind(x.gwas,y.gwas))
      q.gwas <- ggplot(d.gwas.chr, aes(x=x.gwas, y=y.gwas))
      q.gwas <- q.gwas + geom_point(color="dark grey") + theme
      
      #fst
      d.fst.chr <- subset(d.fst, chr ==current.chr)
      #fst.genes <- subset(top.100.fst.genes.positions.unique, V1 == current.chr)
      x.fst <- rollapply(d.fst.chr$pos, width = width, by = by, FUN = mean, align = "left")
      y.fst <- rollapply(d.fst.chr$fst, width = width, by = by, FUN = mean, align = "left")
      d.fst.chr.rollmean <- as.data.frame(cbind(x.fst,y.fst))
      q.fst <- ggplot(d.fst.chr.rollmean, aes(x=x.fst, y=y.fst))
      q.fst <- q.fst + geom_point(color="dark grey") + theme
      
      #gwas fst overlap
      gwas.fst.overlap.chr <- NULL
      gwas.fst.overlap.chr <- subset(fst.gwas.overlap[[i]], chr.x == current.chr)
      gwas.fst.overlap.chr <- gwas.fst.overlap.chr[order(gwas.fst.overlap.chr$p),]
      
      # produce Fst plot colored by GWAS p-values
      if (nrow(gwas.fst.overlap.chr)>0) {
        q.fst.colored <- q.fst + geom_point(data=gwas.fst.overlap.chr, aes(x=pos, y=fst, color=p), size=gwas.circle.size, shape=16 ) + ggtitle(paste(current.chr, fileList[i], sep="_")) + labs(x="Position", y="Fst") + scale_color_viridis() + theme } else {
        q.fst.colored <- q.fst + theme}
      plots.fst[[n]] = q.fst.colored
      
      n=n+1
      
    }
    
    #print plots
    filename <- paste(Sys.Date(), fst.file, "chr", chr, ".png", sep="_")
    png(filename, 12,3*length(traits.list[[chr]]), units="in", res=200)
    grid.newpage()
    if (length(traits.list[[chr]]) == 2) {
      grid.draw(rbind(ggplotGrob(plots.fst[[1]]), ggplotGrob(plots.fst[[2]]), size = "last")) } else if  (length(traits.list[[chr]]) == 3) {
        grid.draw(rbind(ggplotGrob(plots.fst[[1]]), ggplotGrob(plots.fst[[2]]), ggplotGrob(plots.fst[[3]]), size = "last"))} else if  (length(traits.list[[chr]]) == 1) {
          grid.draw(rbind(ggplotGrob(plots.fst[[1]]), size = "last")) }
    dev.off() 
  
  }


  
}


## Produce gwas and Fst charts for the supplementary figure
{
  # Read in libraries  
  library(zoo)
  library(ggplot2)
  #install.packages("viridis")
  library(viridis)
  library(grid)
  library(dplyr)
  library(gridExtra)
  
  # set averaging parameters for rolling mean applied to Fst data
  by=1
  width=10
  
  # set plot theme
  font.size=16
  #theme <- theme(panel.grid.major = element_blank(), legend.position="none", panel.grid.minor = element_blank(), axis.text=element_text(size=font.size), axis.title=element_text(size=font.size,face="bold") )
  theme <- theme(axis.text=element_text(size=font.size), axis.title=element_text(size=font.size,face="bold"), legend.text=element_text(size=font.size), legend.title=element_text(size=font.size), plot.title = element_text(size=font.size) )
  
  plots = list()
  plots.fst = list()
  traits.list <- seq(1:25)
  trait.names <- c("Altitude","Mean temperature","Minimum temperature","Overwintering 2014", "Overwintering 2015", "Overwintering 2016", "1st FT","2nd FT","2nd FT dur","FT DK", "FT 2014", "FT 2015", "FT 2017", "FT 2018", "K ctrl", "Na ctrl", "K salt", "Na salt", "Seed weight", "Seed size", "Seed perimeter", "Seed length", "Seed width", "Seed length/width ratio", "Seed circularity")
  gwas.dot.size=3
  gwas.circle.size=5
  
  # set plot directory
  #setwd("")
  
  # generate the plots
  for (chr in c(1:6)) {
    current.chr <- paste("chr",chr, sep="")
    n=1
    
    for (i in traits.list) {
      
      #gwas
      gwas.chr <- subset(dataFiles[[i]], chr == current.chr)
      x.gwas <- gwas.chr$positions
      y.gwas <- gwas.chr$p
      d.gwas.chr <- as.data.frame(cbind(x.gwas,y.gwas))

      #fst
      d.fst.chr <- subset(d.fst, chr ==current.chr)
      #fst.genes <- subset(top.100.fst.genes.positions.unique, V1 == current.chr)
      x.fst <- rollapply(d.fst.chr$pos, width = width, by = by, FUN = mean, align = "left")
      y.fst <- rollapply(d.fst.chr$fst, width = width, by = by, FUN = mean, align = "left")
      d.fst.chr.rollmean <- as.data.frame(cbind(x.fst,y.fst))
      q.fst <- ggplot(d.fst.chr.rollmean, aes(x=x.fst, y=y.fst))
      q.fst <- q.fst + geom_point(color="dark grey") + ggtitle(trait.names[n]) + labs(x=paste(current.chr,"position", sep=" "), y="Fst") + theme
      
      #gwas fst overlap
      gwas.fst.overlap.chr <- NULL
      gwas.fst.overlap.chr <- subset(fst.gwas.overlap[[i]], chr.x == current.chr)
      gwas.fst.overlap.chr <- gwas.fst.overlap.chr[order(gwas.fst.overlap.chr$p),]
      
      # produce GWAS plots colored by Fst values and Fst plot colored by GWAS p-values
      if (nrow(gwas.fst.overlap.chr)>0) {
        
        q.gwas <- q.fst + geom_point(data=gwas.fst.overlap.chr, aes(x=pos, y=p/5, color=fst), size=gwas.circle.size, shape=16) + ggtitle(trait.names[n]) + geom_hline(yintercept = (-log( 2.479544e-07,10)/5), color="blue") + labs(x=paste(current.chr,"position", sep=" "), y="Fst or -log(p)/5") + scale_color_viridis(option="plasma") + theme 
                   q.fst.colored <- q.fst + geom_point(data=gwas.fst.overlap.chr, aes(x=pos, y=fst, color=p), size=gwas.circle.size, shape=16 ) + ggtitle(trait.names[n]) + labs(x=paste(current.chr,"position", sep=" "), y="Fst") + scale_color_viridis() + theme } else {
                     q.gwas <- ggplot(d.fst.chr.rollmean, aes(x=x.fst, y=y.fst)) + geom_point(aes(color="Fst"), shape=16) + scale_color_manual(breaks = 'Fst', values = 'dark grey', guide = guide_legend(title = NULL))  + ggtitle(trait.names[n]) + labs(x=paste(current.chr,"position", sep=" "), y="Fst") + theme
                     q.fst.colored <- ggplot(d.fst.chr.rollmean, aes(x=x.fst, y=y.fst)) + geom_point(aes(color="Fst"), shape=16) + scale_color_manual(breaks = 'Fst', values = 'dark grey', guide = guide_legend(title = NULL))  + ggtitle(trait.names[n]) + labs(x=paste(current.chr,"position", sep=" "), y="Fst") + theme}
                 
        
      plots[[n]] = q.gwas
      plots.fst[[n]] = q.fst.colored
      
      n=n+1
      
    }
    
    #print plots
    #filename <- paste(Sys.Date(), fst.file, "chr", chr, ".png", sep="_")
    #png(filename, 24,3*length(traits.list), units="in", res=100)
    #do.call("grid.arrange", c(c(plots.fst,plots), nrow=length(traits.list), ncol=2, as.table=FALSE))  
    #dev.off()
    

    #print plots
    # fst colored by GWAS
    filename <- paste(Sys.Date(), fst.file, "chr", chr, ".png", sep="_")
    png(filename, 12,3*length(traits.list), units="in", res=150)
    grid.newpage()
    grid.draw(rbind(ggplotGrob(plots.fst[[1]]), 
                    ggplotGrob(plots.fst[[2]]),
                    ggplotGrob(plots.fst[[3]]),
                    ggplotGrob(plots.fst[[4]]), 
                    ggplotGrob(plots.fst[[5]]),
                    ggplotGrob(plots.fst[[6]]),
                    ggplotGrob(plots.fst[[7]]),
                    ggplotGrob(plots.fst[[8]]),
                    ggplotGrob(plots.fst[[9]]),
                    ggplotGrob(plots.fst[[10]]),
                    ggplotGrob(plots.fst[[11]]),
                    ggplotGrob(plots.fst[[12]]),
                    ggplotGrob(plots.fst[[13]]),
                    ggplotGrob(plots.fst[[14]]),
                    ggplotGrob(plots.fst[[15]]),
                    ggplotGrob(plots.fst[[16]]),
                    ggplotGrob(plots.fst[[17]]),
                    ggplotGrob(plots.fst[[18]]),
                    ggplotGrob(plots.fst[[19]]),
                    ggplotGrob(plots.fst[[20]]),
                    ggplotGrob(plots.fst[[21]]),
                    ggplotGrob(plots.fst[[22]]),
                    ggplotGrob(plots.fst[[23]]),
                    ggplotGrob(plots.fst[[24]]),
                    ggplotGrob(plots.fst[[25]]),			
                    size = "first"))
    dev.off() 

    # GWAS colored by FST
    filename <- paste(Sys.Date(), fst.file, "gwa_chr", chr, ".png", sep="_")
    png(filename, 12,3*length(traits.list), units="in", res=150)
    grid.newpage()
    grid.draw(rbind(ggplotGrob(plots[[1]]), 
                    ggplotGrob(plots[[2]]),
                    ggplotGrob(plots[[3]]),
                    ggplotGrob(plots[[4]]), 
                    ggplotGrob(plots[[5]]),
                    ggplotGrob(plots[[6]]),
                    ggplotGrob(plots[[7]]),
                    ggplotGrob(plots[[8]]),
                    ggplotGrob(plots[[9]]),
                    ggplotGrob(plots[[10]]),
                    ggplotGrob(plots[[11]]),
                    ggplotGrob(plots[[12]]),
                    ggplotGrob(plots[[13]]),
                    ggplotGrob(plots[[14]]),
                    ggplotGrob(plots[[15]]),
                    ggplotGrob(plots[[16]]),
                    ggplotGrob(plots[[17]]),
                    ggplotGrob(plots[[18]]),
                    ggplotGrob(plots[[19]]),
                    ggplotGrob(plots[[20]]),
                    ggplotGrob(plots[[21]]),
                    ggplotGrob(plots[[22]]),
                    ggplotGrob(plots[[23]]),
                    ggplotGrob(plots[[24]]),
                    ggplotGrob(plots[[25]]),			
                    size = "first"))
    dev.off() 
   
  }
  
  
  
}

## Calculate expected and observed heterozygosity and selfing rate
{
  setwd("")
  
  #install.packages("adegenet", dependencies = TRUE)
  library("adegenet")
  #install.packages("hierfstat", dependencies = TRUE)
  library("hierfstat")
  #install.packages("pegas", dependencies = TRUE)
  library("pegas")
  
  whichpop=read.table("PopulationMembership.csv",sep=";",header=T) #This is a file with 2 columns. First column contains accession name, second column contain a 1,2 or 3 depending on which population the accession belongs to. The file has 136 rows, one for each accession.
  genotypes_all_ind=read.table("all_chromosomes_binary.csv",sep=",") #The genotype file with a row for each SNP and the two first columns describing Chr and positions. These will not be included in analyses.
  colnames(genotypes_all_ind)=c("CHROM","POS","Gifu","mg001","mg002","mg003","mg004","mg005","mg007","mg008","mg009","mg010","mg011","mg012","mg013","mg014","mg016","mg017","mg018","mg019","mg020","mg021","mg022","mg023","mg024","mg025","mg026","mg027","mg028","mg030","mg032","mg033","mg034","mg035","mg036","mg038","mg039","mg040","mg041","mg042","mg044","mg045","mg046","mg049","mg050","mg051","mg052","mg053","mg056","mg057","mg058","mg059","mg060","mg061","mg062","mg063","mg065","mg066","mg067","mg068","mg069","mg070","mg071","mg072","mg073","mg074","mg075","mg076","mg077","mg078","mg080","mg081","mg082","mg083","mg084","mg085","mg086","mg088","mg089","mg090","mg091","mg092","mg093","mg094","mg095","mg096","mg097","mg098","mg099","mg100","mg101","mg102","mg103","mg104","mg105","mg106","mg107","mg109","mg110","mg111","mg112","mg113","mg115","mg116","mg117","mg118","mg119","mg120","mg121","mg123","mg124","mg125","mg126","mg127","mg128","mg129","mg130","mg131","mg132","mg133","mg134","mg135","mg136","mg138","mg139","mg140","mg141","mg142","mg143","mg144","mg145","mg146","mg149","mg150","mg151","mg152","mg154","mg155","mg156")
  
  t_genotypes_all_ind=t(genotypes_all_ind) #Transpose the genotype matrix
  t_genotypes_all_ind_new=t_genotypes_all_ind[3:nrow(t_genotypes_all_ind),] #To get SNPs only and not position and chromosome information
  
  keep=as.logical(rownames(t_genotypes_all_ind_new) %in% whichpop[,1])
  t_genotypes_all_ind_new=t_genotypes_all_ind_new[keep,]
  nrow(t_genotypes_all_ind_new)==nrow(whichpop) #check
  
  dim(t_genotypes_all_ind_new) #dimensions are 136 x 201694
  colnames(t_genotypes_all_ind_new)=seq(1:ncol(t_genotypes_all_ind_new)) #naming SNPs from 1 to 201694
  
  t_genotypes_all_ind_new[t_genotypes_all_ind_new==2]=22 #Changing genotype format
  t_genotypes_all_ind_new[t_genotypes_all_ind_new==1]=12 #Changing genotype format
  t_genotypes_all_ind_new[t_genotypes_all_ind_new==0]=11 #Changing genotype format
  
  New_table=cbind(whichpop,t_genotypes_all_ind_new)
  dim(New_table)
  
  
  pop1ind=which(New_table[,2]==1) #Including population 1 individuals only
  pop1=New_table[pop1ind,]
  
  #pop2ind=which(New_table[,2]==2)
  #pop2=New_table[pop2ind,]
  
  #pop3ind=which(New_table[,2]==3)
  #pop3=New_table[pop3ind,]
  
  
  locus=pop1[,3:ncol(pop1)]
  ind=as.character(pop1[,1])
  
  #SMALLGENO=locus[,1:10]
  
  #Mydata1<- df2genind(t_genotypes_all_ind_new,ploidy=2,ind.names=row.names(t_genotypes_all_ind_new),pop=as.character(whichpop[,2]), sep = "")
  Mydata1<- df2genind(locus,ploidy=2,ind.names=ind, sep = "")
  Mydata1
  nAll(Mydata1)
  div <- summary(Mydata1)
  div
  names(div)
  div$Hobs
  div$Hexp
  write.table(div$Hobs,"obspop1.txt",row.names=F) #A file containing all observed heterozygosity for each loci
  write.table(div$Hexp,"exppop1.txt",row.names=F) #A file containing all expected heterozygosity for each loci
  
  setwd("")
  obs=read.table("obspop1.txt",head=T)
  exp=read.table("exppop1.txt",head=T)
  FIS1=1-colMeans(obs)/colMeans(exp)
  FIS1
  s1=2*FIS1/(1+FIS1)
  s1
  
  obs=read.table("obspop2.txt",head=T)
  exp=read.table("exppop2.txt",head=T)
  FIS2=1-colMeans(obs)/colMeans(exp)
  FIS2
  s2=2*FIS2/(1+FIS2)
  s2
  
  obs=read.table("obspop3.txt",head=T)
  exp=read.table("exppop3.txt",head=T)
  FIS3=1-colMeans(obs)/colMeans(exp)
  FIS3
  s3=2*FIS3/(1+FIS3)
  s3
  
  Average_FIS=(FIS1+FIS2+FIS3)/3
  Average_selfing=(s1+s2+s3)/3
}
