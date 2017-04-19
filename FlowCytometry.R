data()
load(airquality)
?load
getwd()
library(ggplot2)
library(gplots)
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
nba$Name <- with(nba, reorder(Name, PTS))
nba.m <- melt(nba)
library (gplots)
mat <- matrix(round(runif(n=30^2, min=0, max=1),2), nrow=30, ncol=30)
#Let's use a submatrix to play
minimat <- mat[1:10,1:10]

# Using cut to build a factor with the different groups
# and change it to 1 to 3 scale
groups <- cut(minimat,c(0,0.4,0.6,1))

levels(groups)
levels(groups) <- 1:length(levels(groups))
groups<-matrix(as.numeric(groups), nrow= 10)

# Build your palette
my_palette <- c("green","red", "blue")

# In heatmap.2 use groups as values to color with your palette 
# and minimat to display values.
heatmap.2(x = groups, Rowv = FALSE, Colv = FALSE, col = my_palette, dendrogram = "none", 
          cellnote = minimat, notecol = "black", notecex = 2, trace = "none", key = FALSE, margins = c(2, 2))

source("https://bioconductor.org/biocLite.R")
biocLite("cytofkit")
library(cytofkit)
library("cytofkit") 


cytofkit_GUI()  
set.seed(100)
dir <- system.file('extdata',package='cytofkit')
file <- list.files(dir ,pattern='.fcs$', full=TRUE)
file

parameters <- list.files(dir, pattern='.txt$', full=TRUE)
paraFile <- list.files(dir, pattern='.txt$', full=TRUE)
parameters <- as.character(read.table(paraFile, header = TRUE)[,1])
data_transformed <- cytof_exprsExtract(fcsFile = file, markers = parameters, 
                                       comp = FALSE, 
                                       transformMethod = "cytofAsinh")
head(data_transformed[ ,1:3])
data_transformed_1k <- data_transformed[1:1000, ]

## run PhenoGraph
cluster_PhenoGraph <- cytof_cluster(xdata = data_transformed_1k, method = "Rphenograph")

data_transformed_1k_tsne <- cytof_dimReduction(data=data_transformed_1k, method = "tsne")


cluster_ClusterX <- cytof_cluster(ydata = data_transformed_1k_tsne,  method="ClusterX")
cluster_FlowSOM <- cytof_cluster(xdata = data_transformed_1k, method = "FlowSOM", FlowSOM_k = 12)
data_1k_all <- cbind(data_transformed_1k, data_transformed_1k_tsne, 
                     PhenoGraph = cluster_PhenoGraph, ClusterX=cluster_ClusterX, 
                     FlowSOM=cluster_FlowSOM)
data_1k_all <- as.data.frame(data_1k_all)
data_1k_all <- data_1k_all[order(data_1k_all$ClusterX),]
cytof_clusterPlot(data=data_1k_all, xlab="tsne_1", ylab="tsne_2", 
                 cluster="PhenoGraph", sampleLabel = FALSE)
PhenoGraph_cluster_median <- aggregate(. ~ PhenoGraph, data = data_1k_all, median)
cytof_heatmap(PhenoGraph_cluster_median[, 2:37], baseName = "PhenoGraph Cluster Median")

cytof_clusterPlot(data= data_1k_all, xlab="tsne_1", ylab="tsne_2", 
                  cluster="ClusterX", sampleLabel = FALSE)
ClusterX_cluster_median <- aggregate(. ~ ClusterX, data = data_1k_all, median)

cytof_heatmap(ClusterX_cluster_median[, 2:37],  baseName = "ClusterX Cluster Median")
color.palette  <- colorRampPalette(c('#fee8c8','#fdbb84','#e34a33'))
quantile.range <- quantile(p_d, probs = seq(0, 1, 0.01))
palette.breaks <- seq(quantile.range["5%"], quantile.range["95%"], 0.1)
color.palette  <- colorRampPalette(c("#FC8D59", "#FFFFBF", "#91CF60"))(length(palette.breaks) - 1)

heatmap.2(as.matrix(ClusterX_cluster_median[, 2:37]),
          density.info="none",  
          #baseName = "ClusterX Cluster Median",
          trace="none",         
          margins =c(12,9),    
          #col=greenred(5), 
          col =color.palette,
          dendrogram='none',     
          Rowv=FALSE,
          Colv=FALSE)     
cytof_clusterStat(data = data_1k_all, cluster = "Phenograph",parameters,
                  statMethod =  "mean")
cytof_colorPlot(data, xlab, ylab, zlab, colorPalette = c("bluered",
                                                         "spectral", "heat"), pointSize = 1, removeOutlier = TRUE)
PhenoGraph_progression <- cytof_progression(data = data_transformed_1k, 
                                            cluster = cluster_PhenoGraph, 
                                            method="isomap", clusterSampleSize = 50, 
                                            sampleSeed = 5)
p_d <- data.frame(PhenoGraph_progression$sampleData, 
                  PhenoGraph_progression$progressionData, 
                  cluster = PhenoGraph_progression$sampleCluster, 
                  check.names = FALSE)
test <- data_1k_all[order(data_1k_all$ClusterX),]
colnames(data_1k_all)
data_1k_all$cluster
cytof_clusterPlot
markers <- c("(Sm150)Di<GranzymeB>", "(Yb173)Di<Perforin>")

cytof_colorPlot(data=p_d, xlab="isomap_1", ylab="isomap_2", zlab = markers[2],colorPalette = "spectral",
                pointSize = 1, removeOutlier = TRUE)
cytof_progressionPlot(data=p_d, markers, orderCol="isomap_1", clusterCol = "cluster")
m1 <- c(rnorm(300, 10, 2), rnorm(400, 4, 2), rnorm(300, 7))
m2 <- c(rnorm(300, 4), rnorm(400, 16), rnorm(300, 10, 3))
m3 <- c(rnorm(300, 16), rnorm(400, 40, 3), rnorm(300, 10))
m4 <- c(rnorm(300, 7, 3), rnorm(400, 30, 2), rnorm(300, 10))
m5 <- c(rnorm(300, 27), rnorm(400, 40, 1),rnorm(300, 10))
c <- c(rep(1,300), rep(2,400), rep(3,300))
rnames <- paste(paste('sample_', c('A','B','C','D'), sep = ''),
                rep(1:250,each = 4), sep='_')
exprs_cluster <- data.frame(cluster = c, m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5)
row.names(exprs_cluster) <- rnames
cytof_clusterStat(data = exprs_cluster, cluster = "cluster", statMethod = "percentage")
cytof_clusterStat(data = p_d, cluster = "cluster", statMethod = "percentage")
order(rownames(p_d))
summarise(p_d)
table(p_d$cluster)
length(p_d$cluster==9)
unique(p_d$cluster)
p_d_cluster <-subset(p_d, p_d$cluster == 5)
length(p_d_cluster)
cytof_colorPlot(data= (p_d[which(p_d$cluster ==5)]), xlab="isomap_1", ylab="isomap_2", zlab = markers[2],colorPalette = "spectral",
                pointSize = 1, removeOutlier = TRUE)