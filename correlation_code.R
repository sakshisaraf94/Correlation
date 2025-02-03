library(ggplot2) 
library(caret)
library(dplyr)
library(stats)
library(regclass)
library(corrplot)
library(lattice)
library(ggplot2)
library(reshape2)

sample <- read.csv("D:/Correlation/Training_1623_a.csv")
head(sample)
#To remove duplicate rows using specific columns  
sample <- sample[!duplicated(sample[,1:39]), ]    
#Remove rows with NA values 
sample1 <- sample[complete.cases(sample[,8:39]), ]

#covert dataframe to matrix
x <- as.matrix(sample1[,c(8:39)])
# LET'S PUT ALL THE PREDICTORS ON A COMPARABLE SCALE 
xs <- scale(x,center = TRUE, scale = TRUE)

y <- sample1$Percent
data.scale <- data.frame(y,xs)
# correlation matrix 
corMatrix <- round(cor(xs),2)
corMatrix
# highly correlated variables
findCorrelation(corMatrix, cutoff = 0.7, names = TRUE)
#Dropping the highly correlated variables 
drops <- c("NDYI","Green","LSWI","NDWI","TCG","T_MAM","T_MAMcv","TCW","T_JJAcv","T_JJA","MAP2","Snow_c","SWEcv","SWE") 
data.scale1 <- data.scale[ , !(names(data.scale) %in% drops)]

#In case required for pearson or spearman correlation
df <- data.scale1[,c(2:19)]
df1 <- as.matrix(df)
res <- cor(df1, method = "pearson") 
res
findCorrelation(res, cutoff = 0.7, names = TRUE)  

#Visualization or Correlation plot
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

cormat <- round(res,2)
cormat

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
} 

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "#BB4444", high = "#4477AA", mid = "#FFFFFF", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1,face='bold'),axis.text.y = element_text(face='bold',size = 9) )+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(2, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

setwd("D:/Correlation")
dev.print(tiff, "correlationplot.tif", res=600, height=10, width=10, units="in")
