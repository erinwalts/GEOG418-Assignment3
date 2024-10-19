# GEOG418-Assignment3
Introduction
  Describe libraries
```{r Libraries, eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE}
#install.packages("knitr")
#install.packages("tmap")
#install.packages("spdep")
#install.packages("raster")
#install.packages("shinyjs")
#install.packages("e1071")
#install.packages("sf")
#install.packages("st")
#load libraries
library("knitr")
library("tmap")
library("spdep")
library("raster")
library("shinyjs")
library("e1071")
library("sf")
library("st")
#no rgdal available so deleted
```

  Describe shapefiles, and census data files:

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#set directory:
dir <- "C:/Users/Owner/Documents/UVIC_2024-2025/GEOG418/Assignment_3"
setwd(dir)
#set directory;
#From the working dir read in the csv
csv <- read.csv("ucgsJQnBVLvP_data.csv") 
#Data source is the working dir (where the layer is), layer is the name of the file (without .shp)
shp <- st_read("lda_000a16a_e.shp")
```
  Describe clean up

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#CLEAN UP DATA
#new column
cols <- c("GEO UID", "Province code", "Province name", "CD code",
          "CD name", "DA name", "Population", "Land area", 
          "Median total income", "Income Sample Size", "French Knowledge", 
          "Language Sample Size")
#Apply those names to dataframe
colnames(csv) <- cols
#Add column to count number of ID characters
csv$len <- nchar(csv$`GEO UID`)
#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)
#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)
#Choose a city
Municp <- subset(census_DAs, census_DAs$CMANAME == "Kelowna")
#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

Describe only using relevant data:

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#CONFIRM CLEAN UP OF DATA
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$"Median total income")),]
#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$"French Knowledge")),]
```

Describe the descriptive stats:

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#ANALYSIS
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$"Median total income")
stdevIncome <- sd(Income_noNA$"Median total income")
skewIncome <- skewness(Income_noNA$"Median total income")
#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$"French Knowledge")
stdevFrench <- sd(French_noNA$"French Knowledge")
skewFrench <- skewness(French_noNA$"French Knowledge")
#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))
#Produce table
kable(data, caption = paste0("Descriptive statistics for ", 2016, " census variables"))
```

Describe how map is created:

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#hatch inside
#Choose a palette
tmaptools::palette_explorer() #Tool for selecting palettes

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "viridis", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "inferno", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))
#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```

Neighbourhood Matrix
  describe concept of weighted neighbourhood matrix

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}
#Income Neighbours - Queens weight
# Use st_coordinates to get the coordinates:
Income.nb <- poly2nb(Income_noNA)
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net) <- crs(Income_noNA)
#Income Neighbours - Rooks weight
#CRS projargs should not be NULL; set to NA? -warning
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)
#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)
#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net2) <- crs(French_noNA)
```

Explain how maps (weighted rook & queen) below are created and what they show:

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}
#Make queens map
#Choose a palette
#tmaptools::palette_explorer()
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='black') + 
  tm_shape(Income.net) + tm_lines(col='blue')
#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='black') + 
  tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)
#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='black') + 
  tm_shape(Income.net) + tm_lines(col='blue', lwd = 2) +
  tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)
#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)
```

Describe code for weighted matrix file:

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")
#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")
#head(Income.lw[["weights"]])[c(1:3)]
#head(Income.lw$weights)[1:3]
#subset then print
subset_weights <- head(Income.lw[["weights"]])[c(1:3)]
print(subset_weights)
```

Global Moran's I:
Explain:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

Explain equation^

```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$"Median total income", Income.lw, zero.policy = TRUE)
#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$"PercFrench", French.lw, zero.policy = TRUE)
#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```

Describe the results:

```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```

Describe what results indicate:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))
#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```
 explain what the Z score mean^

 Local Spatial Autocorrelation
 explain ^
 
$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$"Median total income", Income.lw)
#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for French
lisa.testFrench <- localmoran(French_noNA$"PercFrench", French.lw)
#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```

describe mapping it:

```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#can change colours if I want:
#Choose a palette
#tmaptools::palette_explorer()
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```

Explain the results

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$"Median total income", Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```

```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$"PercFrench", French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```

Explain scatterplots^

Summary:

References:
