# Spatial Autocorrelation
### Introduction:
Spatial autocorrelation is a type of statistical test that compares how similar or dissimilar a variable is within a dataset on a geographical space [1]. It calculates the correlation between observations within a study area to determine if the variable is clustered, random, or dispersed, similar to nearest neighbour, K function, and quadrat analysis. This test follows Tobler's first law of geography: "everything is related to everything else, but near things are more related than far things" [2], the assumption that observations closer together are more related, and observations farther apart are less related, conclusion about the spatial patterns of the dataset can be conducted. Spatial autocorrelation uses distance and variability to measure similarity or dissimilarity of observations and is useful to determine where clustering or lack of clustering occurs. It is also helpful in supplying the degrees of spatial autocorrelation, or how related things are based on distance, and this can be used to measure the strength of spatial effects in observations[3]. Positive spatial autocorrelation refers to clustering of points, and negative spatial autocorrelation refers to dispersed points. If a variables follows a random distribution, there is no spatial autocorrelation present in the dataset.

For this tutorial, we are conducting spatial autocorrelation on census data for the city of Kelowna, BC. The census data was obtained from statistics Canada (2016) and contains the two variables 'French Knowledge' and 'Median Total Income', which will be analyzed for their spatial distribution. Census data is a useful source of information to use in spatial autocorrelation analysis, as it contains data on many variables (ex: income or population size), and is updated every five years[4]. 

To conduct spatial autocorrelation in this tutorial, we are using R Studio and have to install several packages. In the code below, the '#' hashtag symbol indicates a note within R, and won't be read as code. Delete this symbol to enable the packages to be installed. Libraries are the directories where these packages are stored and have to be loaded into the code to be enabled[5].
  
```{r Libraries, eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE}
#install.packages("knitr")
#install.packages("tmap")
#install.packages("spdep")
#install.packages("raster")
#install.packages("shinyjs")
#install.packages("e1071")
#install.packages("sf")
#install.packages("st")

library("knitr")
library("tmap")
library("spdep")
library("raster")
library("shinyjs")
library("e1071")
library("sf")
library("st")
```

A directory is where the code will pull the data from, set your working directory as a file location where your data is stored. Once your directory is set, data files like csv's or shapefiles can be called into R. We will call the csv file containing the census data, and the shapefile containing spatial data for BC.

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#set directory:
dir <- "C:/Users/Owner/Documents/UVIC_2024-2025/GEOG418/Assignment_3"
setwd(dir)
csv <- read.csv("ucgsJQnBVLvP_data.csv") 
shp <- st_read("lda_000a16a_e.shp")
```

Since the census data contains information for all of Canada, we will create a new column to include relevant attributes. This column will be added to a dataframe called 'colnames', and a filter will be applied to remove data IDs with less than eight numbers to clean up the census data. The spatial and aspatial data, which includes the shapefile and cleaned csv file, are merged into a new object called 'censuus_DAs'.

We will choose the city of Kelowna as the study area for this tutorial, and create a new object called 'Municp' that contains census and spatial data only for Kelowna. The variable 'French Knowledge' is converted to a rate in percentage of French speakers.

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
cols <- c("GEO UID", "Province code", "Province name", "CD code",
          "CD name", "DA name", "Population", "Land area", 
          "Median total income", "Income Sample Size", "French Knowledge", 
          "Language Sample Size")
colnames(csv) <- cols
#Add column to count number of ID characters
csv$len <- nchar(csv$`GEO UID`)
#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

Municp <- subset(census_DAs, census_DAs$CMANAME == "Kelowna")
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

Before we apply statistical anlaysis, the two variables need to be cleaned to remove any values containing NULL or 0s, which can impact the final results.

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
Income_noNA <- Municp[which(!is.na(Municp$"Median total income")),]
French_noNA <- Municp[which(!is.na(Municp$"French Knowledge")),]
```
### Descriptive Statistics
The first step in statistical analysis is conducting descriptive statistics on our two variables, French Knowledge and Median Total Income, which includes calculating the mean, standard deviation, and skewness to determine the distribution of the data along the mean. These results are put into a table so we can see the data succinctly.

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
meanIncome <- mean(Income_noNA$"Median total income")
stdevIncome <- sd(Income_noNA$"Median total income")
skewIncome <- skewness(Income_noNA$"Median total income")

meanFrench <- mean(French_noNA$"French Knowledge")
stdevFrench <- sd(French_noNA$"French Knowledge")
skewFrench <- skewness(French_noNA$"French Knowledge")

data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))
kable(data, caption = paste0("Descriptive statistics for ", 2016, " census variables"))
```

Next, we will create a map that shows the distribution of the two variables in Kelowna. The function: tmaptools::palette_explorer(), is a selection tool used to choose colour palettes; a new window will open with palettes available, make sure to delete this window before continuing on in the code. 
Maps for the two variables are created by first calling the variable's census dataframe to a shape, and then using the polygon and layout function to create the map. The polygon function calls the variable attribute column as the data source and includes other features like title, style which describes how the data is classified and we are using the Jenks method which are natural breaks[6], and border lines. In the layout function, external map features like the legend are and its position are created.

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#Choose a palette
tmaptools::palette_explorer() #Tool for selecting palettes

map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "viridis", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "viridis", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))
#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```

### Neighbourhood Matrix
To spatially quantify autocorrelation, the study area is overlain with a raster grid to determine what is a neighbour of an observed point. A neighbour are the cells around a observation point, there are several ways to describe which cells are neighbours and which are not. In this tutorial we use the rook and the queen weights. The rook weight assigns the top, bottom, left and right squares of a observation point as the neigbours, think of a rook chess piece that can only move horizontally and vertically. The queen weight assigns every cell touching the observation point as a neighbour, think of the queen piece which can move horizontally, vertically, and diagonally. A cell designated as a neighbour receives a value of '1', and non-neighbour cells are valued as '0', following the binary system.

The function poly2nb() is a function used to change the neighbourhood matrix between queen and rook weights. By inserting 'queen - FALSE', we can change from a queen weight to a rook weight [7]. Both the rook and queen weights are applied to the two variables.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}
#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net) <- crs(Income_noNA)
#Income Neighbours - Rooks weight
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

Next, we will map the rook and queen weights separately, and then together in a three pane map window. Similar to the previous map, a shape is created with the median income variable with borders on black, and another shape is created with the queen weight as calculated in the previous code, 'Income.net'. This is done with the rook weight as well. To combine the two maps, '+' symbols are used to join together the queen and rook codes. 
These maps display the connection of observation points to each other in shown by the blue lines[8]. Since the study area is quite large, the differences between the two weight types are small but are distinct enough that between the rook and queen maps several lines are missing. The type of neighbour matrix, rook or queen, determines what constitutes a neighbour, which is why the queen map has more lines connecting points than the rook map. 

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kelowna census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}
#Queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='black') + 
  tm_shape(Income.net) + tm_lines(col='blue', lwd = 1)
#Rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='black') + 
  tm_shape(Income.net2) + tm_lines(col='blue', lwd = 1)
#Combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='black') + 
  tm_shape(Income.net) + tm_lines(col='blue', lwd = 1) +
  tm_shape(Income.net2) + tm_lines(col='blue', lwd = 1)
#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)
```
### Weighted Matrix
The weight matrix is basically how the R code reads the grid cells of the study area based on a defined neighbourhood structure (rook or queen). The categorization for how the code reads the cells is defined by the style, or type, there are three types: B, C, and W. The code sets up an n x n matrix that goes through the grid cells assigning values based on the chosen style. The B style is the basic binary coding with 0s and 1s, C is globally standardized where all neighbours in the dataset are applied the same weight, and W is row standardized where all the neighbours along a row are divided by the row sum [9]. In this tutorial, we will use the W function as it assigns an equal weight to each neighbour making them spatially contiguous; each observation value is proportionally represented [3].
The function 'nb2lines' of the 'spdep' package is used to create this matrix with our variable objects 'Income.nb' and 'French.nb'. Because we want the code to run even if there are observations with no neighbour links, we set the 'zero.policy = TRUE', and this will give these cells weights of 0 [9]. We can then print the final matrix with the 'print.listw' function, to assess the weights assigned from observation 'i' to neighbour 'j' for the two variables. 

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Income matrix:
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")
Income.matrix <- head(Income.lw[["weights"]])[c(1:3)]
#French matrix:
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")
French.matrix <- head(French.lw[["weights"]])[c(1:3)]
subset_weights <- c(Income.matrix, French.matrix)
print(subset_weights)
```

### Global Moran's I:
The Global Moran's I is a test statistic that calculates one value for the entire dataset, in this case for each of our variables. This test determines the spatial autocorrelation of the dataset, how similar (clustered or positive) or dissimilar (dispersed or negative) the observations within the dataset are to each other [3]. The Global Moran's I equation is shown below:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

This equation sums the multiple 'i' (observation) and 'j' (neighbour) values together with an assigned weight 'w'. This value is multiplied by the observation value 'x(i)' subtracted from the observation mean for the entire dataset, and multiply by the neighbour value 'x(j)' subtracted from the neighbour mean for the entire dataset [10]. The denominator standardizes the resultant value since we divide by a squared value. 

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
