### Function to plot hexagonal cells
# install.packages('fields')
library(RColorBrewer) #to use brewer.pal
library(fields) #to use designer.colors

#Function to create the polygon for each hexagon
Hexagon <- function (x, y, unitcell = 1, col = col) {
  polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell,
            x + unitcell/2), c(y + unitcell * 0.125,
                               y + unitcell * 0.875,
                               y + unitcell * 1.125,
                               y + unitcell * 0.875,
                               y + unitcell * 0.125,
                               y - unitcell * 0.125),
          col = col, border=NA)
}#function

Hexagon <- function (x, y, unitcell = 1, col = col) {
  polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell,
            x + unitcell/2), c(y + unitcell * 0.125,
                               y + unitcell * 0.875,
                               y + unitcell * 1.125,
                               y + unitcell * 0.875,
                               y + unitcell * 0.125,
                               y - unitcell * 0.125),
          col = col, border='black')
}#function

######################################################################

##########################################Import your data
setwd('/Users/ashle/Desktop/MMV/')
dataset= as.matrix(read.csv('Updated_mar_oc_mmvRank_PBV(2um).csv', sep = ',', header = F))


#Start with a matrix that would be the numerical representation of you heatmap
#called Heatmap_Matrix
#This matrix has the same number of rows as the SOM map
#and the same number of columns as the SOM map
#and each value in the Heatmap represents the value for one hexagon
#Here [1,1] will become the lower left node (1st row, 1st column),
#[1,2] will become the node to the right
#[2,1] will be the first node to the left in the second row
#So visually you work your way from bottom left to top right
x <- as.vector(dataset)

#Number of rows and columns of your SOM
SOM_Rows <- dim(dataset)[1]
SOM_Columns <- dim(dataset)[2]

#To make room for the legend
par(mar = c(0.4, 2, 0.5, 7))

#Initiate the plot window but do show any axes or points on the plot
plot(0, 0, type = "n", axes = F, xlim=c(0, SOM_Columns),
     ylim=c(0, SOM_Rows), xlab="", asp=1 )

# For inhibition use this code
ColRamp <- rev(designer.colors(n=50, col=brewer.pal(9, "Spectral")))

# For viability use this code
ColRamp <- designer.colors(n=50, col=brewer.pal(9, "Spectral"))

#Make a vector with length(ColRamp) number of bins between the minimum and
#maximum value of x.
#Next match each point from x with one of the colors in ColorRamp
ColorCode <- rep("#FFFFFF", length(x)) #default is all white
Bins <- seq(min(x, na.rm=T), max(x, na.rm=T), length=length(ColRamp))
for (i in 1:length(x))
  if (!is.na(x[i])) ColorCode[i] <- ColRamp[which.min(abs(Bins-x[i]))]

#Actual plotting of hexagonal polygons on map
offset <- 0.5 #offset for the hexagons when moving up a row
for (row in 1:SOM_Rows) {
  for (column in 0:(SOM_Columns - 1))
    Hexagon(column + offset, row - 1, col = ColorCode[row + SOM_Rows * column])
  offset <- ifelse(offset, 0, 0.5)
}

#Add legend to the right if you want to
image.plot(legend.only=TRUE, col=ColRamp, zlim=c(min(x, na.rm=T), max(x, na.rm=T)))

###To label rows and columns of the plot rough code was used to insert the text
###This part of the code still needs revision to make it userfriendly

text(x= 4.6, y = 20.7, labels = 'a', family='sans', 
     cex = 1)
text(x= 5.4, y = 20.7, labels = 'b', family='sans', 
     cex = 1.1)
text(x= 6.2, y = 20.7, labels = 'c', family='sans', 
     cex = 1.1)
text(x= 6.9, y = 20.7, labels = 'd', family='sans', 
     cex = 1.1)
text(x= 7.7, y = 20.7, labels = 'e', family='sans',
     cex = 1.1)
text(x= 8.5, y = 20.7, labels = 'f', family='sans',
     cex = 1.1)
text(x= 9.2, y = 20.7, labels = 'g', family='sans',
     cex = 1.1)
text(x= 10, y = 20.7, labels = 'h', family='sans',
     cex = 1.1)
text(x= 10.7, y = 20.7, labels = 'i', family='sans',
     cex = 1.1)
text(x= 11.5, y = 20.7, labels = 'j', family='sans',
     cex = 1.1)
text(x= 12.2, y = 20.7, labels = 'k', family='sans',
     cex = 1.1)
text(x= 13, y = 20.7, labels = 'l', family='sans',
     cex = 1.1)
text(x= 13.7, y = 20.7, labels = 'm', family='sans',
     cex = 1.1)
text(x= 14.5, y = 20.7, labels = 'n', family='sans',
     cex = 1.1)
text(x= 15.2, y = 20.7, labels = 'o', family='sans',
     cex = 1.1)
text(x= 16, y = 20.7, labels = 'p', family='sans',
     cex = 1.1)
text(x= 16.7, y = 20.7, labels = 'q', family='sans',
     cex = 1.1)
text(x= 17.5, y = 20.7, labels = 'r', family='sans',
     cex = 1.1)
text(x= 18.3, y = 20.7, labels = 's', family='sans',
     cex = 1.1)
text(x= 19.1, y = 20.7, labels = 't', family='sans', 
     cex = 1.1)

#y-axis
text(x= 20.3, y = 19.7, labels = '1', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 18.6, labels = '2', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 17.6, labels = '3', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 16.6, labels = '4', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 15.6, labels = '5', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 14.6, labels = '6', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 13.5, labels = '7', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 12.5, labels = '8', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 11.5, labels = '9', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 10.5, labels = '10', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 9.5, labels = "11", family='sans', 
     cex = 1.1)
text(x= 20.3, y = 8.5, labels = '12', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 7.5, labels = '13', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 6.5, labels = '14', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 5.4, labels = '15', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 4.4, labels = '16', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 3.4, labels = '17', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 2.4, labels = '18', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 1.4, labels = '19', family='sans', 
     cex = 1.1)
text(x= 20.3, y = 0.3, labels = '20', family='sans', 
     cex = 1.1)

