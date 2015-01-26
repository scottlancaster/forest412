#-------------------------------------------------------------------------------
# Forest Cover Project
# PREDICT 412-DL Section 55 Winter 2015
#
# Scott's Copy -- this is Scott's source code
#
# Scott Lancaster
#
# Perform a data quality check as well as exploratory data analysis of the
# Forest Cover data
#

setwd("/Users/Scott/Documents/GitHub/forest412")

#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages("Lattice")
#install.packages("rattle")
#install.packages("party")
#install.packages("foreach", "iterators")
#install.packages("doParallel")

library(ggplot2)
library(reshape2)
library(lattice)
library(rpart)
library(rattle)
library(party)
library(corrplot)
library(doParallel)

#-------------------------------------------------------------------------------
# Create a custom summary function
my.summary <- function(x) {
    x.num <- sapply(x,is.numeric)   # identify numeric vectors
    x.num.only <- x[,x.num]     # subset of numeric vectors
    mean <- apply(x.num.only, 2, mean)
    var <- apply(x.num.only, 2, var)
    sd <- apply(x.num.only, 2, sd)
    min <- apply(x.num.only, 2, min)
    max <- apply(x.num.only, 2, max)
    quant <- apply(x.num.only, 2, quantile)
    return(rbind(mean, var, sd, min, max, quant))
}

#-------------------------------------------------------------------------------
# Convert binary (or other base) to decimal
# Paul Hiemstra
# http://stackoverflow.com/questions/12892348/in-r-how-to-convert-binary-string-to-binary-or-decimal-value
base2decimal = function(base_number, base = 2) {
    split_base = strsplit(as.character(base_number), split = "")
    return(sapply(split_base, function(x) sum(as.numeric(x) * base^(rev(seq_along(x) - 1)))))
}

#-------------------------------------------------------------------------------
# Load, initialize, and view summary statistics
forest.cover <- read.csv(file = "covtype.data")
colnames(forest.cover) <- c("Elevation",                           
                            "Aspect",
                            "Slope",
                            "Horizontal_Distance_To_Hydrology",
                            "Vertical_Distance_To_Hydrology",
                            "Horizontal_Distance_To_Roadways",
                            "Hillshade_9am",
                            "Hillshade_Noon",
                            "Hillshade_3pm",
                            "Horizontal_Distance_To_Fire_Points",
                            "Wilderness_Area1",
                            "Wilderness_Area2",
                            "Wilderness_Area3",
                            "Wilderness_Area4",
                            "Soil_Type1",
                            "Soil_Type2",
                            "Soil_Type3",
                            "Soil_Type4",
                            "Soil_Type5",
                            "Soil_Type6",
                            "Soil_Type7",
                            "Soil_Type8",
                            "Soil_Type9",
                            "Soil_Type10",
                            "Soil_Type11",
                            "Soil_Type12",
                            "Soil_Type13",
                            "Soil_Type14",
                            "Soil_Type15",
                            "Soil_Type16",
                            "Soil_Type17",
                            "Soil_Type18",
                            "Soil_Type19",
                            "Soil_Type20",
                            "Soil_Type21",
                            "Soil_Type22",
                            "Soil_Type23",
                            "Soil_Type24",
                            "Soil_Type25",
                            "Soil_Type26",
                            "Soil_Type27",
                            "Soil_Type28",
                            "Soil_Type29",
                            "Soil_Type30",
                            "Soil_Type31",
                            "Soil_Type32",
                            "Soil_Type33",
                            "Soil_Type34",
                            "Soil_Type35",
                            "Soil_Type36",
                            "Soil_Type37",
                            "Soil_Type38",
                            "Soil_Type39",
                            "Soil_Type40",
                            "Cover_Type")

# Add the name of each cover type to a new column
cover.types <- data.frame(id = 1:7,
                          types = c("Spruce/Fir", "Lodgepole Pine",
                                    "Ponderosa Pine", "Cottonwood/Willow",
                                    "Aspen", "Douglas-fir", "Krummholz"))
forest.cover$Cover_Name <- cover.types$types[match(forest.cover$Cover_Type,
                                                   cover.types$id)]

# Obtain summary
summary(forest.cover)
my.summary(forest.cover)
str(forest.cover)
nrow(forest.cover)

# Get the count for each cover type class
table(forest.cover$Cover_Type)
table(forest.cover$Cover_Name)
# Now break out the different wilderness areas
forest.cover.Rawah <- forest.cover[forest.cover$Wilderness_Area1==1,]
forest.cover.Neota <- forest.cover[forest.cover$Wilderness_Area2==1,]
forest.cover.Comanche <- forest.cover[forest.cover$Wilderness_Area3==1,]
forest.cover.Poudre <- forest.cover[forest.cover$Wilderness_Area4==1,]
my.summary(forest.cover.Rawah)
my.summary(forest.cover.Neota)
my.summary(forest.cover.Comanche)
my.summary(forest.cover.Poudre)
nrow(forest.cover.Rawah)
nrow(forest.cover.Neota)
nrow(forest.cover.Comanche)
nrow(forest.cover.Poudre)
table(forest.cover.Rawah$Cover_Name)
table(forest.cover.Neota$Cover_Name)
table(forest.cover.Comanche$Cover_Name)
table(forest.cover.Poudre$Cover_Name)

# Convert the 40 different variables for soil type to just one
# Read as a 40-digit binary number and convert to a a number
forest.cover$Wilderness_Area <- paste(forest.cover$Wilderness_Area1,
                                      forest.cover$Wilderness_Area2,
                                      forest.cover$Wilderness_Area3,
                                      forest.cover$Wilderness_Area4,
                                      collapse = "")

wilderness.area <- data.frame(forest.cover[,11:14])

#-------------------------------------------------------------------------------
# Density plots
# Perform a density plot to see how the classes look against each other
densityplot(~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology
            + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways
            + Hillshade_9am + Hillshade_Noon + Hillshade_3pm
            + Horizontal_Distance_To_Fire_Points,
            data=forest.cover,
            group=Cover_Name,
            as.table=T,
            plot.points=F,
            ref=T,
            auto.key=list(columns=4),
            scales=list(x="free", y="free"),
            layout=(c(3,4)))

#-------------------------------------------------------------------------------
# Look at some box plots to get another view of the data and to look at outliers
forest.cover.num <- forest.cover[1:10]
forest.cover.num$Cover_Type <- forest.cover$Cover_Type
forest.cover.num$Cover_Name <- forest.cover$Cover_Name
melt.cover <- melt(forest.cover.num, id=c("Cover_Type", "Cover_Name"))

ggplot(melt.cover,
       aes(factor(Cover_Name), value, color=Cover_Name, fill=Cover_Name)) +
    #ggtitle("Box Plots - Cover Name\") +
    xlab("Forest Cover Numeric Predictors") +
    ylab("Value") +
    facet_wrap(~variable, ncol = 2, scales = 'free_y') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
    geom_boxplot(alpha=.25) +
    theme(text = element_text(size = 20)) +
    theme(legend.position = "top")

classColors = c("blue", "magenta", "darkgreen")
bwplot(value ~ Cover_Name | variable,
       data=melt.cover,
       group=Cover_Name,
       as.table=T,
       ref=T,
       auto.key=list(columns=4),
       scales=list(abbreviate=T, minlength=5, y=list(relation="free")),
       layout=(c(2,5)),
       fill=classColors,
       col="white")

#-------------------------------------------------------------------------------
# Perform a tree analysis
forest.cover.vals <- forest.cover[1:54]
forest.cover.vals$Cover_Name <- forest.cover[,56]
ptime <- proc.time()
forest.cover.tree <- rpart(Cover_Name ~ ., data=forest.cover.vals, method="class")
proc.time() - ptime

#ptime <- proc.time()
#forest.cover.tree <- ctree(Cover_Name ~ ., data = forest.cover.vals)
#proc.time() - ptime
plot(forest.cover.tree, type="simple")

#fancyRpartPlot(forest.cover.tree)

#-------------------------------------------------------------------------------
# This package is much more flexible allowing you to specify the order of the
# data for the correlation matrix as well as various shapes. It has a nice
# default color palette as well.
corr.forest.cover.z <- cor(forest.cover)

# Set the outer margins so text around the edges won't get truncated
oldpar <- par(omi=c(0.25, 0.1, 0.25, 0.1))
corrplot(corr.forest.cover.z, method="color", order = "FPC", tl.col="black",
         tl.srt=45, mar=c(0.1, 0, 0.5, 0),
         title="Correlation Plot of Forest Cover")
par(oldpar)

#-------------------------------------------------------------------------------
