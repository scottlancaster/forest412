#-------------------------------------------------------------------------------
# Forest Cover Project
# PREDICT 412-DL Section 55 Winter 2015
#
# MASTER Copy -- this is the master version from all team contributors
#
# Team forest412:
# Arasu Narayan
# Arina Polukhina
# James Clark
# Scott Lancaster
# Steve Pomeroy
#
# Perform a data quality check as well as exploratory data analysis of the
# Forest Cover data
#

setwd("/Users/Scott/Documents/GitHub/forest412")

#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages("Lattice")

library(corrplot)
library(ggplot2)
library(lattice)

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
forest.cover$Cover_Name <- cover.types$types[match(forest.cover$Cover_Type, cover.types$id)]

# Obtain summary
summary(forest.cover)
str(forest.cover)
nrow(forest.cover)

# Get the count for each cover type class
table(forest.cover$Cover_Type)
table(forest.cover$Cover_Name)

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
