#####################################################################################
# Leverage supply chains in the carbon footprints of production and consumption in UK
#####################################################################################

library(tidyverse)
library(openxlsx)
#install.packages('rmatio')
library(rmatio)

#install.packages('expm')
#library(expm)
#library(readxl)
# install.packages('R.matlab')
#library(R.matlab)


# Year of analysis
year <- 2019

# Create aggregation function
Agg <- function(x,aggkey,dim)
{
  if(dim == 1) x <- t(x)
  colnames(x) <- aggkey
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  if(dim == 1) x <- t(x)
  
  return(x)
}
# Set paths 
path <- list("EXIO" = paste0("W:/WU/Projekte/GRU/04_Daten/MRIO/EXIOBASE/EXIOBASE 3.8/IOT_pxp/","IOT_",year,"_pxp/") ,
             "input" = paste0( getwd(),"/input/"),
             "output" = paste0( getwd(),"/output/") )


### Import labels and codes ###

code <- list( "F" = read.delim( paste0( path$EXIO, "impacts/unit.txt"), header = TRUE, stringsAsFactors = FALSE), 
              "Z" = read.delim( paste0( path$EXIO, "unit.txt"), header = TRUE, stringsAsFactors = FALSE),
              "FinalDemand" = read.delim( paste0( path$EXIO, "finaldemands.txt"), header = TRUE, stringsAsFactors = FALSE),
              "Products" = read.delim( paste0( path$EXIO, "products.txt"), header = TRUE, stringsAsFactors = FALSE),
              "Industries" = read.delim( paste0( path$EXIO, "industries.txt"), header = TRUE, stringsAsFactors = FALSE),
              "Groups" = read.xlsx( paste0(path$input, "IPCC product group concordance and GHG scope classification.xlsx" ) ) )

# Merge/transform/add labels/codes for better handling aggregation steps and processing

code$Z <- left_join( code$Z, code$Groups, by = "sector" )
code$Z$index <- 1:nrow(code$Z)

code[["region"]] <- data.frame( "index" = 1:49, 
                                "name" = unique(code$Z$region), 
                                stringsAsFactors = FALSE )

code[["Y"]] <- data.frame("index" = 1:ncol(Y),
                          code$FinalDemand[,-1],
                          "region" = rep(code$region$name, each = 7),
                          stringsAsFactors = FALSE )

code$F["index"] <- 1:nrow(code$F)

# Re-define regions for aggregation
code[["regionAgg"]] <- code$region

code$regionAgg$name[29:49] <- 'ROW'
code$regionAgg$name[1:27] <- 'EU'

# Read index of UK
code[["UK"]] <- code$Z$index[ code$Z$region == "GB"]



### Import MRIO model ###

# Technology matrix
A <- read.delim( paste0( path$EXIO, "A.txt"), header = FALSE, skip = 3, stringsAsFactors = FALSE)[,c(-1,-2)]
A <- as.matrix(A)

# Transaction matrix
Z <- read.delim( paste0( path$EXIO, "Z.txt"), header = FALSE, skip = 3, stringsAsFactors = FALSE)[,c(-1,-2)]
Z <- as.matrix(Z)

# Final demand block
Y <- read.delim( paste0( path$EXIO, "Y.txt"), header = FALSE, skip = 3, stringsAsFactors = FALSE)[,c(-1,-2)]
Y <- as.matrix(Y)

# Gross production vector
x <- read.delim( paste0( path$EXIO, "x.txt"), header = FALSE, skip = 1, stringsAsFactors = FALSE)[,c(-1,-2)]

# Satellite accounts
F <- read.delim( paste0( path$EXIO, "impacts/F.txt"), header = FALSE, skip = 3, stringsAsFactors = FALSE)[,-1]
F_hh <- read.delim( paste0( path$EXIO, "impacts/F_hh.txt"), header = FALSE, skip = 3, stringsAsFactors = FALSE)[,-1]


### Create Leontief inverse ###
L <- solve( diag(9800) - A )

### Aggregate final demand block across categories ###

Y <- Agg(x = Y, dim = 2, aggkey = code$Y$region)
Y <- Y[, code$region$index[ code$region$name == 'GB'] ]

### Select stressor and start calculation ###

stressor <- data.frame("index" = c(1,4,2), 
                       "name" = c("Value","Carbon","Employment"), 
                       stringsAsFactors = FALSE )

cat(paste("\n\n", year, "processing ...\n"))

for( str in 1:nrow(stressor) )
{
  print(stressor[str,])
  
  ### Extracting stressor vectors ###
  
  HH <- colSums(F_hh[ stressor$index[str],])  # Household 
  Q <- colSums(F[ stressor$index[str],])  # Industrial stressor
  
  ### Testing global/UK sums ###
  
  # Total global GHG emissions in GWP 100
  sum(HH,Q)
  
  # Total UK emissions in GWP 100
  sum( Q[code$UK], HH[code$Y$index[code$Y$region == "GB"]] )
  
  
  ### Calculate direct emission intensities  and mulitpliers ###
  Q <- Q/x
  Q[is.na(Q)] <- 0
  Q[Q == Inf] <- 0
  
  MP <- L * Q
  
  ### Calculate PLD #1 of UK Carbon Footprint ###
  
  # Start calculation #1XPLD (flows embodied in intermediates layer 1+) --------------------
  
  
  result <- data.frame("index" = 1:(3*200*3*200*200),
                       "source.region" = rep(c("EU","GB","ROW"), each = 200),
                       "source.product" = code$Products$Name,
                       "source.product.group" = code$Groups$IPCC_group,
                       "intermediate.product.region" =  rep(c("EU","GB","ROW"), each = (200*3*200)),
                       "intermediate.product" =  rep(code$Products$Name, each = (200*3)),
                       "intermediate.product.group" =  rep(code$Groups$IPCC_group[1:200], each = (200*3)),
                       "destination.product" = rep( code$Products$Name, each = (3*200*200*3)),
                       "destination.product.group" = rep( code$Groups$IPCC_group,each = (3*200*200*3)),
                       "value" = 0,
                       "unit" = code$F$unit[stressor$index[str]],
                       "stressor" = stressor$name[str],
                       stringsAsFactors = FALSE)
  
  i <- 150
  for(i in 1:200)
  {
    print(i)
    print(code$Products$Name[i])
    
    z <- Z[, code$UK[i] ]
    FP <- t( t(MP) * z )
    
    FP <- Agg( x = FP, 
               aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
               dim = 2 )
    
    FP <- Agg( x = FP, 
               aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
               dim = 1 )
    
    sum(FP)
    
    result$value[ result$destination.product == code$Products$Name[i]] <- FP
    sum( result$value[result$destination.product == code$Products$Name[i]] ) 
    
  }
  
  # Remove zero entries
  data <- result[result$value != 0,]
  
  ### Export results in several formats ###
  
  save(data, file = paste0(path$output,stressor$name[str],"_XPLD_Source-Intermediates-Destination_UK_",year,".RData"))
  
  write.mat(data,paste0(path$output,stressor$name[str],"_XPLD_Source-Intermediates-Destination_UK_",year,".mat"))
  
  
  ### Select three example products from results and extract data ### 
  rea <- code$Products$Name[c(150,162,175)]
  
  export <- data[data$destination.product %in% rea,]
  unique(export$destination.product)
  
  write.table(export,
              file = paste0(path$output,stressor$name[str],"_XPLD_Source-Intermediate-Destination_UK_",year,"_Results for 3 destination products.txt"),
              sep = "§", 
              row.names = FALSE )
  
  
  
  ### Start calculation #2XPLD (classic PLD) ###
  
  reg <- 3
  ind <- 200
  lay_max <- 2
  lay <- lay_max + 2
  layers <- c("0","1","2","3+")
  
  # setup storage
  
  result <- data.frame("index" = 1:(lay*ind*ind*reg),
                       "source.region" = rep(c("EU","GB","ROW"),each = 200),
                       "source.product" = code$Products$Name,
                       "source.product.group" = code$Groups$IPCC_group,
                       "Scope" = code$Groups$scope,
                       "destination.product" = rep(code$Products$Name,each = (ind*reg)),
                       "destination.product.group" = rep(code$Groups$IPCC_group,each = (ind*reg)),
                       "layer" = rep(c("0","1","2","3+"),each = (ind*reg*ind)),
                       "value" = 0,
                       "unit" = code$F$unit[stressor$index[str]],
                       "stressor" = stressor$name[str],
                       "year" = year,
                       stringsAsFactors = FALSE)
  
  result$Scope[result$layer == "0"] <- "Scope 1"   # Layer 0 emissions are by definition scope 0
  
  A.sum <- FALSE
  
  # Layer 0:
  MP <- diag(49*200)*Q
  
  FP <- t( t(MP) * x )
  
  FP <- Agg(x = FP,
            aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
            dim = 2)
  
  FP <- Agg(x = FP,
            aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
            dim = 1)
  
  
  FP <- FP[, colnames(FP) %in%  paste0("GB","-",1:200) ]
  

  
  result$value[result$layer == "0"] <- FP
  
  
  # Layer 1:
  MP <- A*Q
  
  FP <- t( t(MP)*x )
  
  FP <- Agg(x = FP,
            aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
            dim = 2)
  
  FP <- Agg(x = FP,
            aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
            dim = 1)
  
  
  FP <- FP[, colnames(FP) %in%  paste0("GB","-",1:200) ]
  
  
  result$value[result$layer == "1"] <- FP
  
  B <- A
  A.sum <- diag(200*49)+A
  
  l <- 3
  for(l in 3:(lay_max+1) )
  {
    print(layers[l])
    B <- B%*%A
    A.sum <- A.sum + B
    
    MP <- B*Q
    
    FP <- t( t(MP) * x )
    
    FP <- Agg(x = FP,
              aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
              dim = 2)
    
    FP <- Agg(x = FP,
              aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
              dim = 1)
    
    
    FP <- FP[, colnames(FP) %in%  paste0("GB","-",1:200) ]
    
    result$value[result$layer == layers[l]] <- FP
  }
  
  
  L.rest <- L - A.sum
  
  
  MP <- L.rest*Q
  
  FP <- t( t(MP) * x )
  
  FP <- Agg(x = FP,
            aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
            dim = 2)
  
  FP <- Agg(x = FP,
            aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
            dim = 1)
  
  
  FP <- FP[, colnames(FP) %in%  paste0("GB","-",1:200) ]
  
  l <- l +1
  
  result$value[result$layer == layers[l]] <- FP
  
  sum(result$value)
  
  export <- result[result$value != 0,]
  
  
  write.table(export,
              file = paste0(path$output,stressor$name[str],"_XPLD_Source-Destination_UK_",year,".txt"),
              sep = "§",
              row.names = FALSE )
  
  
  ### Calculate Footprints of Production and Consumption ###
  
  result <- data.frame("index" = 1:(3*200*200),
                       "source.region" = rep( c("EU","GB","ROW"), each = 200),
                       "source.product" = code$Products$Name,
                       "source.product.group" = code$Groups$IPCC_group[1:200],
                       "destination/final.product" = rep( code$Products$Name, each = (ind*reg)),
                       "destination/final.group" = rep( code$Groups$IPCC_group[1:200],each = (ind*reg)),
                       "value_destination_production_X" = 0,
                       "value_final_consumption_Y" = 0,
                       "unit" = code$F$unit[stressor$index[str]],
                       "stressor" = stressor$name[str],
                       "year" = year,
                       stringsAsFactors = FALSE)
  
  FP <- t( t(L*Q)*x )
  
  FP <- Agg(x = FP,
            aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
            dim = 2)
  
  FP <- Agg(x = FP,
            aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
            dim = 1)
  
  
  FP <- FP[, colnames(FP) %in%  paste0("GB","-",1:200) ]
  sum(FP)
  
  result$value_destination_production_X <- c(FP)
  sum(result$value_destination_production_X)
  
  
  # Calculate Consumption Footprint
  
  FP <- t( t(L*Q)*Y )
  
  FP <- Agg(x = FP,
            aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
            dim = 2)
  
  FP <- Agg(x = FP,
            aggkey = paste0(rep(code$regionAgg$name,each = 200),"-",1:200),
            dim = 1)
  
  
  FP <- FP[, colnames(FP) %in%  paste0("GB","-",1:200) ]
  
  sum(FP)
  
  result$value_final_consumption_Y <- c(FP)
  sum(result$value_final_consumption_Y)
  
  write.table(result,
              file = paste0(path$output,stressor$name[str],"_Footprints of Production (X) and Consumption(Y)_UK_",year,".txt"),
              sep = "§",
              row.names = FALSE)
  
  
}
  
  
  
