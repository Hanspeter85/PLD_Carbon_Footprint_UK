#####################################################################################
# Leverage supply chains in the carbon footprints of production and consumption in UK
#####################################################################################

library(tidyverse)
library(openxlsx)
#install.packages('expm')
#library(expm)
#library(readxl)
# install.packages('R.matlab')
# library(R.matlab)

# Year of analysis
year <- 2019   

# Set paths 
path <- list("EXIO" = paste0("W:/WU/Projekte/GRU/04_Daten/MRIO/EXIOBASE/EXIOBASE 3.8/IOT_pxp/","IOT_",year,"_pxp/") ,
             "input" = paste0( getwd(),"/input/"),
             "output" = paste0( getwd(),"/output/") )


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

code[["region"]] <- unique(code$Z$region)

code[["Y"]] <- data.frame("index" = 1:ncol(Y),
                          code$FinalDemand[,-1],
                          "region" = rep(code$region, each = 7),
                          stringsAsFactors = FALSE )

# Re-define regions for aggregation
code[["regionAgg"]] <- code$region

code$regionAgg[29:49] <- 'ROW'
code$regionAgg[1:27] <- 'EU'

#IO.codes$Region.Code[IO.codes$Region.Code != 'EU'] <- 'ROW'

reg <- 3
ind <- 200
lay <- 6+2
layers <- c("0","1","2","3","4","5","6","7+")

# Read index of UK
code[["UK"]] <- code$Z$index[ code$Z$region == "GB"]
  
  
# setup store -------------------------------------------------------------

result <- data.frame("index" = 1:(lay*ind*ind*reg),
                     "from.region" = rep(c("EU","GB","ROW"),each = 200),
                     "from.product" = products$product,
                     "from.product.group" = IO.codes$IPCC.groups[1:200],
                     "Scope" = IO.codes$Scope[1:200],
                     "final.product" = rep(products$product,each = (ind*reg)),
                     "final.product.group" = rep(IO.codes$IPCC.groups[1:200],each = (ind*reg)),
                     "layer" = rep(c("0","1","2","3","4","5","6","7+"),each = (ind*reg*ind)),
                     "value" = 0,
                     stringsAsFactors = FALSE)

result$Scope[result$layer == "0"] <- "Scope 1"

# Calculate PLD #1 of UK Carbon Footprint ------------------------------------

year <- 2011

cat(paste("\n\n", year, "processing ...\n"))
# load parsed data
get_MRIO_data(version = "EXIOBASE 3.6", year = year, type = type)
load(file = paste0(parsedpath,type,"/",year,"_A.RData"))
load(file = paste0(parsedpath,type,"/",year,"_F_hh.RData"))

# colnames(Y) <- Y.codes$`Region Name`
# Y <- agg(Y)
# Y <- Y[,regions$index[regions$regions == 'GB']]

HH <- colSums(F_hh*Q.codes$GWP)/1000000
Q <- colSums(E * Q.codes$GWP)/1000000

sum(Q[UK])+sum(HH[Y.codes$index[Y.codes$Region.Name == "GB"]])

# Function to scale each column to one unit of output (x)

star <- diag(L)

starring <- function(X)
{
  G <- t(t(X)/star)
  G[is.na(G)] <- 0
  G[G==Inf] <- 0
  
  return(G)
}

L.star <- starring(L)

Q <- Q/x
Q[is.na(Q)] <- 0
Q[Q == Inf] <- 0


# Start calculation #1XPLD (flows embodied in intermediates layer 1+) --------------------

Z <- A%*%diag(x)

IO.codes[UK[i],]

result <- data.frame("index" = 1:(3*200*3*200*200),
                     "from.region" = rep(c("EU","GB","ROW"),each = 200),
                     "from.product" = products$product,
                     "from.product.group" = IO.codes$IPCC.groups[1:200],
                     "intermediate.product.region" =  rep(c("EU","GB","ROW"),each = (200*3*200)),
                     "intermediate.product" =  rep(products$product,each = (200*3)),
                     "intermediate.product.group" =  rep(IO.codes$IPCC.groups[1:200],each = (200*3)),
                     "final.product" = rep(products$product,each = (3*200*200*3)),
                     "final.product.group" = rep(IO.codes$IPCC.groups[1:200],each = (3*200*200*3)),
                     "value" = 0,
                     stringsAsFactors = FALSE)

i <- 10
for(i in 1:200)
{
  print(products$product[i])
  
  z <- Z[,UK[i]]
  FP <- t(L*Q) * z
  
  colnames(FP) <- paste0(rep(regions$regions,each = 200),"-",1:200) 
  FP <- agg(FP)
  FP <- t(FP)
  colnames(FP) <- paste0(rep(regions$regions,each = 200),"-",1:200) 
  FP <- agg(FP)
  sum(FP)
  
  result$value[result$final.product == products$product[i]] <- FP
  sum(result$value[result$final.product == products$product[i]])
  
}


data <- result[result$value != 0,]
resultpath <- "//SHARES/data/Users/hwieland/Docs/Documents/Projekte/PLD Coop/"
save(data, file = paste0(resultpath,"Source-Intermediates-Products.RData"))
#load(file = paste0(resultpath,"Source-Intermediates-Products.RData"))

# install.packages('rmatio')
# library(rmatio)
# 
# write.mat(data,paste0(resultpath,"Source-Intermediates-Products.mat"))

rea <- IO.codes$Product.Name[c(150,162,175)]

export <- data[data$final.product %in% rea,]
unique(export$final.product)

write.table(export,file = paste0(resultpath,"XPLD UK 2011 pxp_analyzing intermediate inputs of 3 final products.txt"),sep = "§")




# Start calculation #2XPLD (classic PLD) ----------------------------------------

A.sum <- FALSE

# Layer 0:
MP <- diag(49*200)*Q
FP <- t(MP)*x
colnames(FP) <- paste0(rep(regions$regions,each = 200),"-",1:200) 
FP <- agg(FP)
FP <- t(FP)
FP <- FP[,UK]

# colnames(FP) <- rep(1:163,49) 
# FP <- agg(FP)
result$value[result$layer == "0"] <- FP


# Layer 1:
MP <- A*Q
#diag(MP) <- 0

FP <- t(MP)*x
colnames(FP) <- paste0(rep(regions$regions,each = 200),"-",1:200) 
FP <- agg(FP)
FP <- t(FP)

# colnames(FP) <- rep(1:163,49) 
# FP <- agg(FP)
FP <- FP[,UK]
result$value[result$layer == "1"] <- FP

B <- A
A.sum <- diag(200*49)+A

l <- 3
for(l in 3:7)
{
  print(layers[l])
  B <- B%*%A
  A.sum <- A.sum + B
  
  MP <- B*Q
  
  #diag(MP) <- 0
  FP <- t(MP)*x
  colnames(FP) <- paste0(rep(regions$regions,each = 200),"-",1:200)  
  FP <- agg(FP)
  FP <- t(FP)

  FP <- FP[,UK]
  # colnames(FP) <- rep(1:163,49) 
  # FP <- agg(FP)
  
  result$value[result$layer == layers[l]] <- FP
}


L.rest <- L - A.sum


MP <- L.rest*Q

#diag(MP) <- 0
FP <- t(MP)*x
colnames(FP) <- paste0(rep(regions$regions,each = 200),"-",1:200) 
FP <- agg(FP)
FP <- t(FP)

# colnames(FP) <- rep(1:163,49) 
# FP <- agg(FP)
FP <- FP[,UK]
l <- l +1
result$value[result$layer == layers[l]] <- FP

sum(result$value)

export <- result[result$value != 0,]
resultpath <- "//SHARES/data/Users/hwieland/Docs/Documents/Projekte/PLD Coop/"
write.table(export,file = paste0(resultpath,"XPLD UK 2011 pxp.txt"),sep = "§")


# Calculate Carbon Footprint ------------------------------------

FP <- t(L*Q)*x
colnames(FP) <- paste0(rep(regions$regions,each = 200),"-",1:200) 
FP <- agg(FP)
FP <- t(FP)

# colnames(FP) <- rep(1:163,49) 
# FP <- agg(FP)
FP <- FP[,UK]
sum(FP)

result <- data.frame("index" = 1:(3*200*200),
                     "from.region" = rep(c("EU","GB","ROW"),each = 200),
                     "from.product" = products$product,
                     "from.product.group" = IO.codes$IPCC.groups[1:200],
                     "final.product" = rep(products$product,each = (ind*reg)),
                     "final.product.group" = rep(IO.codes$IPCC.groups[1:200],each = (ind*reg)),
                     "value" = 0,
                     stringsAsFactors = FALSE)

result$value <- c(FP)
sum(result$value)
export <- result[result$value != 0,]
write.table(export,file = paste0(resultpath,"Gross Production Carbon Footprint UK 2011 pxp.txt"),sep = "§")







