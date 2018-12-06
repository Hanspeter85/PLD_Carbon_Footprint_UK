#########################################################
# XPLD Carbon Footprint of UK 
#########################################################

library(tidyverse)
install.packages('expm')
library(expm)
library(readxl)
# install.packages('R.matlab')
# library(R.matlab)

years <- 2011
type <- "pxp"
parsedpath <- "W:/WU/Projekte/SRU-Projekte/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/"

# load functions ----------------------------------------------------------

sources_files <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sources_files <- sapply(X = sources_files, FUN = source, local = .GlobalEnv)

# get metadata ------------------------------------------------------------

load(file.path(parsedpath,"Q.codes.RData"))
load(file.path(parsedpath,"Y.codes.RData"))
load(file.path(parsedpath, type, "IO.codes.RData"))

scope <- c(rep("Scope 3",127),rep("Scope 2",14),rep("Scope 3",59))

Product.group <- read_excel("//shares/data/Users/hwieland/Docs/Documents/Projekte/PLD Coop/Product group classification according to IPCC.xlsx", )
colnames(Product.group)[1] <- "Products"


IO.codes <- data.frame(IO.codes,
                  "Scope" = rep(scope,49),
                  "IPCC.groups" = Product.group$Group,
                  stringsAsFactors = FALSE)

Y.codes <- data.frame(Y.codes,
                      "index" = 1:nrow(Y.codes),
                      stringsAsFactors = FALSE)

# define variables --------------------------------------------------------

regions <- data.frame("index" = 1:49, 
                      "regions" = unique(IO.codes$Country.Code),
                      stringsAsFactors = FALSE)

regions$regions[29:49] <- 'ROW'
regions$regions[1:27] <- 'EU'

#IO.codes$Region.Code[IO.codes$Region.Code != 'EU'] <- 'ROW'

products <- data.frame("index" = 1:200, "product" = IO.codes$Product.Name[1:200])
reg <- 3
ind <- 200
lay <- 6+2
layers <- c("0","1","2","3","4","5","6","7+")

# Read index of UK
UK <- IO.codes$Index[IO.codes$Country.Code == "GB"]

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







