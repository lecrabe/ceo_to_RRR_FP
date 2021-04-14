######################################## Structure des fichiers et fichiers des codes et trajectoires
options(stringsAsFactors = F)
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(dplyr)
packages(tidyr)
packages(readr)
packages(rgdal)
packages(raster)


scr_dir <- paste0(ges_dir,"scripts/")
inp_dir <- paste0(ges_dir,"inputs/")
out_dir <- paste0(ges_dir,"output/")

ceo_dir <- paste0(inp_dir,"sortie_ceo/")

lapply(ls(pattern = "_dir"),function(x){dir.create(get(x),showWarnings = F)})


######################################## Fonction "AREA"
area    <- function(df){
  nrow(df)/ taille_db * superficie
}



######################################## Fonction "STABLE"
stable  <- function(categorie){
  df[df$osc_2020 == categorie & df$osc_1990 == categorie,]
}

