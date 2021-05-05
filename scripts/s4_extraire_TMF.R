######################################## Spatialisation des points
spdf <- SpatialPointsDataFrame(df[,c("lon","lat")],
                               df[,c("lon","lat","plot_id","sample_id")],
                               proj4string=CRS("+init=epsg:4326"))
plot(spdf)

######################################## EXTRACTION DES DONNEES TMF
tmf <- brick(paste0(inp_dir,"/tmf_CHG_COG.tif"))

tmp_tmf <- data.frame(raster::extract(tmf,spdf))
names(tmp_tmf) <- paste0("code_",c(1990,2000,2010,2019))

tmp_tmf$sort_id <- row(tmp_tmf)[,1]

tmf_codes <- data.frame(cbind(
  1:6,
  c("TMF",   #Undisturbed tropical moist forest
    "DEG",   #Degraded tropical moist forest
    "DEF",   #Deforested land
    "REG",   #Tropical moist forest regrowth
    "WAT",   #Permanent and seasonal water
    "OLC"    #Other land cover
  )
)
)
names(tmf_codes) <- c("code","label")



d1 <- merge(tmp_tmf,tmf_codes,by.x="code_1990",by.y="code",all.x=T)
names(d1)[ncol(d1)] <- "label_1990"

d1 <- merge(d1,tmf_codes,by.x="code_2000",by.y="code",all.x=T)
names(d1)[ncol(d1)] <- "label_2000"

d1 <- merge(d1,tmf_codes,by.x="code_2010",by.y="code",all.x=T)
names(d1)[ncol(d1)] <- "label_2010"

d1 <- merge(d1,tmf_codes,by.x="code_2019",by.y="code",all.x=T)
names(d1)[ncol(d1)] <- "label_2019"

d1 <- arrange(d1,sort_id)
head(d1)
df[,paste0("TMF_",c(1990,2000,2010,2019))] <- d1[,paste0("label_",c(1990,2000,2010,2019))]


table(df$osc_1990,df$TMF_1990)

print(table(df$TMF_1990,df$TMF_2019))
print(table(df$osc_1990,df$osc_2019))
