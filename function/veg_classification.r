require(dplyr)
require(stringr)
require(raster)
require(rgdal)
require(rgeos)
require(randomForest)

##Inputs to start are inputs to raster training tile (singular, will eventually make this to folder and create a )

## Consider having an option to write the fits to a file. 

## Read all file names
single_random_forest_loop <- function( tile_name, folder_of_tiles, file_path_to_shape){
  
  
  ## read raster make ndvi layer and add it to the brick.
  raster<-brick(paste0(paste(folder_of_tiles, tile_name ,sep="/"), ".tif" ))
  names(raster)<-c("b1", "b2", "b3", "b4") ## Rename Bands for ease of use
  ndvi<-overlay(raster$b4, raster$b3, fun=function(x,y){(x-y)/(x+y)})
  cov<-addLayer(raster,ndvi) ## Merge NDVI and the tile imagery
  names(cov)<-c("b1", "b2", "b3", "b4", "ndvi")
  
  ## parse and read shapefile
  temp<-stringr::str_split(file_path_to_shape, "/")
  temp2<-readOGR(dsn=paste(head(temp[[1]], -1) , collapse = "/"), layer=paste(tail(temp[[1]], 1)))
  temp2@data$code<-as.numeric(temp2@data$Class)
  shape<-crop(temp2, extent(cov))%>%
    rasterize(ndvi, field="code")  ## Change this when you make the final function
  names(shape)<-"class"
  
  rm(ndvi, temp, temp2, raster)
  
  data_to_be_modeled<-crop(cov, extent(shape))%>%
    mask(shape)%>%
    addLayer(shape)%>%
    getValues()%>%
    as.data.frame()%>%
    filter(!is.na(class))
  
  fit<-randomForest(as.factor(class)~.,
                    data=data_to_be_modeled, 
                    importance=TRUE,
                    ntree=200
  )
  
  rm(data_to_be_modeled, cov, shape)
  
  return(fit)
  
  rm(fit)
  
  
  
}




## Looping trough tiles and apply the single random forest loop to each tile.  

build_model<-function(folder_of_tiles, file_path_to_shape){
  t<-base::list.files(folder_of_tiles)%>%
    tools::file_path_sans_ext()%>%
    tools::file_path_sans_ext()%>%
    base::unique() ## May want to change this to just a vector and use this for just apply
  
  t2<-folder_of_tiles
  t3<-file_path_to_shape
  
    lapply(t, single_random_forest_loop, t2, t3)
}

start_time <- Sys.time()

set.seed(420)

test_model<-build_model("test_tiles", "test_shape/training_polygons12N")

end_time <- Sys.time()

end_time - start_time
