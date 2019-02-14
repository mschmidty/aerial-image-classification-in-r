require(dplyr)
require(stringr)
require(raster)
require(rgdal)
require(rgeos)
require(randomForest)



## Function to work subset, merge, and make ndvi layer from each tile to make a training dataset. 
subset_training_data_from_tile<- function( tile_name, folder_of_tiles, file_path_to_shape){
  
  
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
  
  saveRDS(data_to_be_modeled, paste0("training_datasets/", tile_name, "training_data.rds"))
  
  rm(data_to_be_modeled)
}

  
## Run random forest on combined training dataset
# - reads the folder with tiles
# -- takes all file names and stores them in a list to be transfered to the `subset_training_data_from_tile` function which
# - reads the shapefile
# -- adds the shapefile to `subset_training_data_from_tile`
# - tile names are looped over and sent to `subset_training_data_from_tile` individually with the shapefile. 
# - all individually returned tile training datasets are combined together with rbind.fill
# - combined dataset, is run through random forests. 
build_model<-function(folder_of_tiles, file_path_to_shape){
  t<-base::list.files(folder_of_tiles)%>%
    tools::file_path_sans_ext()%>%
    tools::file_path_sans_ext()%>%
    base::unique() ## May want to change this to just a vector and use this for just apply
  
  t2<-folder_of_tiles
  t3<-file_path_to_shape
  
  lapply(t, subset_training_data_from_tile, t2, t3)
  
  t4<-base::list.files("training_datasets")
  
  t5<-lapply(t4, function(x)readRDS(paste0("training_datasets/", x)))
  t6 <- do.call(plyr::rbind.fill, t5)
  
  head(t6)
  
  rm(t5)
  
  fit<-randomForest(as.factor(class)~.,
                    data=t6,
                    importance=TRUE,
                    ntree=500, norm.votes = FALSE
  )
  rm(t6)
  return(fit)
}


## fun the function `build_model`
set.seed(420)
start_time <- Sys.time()

test_model<-build_model("test_tiles", "test_shape/training_polygons12N_12102018")

end_time <- Sys.time()

end_time - start_time


