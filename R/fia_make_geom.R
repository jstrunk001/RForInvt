#'@title
#'  convert xy coordinates to fia plots
#'
#'@description
#'  supply fia PLOT centers (x,y NOT lat,long) and generate SUBPLOT locations as a multipoint
#'  sf object assuming feet, although distances to subplots are provided as parameters. Simply
#'  set offx = c("2"=0,"3"=103.923,"4"=-103.923)*0.3048 and offy= c("2"=120,"3"=-60,"4"=-60)*0.3048
#'  respectively to obtain metric equivalen if the input coordinates are meters. If lat/long
#'  coordinates are desired, simply reproject from a projected system to lat/long after
#'  generating PLOT footprints
#'
#'  Declination: This function assumes the PLOT is perfectly north facing and
#'  does not account for declination.
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 09/09/2022 Upgraded to sf added more parameters for output PLOT   \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param data dataframe with id, x, y columns available
#'@param col_names a named vector with serves as a lookup a table for the input data - must use provided names: PLOT, x, y
#'@param create_polys TRUE / FALSE create polygons or just return theoretical SUBPLOT centers
#'@param  subplot_radius radius or side length  of SUBPLOT
#'@param  endCapStyle enable square or round plots
#'@param  offx  named vector to set x offsets for subplots 2-4 (must have names "2","3","4")
#'@param  offy  named vector to set y offsets for subplots 2-4 (must have names "2","3","4")
#'
#'@return
#'
#'a named list with possible elements output = c("SUBPLOT data.frame","SUBPLOT POINT","SUBPLOT POLYGON","PLOT POINT","PLOT POLYGON"):
#'
#'  "SUBPLOT data.frame" - a data.frame with records for each each PLOT and SUBPLOT
#'  "SUBPLOT POINT" - a point sf object for each PLOT and SUBPLOT
#'  "SUBPLOT POLYGON" - a polygon sf object for PLOT and SUBPLOT
#'  "PLOT POINT" - a multipoint sf object for each PLOT
#'  "PLOT POLYGON" - a multipolygon sf object for each PLOT
#'
#'
#'@examples
#'
#'    res= fia_make_geom(data.frame(PLOT=1:10, x=101:110*10000, y=101:110*10000),create_polys=T)
#'
#'@import dplyr plyr sf
#'
#'@export
#
#'@seealso \code{\link{bbox2polys}}\cr \code{\link{sf}}\cr \code{\link{st_buffer}}\cr

#Desired upgrades to this function:
# add declination
# enable supplying any SUBPLOT as an initial condition

# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ",readClipboard())),collapse="\n"))

fia_make_geom=function(
                      data
                    , col_names=c(PLOT="PLOT",x="x",y="y")
                    #, create_polys=T
                    #, combine_subplots=T
                    , output = c("SUBPLOT data.frame","SUBPLOT POINT","SUBPLOT POLYGON","PLOT POINT","PLOT POLYGON")
                    , subplot_radius=24
                    , endCapStyle="ROUND"
                    , offx = c("2"=0,"3"=103.923,"4"=-103.923)
                    , offy = c("2"=120,"3"=-60,"4"=-60)
                    ){


  # sqrt((120*cos(pi/6))^2+(120*sin(pi/6))^2)

  requireNamespace("plyr")
  requireNamespace("sf")
  requireNamespace("dplyr")

  col_names["SUBPLOT"]="SUBPLOT"
  data[,col_names["SUBPLOT"]]=1

  #test for output types
  do_geom = sum(c("SUBPLOT POINT","SUBPLOT POLYGON","PLOT POINT","PLOT POLYGON") %in% output) > 0
  do_combine_subplots = sum(c("PLOT POINT","PLOT POLYGON") %in% output) > 0

  #return object
  return_list = list()

  #create SUBPLOT coordinates manually from supplied offsets
    dat2=as.data.frame(data,drop=T)
    dat2[,col_names["SUBPLOT"]]=2
    dat2[,col_names["x"]]=dat2[,col_names["x"]] + offx["2"]
    dat2[,col_names["y"]]=dat2[,col_names["y"]] + offy["2"]

    dat3=as.data.frame(data,drop=T)
    dat3[,col_names["SUBPLOT"]]=3
    dat3[,col_names["x"]]=dat3[,col_names["x"]] + offx["3"]
    dat3[,col_names["y"]]=dat3[ , col_names["y"] ] + offy["3"]

    dat4=as.data.frame(data,drop=T)
    dat4[,col_names["SUBPLOT"]]=4
    dat4[,col_names["x"]]=dat4[,col_names["x"]] + offx["4"]
    dat4[,col_names["y"]]=dat4[,col_names["y"]] + offy["4"]

    df_all = plyr::rbind.fill(data,dat2,dat3,dat4)

  #add data.frame will all subplots to return object
    if("SUBPLOT data.frame" %in% output) return_list[["SUBPLOT data.frame"]] = df_all

  if(do_geom){

    #SUBPLOT geom needed for all subsequent steps
      sf_dat0 = sf::st_as_sf(df_all,coords=col_names[c("x","y")])

    #create / load sf SUBPLOT object
      if("SUBPLOT POINT" %in% output){
        return_list[["SUBPLOT POINT"]] = sf_dat0
      }

    #create SUBPLOT polygons
      if("SUBPLOT POLYGON" %in% output){
        sf_poly_in = sf::st_buffer(sf_dat0 , subplot_radius , endCapStyle = endCapStyle)
        return_list[["SUBPLOT POLYGON"]] = sf_poly_in
      }

    #create PLOT geometries from subplots
      if(do_combine_subplots){

        data_in = data
        data_in[,col_names[c("x","y")]] = NULL

        #group SUBPLOT points by PLOT and then merge geometrics - drop tibble
        sf_dat1 = dplyr::group_by_at(sf_dat0, .vars=col_names["PLOT"])
        sf_dat2 = as.data.frame(dplyr::summarise(sf_dat1,geometry = sf::st_union(geometry)))
        sf_dat3 = sf::st_as_sf(merge(data_in, sf_dat2, by = col_names["PLOT"]))


        if("PLOT POINT" %in% output) return_list[["PLOT POINT"]] = sf_dat3

        if("PLOT POLYGON" %in% output){
          #buffer by desired amount
          sf_dat4 = sf::st_buffer(sf::st_as_sf(sf_dat3) , subplot_radius , endCapStyle = endCapStyle)
          return_list[["PLOT POLYGON"]] = sf::st_as_sf(sf_dat4)
        }

      }

  }
  return(return_list)
}

if(F){

  df1 = data.frame(x=seq(0,1000,200), y=seq(0,1000,200))
  df1[,"PLOT"] = 1:nrow(df1)
  df1[,"test"] = nrow(df1):1
  res = fia_make_geom(df1)

  for(i in 1:length(res)){
    par( mar=c(5,5,5,5), xpd=T)
    plot(res[[i]],main=names(res)[i])
  }

  if(F){
    plot(res[["PLOT POINT"]])
    plot(res[["PLOT POLYGON"]])

    library(leaflet)
    l1=leaflet(data=res[1,],options=leafletOptions(crs=leafletCRS(proj4def="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")))
    addPolygons(l1,data=res[1:2,])
  }
}

