#'@title
#'  reformats current "best_coordinates" FIA plot coordinate table to be long format and adds optional geometry fields
#'
#'@description
#'  reformats current "best_coordinates" FIA plot coordinate table to be long format and adds optional geometry fields
#'
#'@details
#'
#'  reformats current "best_coordinates" FIA plot coordinate table to be long format and adds optional geometry fields
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 10/2/2024 Created \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <some.body@@somewhere.com>
#'
#'@param df_best data.frame with coordinates from PNW NIMS view ~coords_best
#'@param id_nms unique identifiers that will be attached to each coordinate
#@param independent_steps vector of steps to implement independently
#'@param sequential_steps  these steps are sequential, and dropping one step will invalidate later steps
#'@param geom_type  if geometry is desired, what type: points and/or polygons and subplot and/or plot multipart objects
#'@param filters vector of string filters used to subset input set of coordinates, see dplyr::filter
#'@param ply_radius what radius should be used around the subplot centers when creating polygons, should be in units of crs_out
#'@param crs_in what is the projection of input coordinates - the coords_best table (as of 10/08/2024) has NAD83 2011 LAT/LON (doesn't have to be named vector)
#'@param crs_out what projection should the output geometry be in (doesn't have to be named vector)
#'@param nms_cds a named list with two elements where each element is a named vector of coordinate columns. The output coordinates are placed in columns with the list names, see defaults for example
#'
#'@return
#'  a list with up to 5 components:
#'
#'    1. long format SUBPLOT coordinate data
#'    2. long format suSUBPLOT coordinate data with point geometry (optionally transformed to crs_out)
#'    3. long format PLOT coordinate data with multipoint geometry (optionally transformed to crs_out)
#'    4. long format SUBPLOT coordinate data with polygon geometry (MUST be transformed to crs_out)
#'    5. long format PLOT coordinate data with multipolygon geometry (MUST be transformed to crs_out)
#'
#'@examples
#'
#' res1 = fia_clean_best_cds(cds_5k)
#'
#'
#@import data.table dplyr
#'
#'@export
#
#'@seealso \code{\link{dplyr::filter}}\cr \code{\link{data.table::melt}}\cr

#the code below with a single comment symbol is not part of the official roxygen code
#
#Desired upgrades to this function (keep track of planned updates):
#1.
#2.
#3.
#4.
#

#Help to set up param documentation above
# 1. Select function parameters, in this example case just "x,y,...", arguments should be on separate line
# 2. select the code starting with writeClipboard(...) below without comment (hash symbol) and run
# 3. paste parameters into @param section above
#
# writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))


#rename function and add guts
fia_clean_best_cds = function(
                    df_best
                    ,id_nms = c('INV_PLOT_ID','STATECD','REGION','COUNTYCD','PLOT','PLOT_PUBLIC',
                      'NFS_PLOT_NUMBER','COORD_TYPE_NAME','PC_COORD_METHOD_NAME','PC_SRID')
                    # ,independent_steps = c(
                    #   "filter_cds"
                    # )
                    ,sequential_steps = c(
                      "remove_duplicates"
                      ,"filter_cds"
                      ,"cds_wide_to_long" #required
                      ,"reproject_crs_out"
                      ,"create_geometry"
                      )
                    ,geom_type = c("SUBPLOT POINT","SUBPLOT POLYGON","PLOT POINT","PLOT POLYGON")
                    ,filters = c("STATECD==41","COORD_TYPE_NAME=='SP1'","PC_COORD_METHOD_NAME == 'HPGPS'")
                    ,ply_radius = c(m=24*0.3048,ft=24)[1]
                    ,crs_in = c(latlon="EPSG:6318")
                    ,crs_out = c(utm10 = "EPSG:6339")
                    ,nms_cds = list(
                        LON_X=paste(paste0("SP",1:4),c("LON_X"),sep="_"),
                        LAT_Y=paste(paste0("SP",1:4),c("LAT_Y"),sep="_")
                        )
                    ,...
                    ){

  df_best_in = df_best

  #check arguments

  if("filter_pts" %in% sequential_steps){

    warning("filter_pts argument has not been tested - test me!!")
    filters_in = paste(filters,collapse=" & ")
    str_call = paste("dplyr::filter(df_best_in,",filters_in,")")
    df_best_in = eval(parse(text=str_call))

  }

  if("remove_duplicates" %in% sequential_steps ){

    #prioritize HPGNSS
    df_HPGNSS = df_best_in[df_best_in$PC_COORD_METHOD_NAME == "HPGPS",]
    df_HPGNSS = df_HPGNSS[!duplicated(df_HPGNSS$PLOT),]
    #second is GNSS
    df_GNSS = df_best_in[df_best_in$PC_COORD_METHOD_NAME == "GPS",]
    df_GNSS = df_GNSS[!df_GNSS$PLOT %in% df_HPGNSS$PLOT, ]
    df_GNSS = df_GNSS[!duplicated(df_GNSS$PLOT),]

    #last is the remainder
    df_non_GNSS = df_best_in[(!df_best_in$PLOT %in% c(df_GNSS$PLOT,df_HPGNSS$PLOT)) & !duplicated(df_best_in$PLOT),]
    #plot(PC_LAT_Y~PC_LON_X, data = df_non_GNSS)

    #compile in hierarchy without duplicates
    df_best_in = rbind(df_HPGNSS,df_GNSS, df_non_GNSS)


  }


  #initiate results
  res_in = list()

  if("cds_wide_to_long" %in% sequential_steps | TRUE){ #must do this

    #manually take wide coordinates to long and rename coordinates into single column
    df_best_in_sp1 = data.frame(unqid = 1:nrow(df_best_in),df_best_in[,id_nms], SUBPLOT = 1, LAT_Y = df_best_in$PC_LAT_Y, LON_X = df_best_in$PC_LON_X )
    df_best_in_sp2 = data.frame(unqid = 1:nrow(df_best_in),df_best_in[,id_nms], SUBPLOT = 2, LAT_Y = df_best_in$SP2_LAT_Y, LON_X = df_best_in$SP2_LON_X )
    df_best_in_sp3 = data.frame(unqid = 1:nrow(df_best_in),df_best_in[,id_nms], SUBPLOT = 3, LAT_Y = df_best_in$SP3_LAT_Y, LON_X = df_best_in$SP3_LON_X )
    df_best_in_sp4 = data.frame(unqid = 1:nrow(df_best_in),df_best_in[,id_nms], SUBPLOT = 4, LAT_Y = df_best_in$SP4_LAT_Y, LON_X = df_best_in$SP4_LON_X )

    #bind together columns
    df_best_in_long = do.call(rbind,list(df_best_in_sp1,df_best_in_sp2,df_best_in_sp3,df_best_in_sp4))

    #drop NA coords?
    #df_best_in_long = df_best_in_long[!is.na(df_best_in_long$LAT_Y) & !is.na(df_best_in_long$LON_X),]

    #load long format data
    res_in[["SUBPLOT data.frame"]] = df_best_in_long

  }


  if("create_geometry" %in% sequential_steps){

    #filter non-NA coordinates
      df_best_in_long_ok = df_best_in_long[!is.na(df_best_in_long[, "LAT_Y"]) & !is.na(df_best_in_long[,"LON_X"]),]

    #create subplot objects
      sf_pt_in = sf::st_as_sf( df_best_in_long_ok , coords = c( "LON_X", "LAT_Y"), crs = crs_in)

    #reproject
    if("reproject_crs_out" %in% sequential_steps){

      sf_pt_in = sf::st_transform(sf_pt_in, crs = crs_out)
      sf_pt_in[,c("X","Y")] = sf::st_coordinates(sf_pt_in)

    }

    #load subplot points
    if("SUBPLOT POINT" %in% geom_type) res_in[["SUBPLOT POINT"]] = sf_pt_in

    #buffer points
    if("SUBPLOT POLYGON" %in% geom_type){

      sp_ply_in = sf::st_buffer(sf_pt_in, dist=ply_radius)
      res_in[["SUBPLOT POLYGON"]] = sp_ply_in

    }


    if("PLOT POINT" %in% geom_type | "PLOT POLYGON" %in% geom_type){

      spl_id_in = split(sf_pt_in ,sf_pt_in[,"unqid",drop=T], drop=T)
      spl_combine = lapply(spl_id_in , .fn_combine, crs_out)
      pl_mult_in = sf::st_as_sf( plyr::rbind.fill(spl_combine))

      res_in[["PLOT POINT"]] = pl_mult_in
      #plot(sf::st_geometry(pl_mult_in[1:nrow(pl_mult_in),]))
    }

    #buffer points
    if("PLOT POLYGON" %in% geom_type){

      pl_mult_in_ply = sf::st_buffer(pl_mult_in, dist=ply_radius)
      res_in[["PLOT Polygon"]] = pl_mult_in_ply

    }

  }#end create geom


  res_in
}#end function

.fn_combine=function(x,crs){
  if(nrow(x)>1){
    geom_i = sf::st_combine(sf::st_as_sf(x,crs=crs))
    x_in = x[1,,drop=F]
    sf::st_geometry(x_in) = geom_i
  }
  if(nrow(x)==1){
    x_in = st_cast(x, "MULTIPOINT")
  }
  x_in
}

if(F){

  #test data
    if(!"cds_all" %in% ls() ){
      cds_all = readRDS(path_cds_today_rds)
      cds_5k = cds_all[sample(nrow(cds_all), 5000),]
    }

  res1 = fia_clean_best_cds(cds_5k)
}
