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
#'@param df_best
#'@param id_nms unique identifiers that will be attached to each coordinate
#'@param independent_steps vector of steps to implement independently
#'@param sequential_steps  these steps are sequential, and dropping one step will invalidate later steps
#'@param geom_type  if geometry is desired, what type: points and/or polygons and subplot and/or plot multipart objects
#'@param filters vector of string filters used to subset input set of coordinates, see dplyr::filter
#'@param ply_radius what radius should be used around the subplot centers when creating polygons, should be in units of crs_out
#'@param crs_in what is the projection of input coordinates - coords best has NAD83 2011 LAT/LON
#'@param crs_out what projection should the geometry be in
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
                    ,independent_steps = c(
                      "filter_cds"
                      ,"reproject_crs_out"
                    )
                    ,sequential_steps = c(
                      "rename_PC_SP1" #the best table uses PC_ for SP1, and then SP2, SP3, SP4
                      ,"cds_wide_to_long" #required
                      ,"create_geom"
                      )
                    ,geom_type = c("plot_multipoint","subplot_point","subplot_poly","plot_multipart_poly")
                    ,filters = c("STATECD==41","COORD_TYPE_NAME=='SP1'","PC_COORD_METHOD_NAME == 'HPGPS'")
                    ,ply_radius = 24*0.3048
                    ,crs_in = "EPSG:6318"
                    ,crs_out = "EPSG:6339"
                    ,nms_cds = list(
                        LON_X=paste(paste0("SP",1:4),c("LON_X"),sep="_"),
                        LAT_Y=paste(paste0("SP",1:4),c("LAT_Y"),sep="_")
                        )
                    ,...
                    ){


  df_best_in = df_best

  #check arguments

  if("filter_pts" %in% independent_steps){

    warning("filter_pts argument has not been tested - test me!!")
    filters_in = paste(filters,collapse=" & ")
    str_call = paste("dplyr::filter(df_best_in,",filters_in,")")
    df_best_in = eval(parse(text=str_call))

  }


  if("rename_PC_SP1" %in% sequential_steps){

    df_best_in[,c("SP1_LON_X","SP1_LAT_Y")] = df_best_in[,c("PC_LON_X","PC_LAT_Y")]

  }

  if("cds_wide_to_long" %in% sequential_steps | TRUE){ #must do this

    #melt
    df_best_in_long =
      as.data.frame(
        data.table::melt(
          data = data.table::as.data.table(df_best_in)
          ,id.vars = id_nms#, variable.name=c("variable")
          ,measure = nms_cds
          ,variable.name = "SUBPLOT"
        )
      )

  }

  res_in = list(cds_clean = df_best_in_long)

  if("create_geom" %in% sequential_steps){

    #filter non-NA coordinates
      df_best_in_long_ok = df_best_in_long[!is.na(df_best_in_long[, "LAT_Y"]) & !is.na(df_best_in_long[,"LON_X"]),]

    #create subplot objects
      sp_pt_in = sf::st_as_sf( df_best_in_long_ok , coords = c( "LON_X", "LAT_Y"), crs = crs_in)


    #reproject
    if("reproject_crs_out" %in% independent_steps){

      sp_pt_in = sf::st_transform(sp_pt_in, crs = crs_out)

    }

    #load subplot points
    res_in[["df_subplot_point"]] = sp_pt_in

    #buffer points
    if("subplot_poly" %in% independent_steps){

      sp_ply_in = sf::st_buffer(sp_pt_in, dist=ply_radius)
      res_in[["df_subplot_polygon"]] = sp_ply_in

    }


    if("plot_multipoint" %in% geom_type | "plot_multipart_poly" %in% geom_type){

      spl_id_in = split(as.data.frame(sp_pt_in) ,sp_pt_in[,unlist(id_nms),drop=T], drop=T)
      spl_combine = lapply(spl_id_in , .fn_combine, crs_out)
      pl_mult_in = do.call(rbind, spl_combine)
      row.names(pl_mult_in) = NULL

      res_in[["df_plot_multipoint"]] = pl_mult_in

    }

    #buffer points
    if("plot_multipart_poly" %in% independent_steps){

      pl_mult_in_ply = sf::st_buffer(pl_mult_in, dist=ply_radius)
      res_in[["df_plot_multipolygon"]] = pl_mult_in_ply

    }

  }#end create geom


  res_in
}#end function

.fn_combine=function(x,crs){
  geom_i = sf::st_combine(sf::st_as_sf(x,crs=crs))
  x_in = x[1,,drop=F]
  sf::st_geometry(x_in) = geom_i
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
