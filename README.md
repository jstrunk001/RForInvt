# RForInvt
A collection of forest inventory related tools.

`RForInvt` bundles tools for forest inventory data compilation and growth projection. The functionality
falls into a few groups:

- **`compile_*`** — compile plot- and tree-level inventory summaries (e.g. `compile_trees`, `compile_plots`).
- **`fia_*`** — an end-to-end FIA (Forest Inventory and Analysis) workflow: read a DataMart SQLite by
  evaluation (`fia_db`, `fia_evalid`, `fia_plots`, `fia_trees`), compile per-tree and per-acre attributes
  (`fia_compile_trees`, `fia_compile_plots`), produce post-stratified design-based estimates
  (`fia_estimate`, `fia_estimate_annual`, `fia_estimate_change`, `fia_components_long`), compare
  design- vs model-based annualized estimation (`fia_growth_model`, `fia_annualize_resid`,
  `fia_estimate_greg`, `fia_estimate_model`), and project with FVS (`fia_fvs_*`). Also clean and
  geolocate plot coordinates (`fia_clean_best_cds`, `fia_make_geom`). A runnable walkthrough on the
  bundled demo database is in `inst/examples/FIA_WWA_Example.Rmd`.
- **`fvs_*`** — build key files for and run the Forest Vegetation Simulator (`fvs_make_keyfiles`, `fvs_run`, ...).
- **`NVEL_*`** — National Volume Estimator Library wrappers for volume, bucking, merchandizing, biomass, and weight factors.
- **`model_archive_*` / `model_predict`** — save, list, load, and apply archived models. The package ships a
  registry (`extdata/models.csv`) of ~280 curated USFS biomass (NBEL, as `closed_form`) and volume (NVEL, as
  `nvel`) equations covering common PNW and Southeast species. See `inst/examples/model_archive_Example.Rmd`
  and `inst/docs/model_archive_spec.md`; rebuild with `data-raw/build_nbel_archive.R`.
- Statistical and utility helpers (`make_strata`, `archive_table`, `aggregate2`, ...).

This functionality is being split off of RSForInvt because of the dependency nightmare resulting from packing too much into a single package.

You can either download this repo as a zip and install from local .zip file in R, or use the devtools or remotes packages to install from github

# Install
Install the latest version of RForInvt from github with devtools. 

You may have to manually install a few packages that aren't brought in by the package for some reason. 

You will need the remotes (or devtools) package to install from github.

#Install with remotes
```r
install.packages("remotes")
remotes::install_github("jstrunk001/RForInvt")
```
#OR Install with devtools - my preference as I often use devtools
```r
install.packages("devtools")
devtools::install_github("jstrunk001/RForInvt")
```
#Manual way to install this package:

Download this git repository to a local zip file, then rename the downloaded zip archive from "RForinvt-master.zip" -> "RForInvt.zip", and then use the R remotes::install_local to install from .zip package file or just use the R or RStudio dropdown to install from a local zip. 

```r
install.packages("remotes")
#your path will vary here!
remotes::install_local("c:\\temp\\RForInvt.zip")

```

Package usage is also fairly simple

# RForInvt FIA estimation example

The `fia_*` family turns a raw FIA DataMart SQLite into design-based, EVALIDator-style estimates in
four steps: `fia_trees()`/`fia_plots()` -> `fia_compile_trees()` -> `fia_compile_plots()` -> `fia_estimate()`.
The example below runs on the small synthetic western-Washington database bundled with the package
(`FIADB_demo.db`); the code is identical on a real state download.

```r
  require(RForInvt)

  #open the bundled demo database and pick the most recent volume evaluation
    db <- fia_db(system.file("extdata", "FIADB_demo.db", package = "RForInvt"))
    ev <- fia_evalid(db, statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)

  #assemble -> compile -> estimate
    tr  <- fia_trees(db, evalid = ev)              # live trees with per-acre TPA_EXP
    pl  <- fia_plots(db, evalid = ev)              # conditions + post-stratification design
    trc <- fia_compile_trees(tr, vol_source = "fiadb")  # ba_ft, diameter class, vol_cf_net, ...
    plc <- fia_compile_plots(trc, pl)              # per-acre, nonforest plots zero-filled

  #net cubic-foot volume per forest acre, and total, by county
    fia_estimate(plc, vars = "vol_cf_net", by = "COUNTYCD", type = "per_acre")
    fia_estimate(plc, vars = "vol_cf_net", by = "COUNTYCD", type = "total")

  #change between two evaluations (paired remeasured plots)
    fia_estimate_change(db, statecd = 53, evalid_t1 = 531901, evalid_t2 = 532101,
                        mode = "remeas", vars = "vol_cf_net", by = "COUNTYCD", type = "per_acre")
```

See `inst/examples/FIA_WWA_Example.Rmd` for multi-year trends, species/diameter-class components, and
the design- vs model-based annualized-estimation comparison.

# RForInvt FVS example
``` 
  #load required packages
  
    require(RForInvt)
    require(parallel)
    require(RSQLite)

  #create cluster with 4 nodes  
  
    clus1=makeCluster(4)

  #in this case supply the path to your sqlite fvs database with tables FVS_StandInit, and FVS_TreeInit
  
    dir_fvs_in = system.file("extdata", "FIADB_RI.db", package = "RForInvt")

  #load FVS data - but limit to the first 50 plots
  # Note that in the FVS ready data, plots are treated as stands
  
    conSQL = DBI::dbConnect( RSQLite::SQLite() , dbname= dir_fvs_in )
      dbListTables(conSQL)
      fvs_stands = dbGetQuery( conSQL , "select * from FVS_STANDINIT_PLOT limit 50")
    dbDisconnect( conSQL )

  #assume a typical inventory dataset and prepare fvs parameters
  #the fvs_make_keyfiles will take this list of parameters and make a
  #separate key file for each record in the FVS_StandInit table
  
    df_params = fvs_prototype_params()
    df_params[1:nrow(fvs_stands),]=NA
    df_params[,"std_id"] = fvs_stands$STAND_ID
    df_params[,"invyr"] = fvs_stands$INV_YEAR
    df_params[,"timeint"] = 10
    df_params[,"numcycle"] = 1
    df_params[,"input_db"] = dir_fvs_in
    df_params[,"fvs_path"] = "C:/FVSbin/FVSne.exe"
    df_params[,"tree_table"] = "FVS_TREEINIT_PLOT"
    df_params[,"stand_table"] = "FVS_STANDINIT_PLOT"
    df_params
    
  #prepare prototype key file - the fvs_prototype_keyfile function just puts together a 
  #series of text strings. Some common keywords are suggested (e.g. notriple, nodgl, and dgstdev). 
  #If the parameter is NULL, then the keyword is not included, and other keywords can be supplied with "other_keywords" parameter
    
    #By default, notriple = NULL, so these two are equivalent
    key_proto = fvs_prototype_keyfile(notriple="NoTriple")
    key_proto = fvs_prototype_keyfile(other_keywords="NoTriple")
    
  #convert prototype key file into series of key files associated with each stand_id
  
    df_keys = fvs_make_keyfiles(df_params, key_proto = key_proto, cluster = clus1 , id="std_id")
  
  #make sure to look at a few keyword files to see that they are formulated correctly  (especially the db paths)
  
    df_keys
  
  #lastly, actually run fvs - if processing in parallel this function creates multiple output dbs, by default these are merged and temp files deleted
  
    fvs_run(df_keys, cluster = clus1, merge_dbs=T, db_merge="c:/temp/RForInvt/fVS_db_out_RI.db")
  
  #release resources assigned to cluster  
  
    parallel::stopCluster(clus1);rm(clus1)

```

# RForInvt NVEL example
``` 
        require(RForInvt)
        
        #grab list of species
        if(!"dfSpp" %in% ls()){
          library(RSQLite)
          db0 = dbConnect(RSQLite::SQLite(), system.file("NVEL/BiomassEqns.db", package="RForInvt"))
            dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
            dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
          dbDisconnect(db0)
        }

        #build a fake tree list
        if(!"df_fake" %in% ls()){
          set.seed=111
          nfake=length(unique(dfCoeff$species_code))

          df_fake = data.frame(
            trid=1:(nfake)
            ,region = 6
            ,forest = "01"
            ,district = "01"
            ,dbh=10*abs(rnorm(nfake))
            ,ht=100*abs(rnorm(nfake))
            ,spcd = unique(dfCoeff$species_code)#'     sample(c("a","b","c","d") , nfake , T)
          )

        }

        #get volumes
        NVEL_volume( dfTL = df_fake )
        
```

