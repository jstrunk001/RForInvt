# RForInvt
A collection of forest inventory related tools

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

Download this git repository to a local zip file, then rename the downloaded zip archive from "RForinvt-master.zip" -> "RForInvt.zip", and then use the R remotes::install_local to install from .zip package file or just use the r or RStudio dropdown to inall a local zip. 

```r
install.packages("remotes)
#your path will vary here!
remotes::install_local("c:\\temp\\RForInvt.zip")

```

Package usage is also fairly simple

# RForBio FVS example
``` 
  #load required packages
    require(RForInvt)
    require(parallel)
    require(RSQLite)

  #in this case supply the path to your sqlite fvs database with tables FVS_StandInit, and FVS_TreeInit
    dir_fvs_in = "c:/temp/fvs_test/fVS_db_in.db"
    dir_fvs_out =  "c:/temp/fvs_test/fVS_db_out.db"
    
  #create cluster with 4 nodes  
    clus1=makeCluster(4)
 
  #load FVS data
    conSQL = DBI::dbConnect( RSQLite::SQLite() , dbname= dir_fvs_in )
      dbListTables(conSQL)
      fvs_stands = dbGetQuery( conSQL , "select * from FVS_StandInit")
    dbDisconnect( conSQL )

  #assume a typical inventory dataset and prepare fvs parameters
  #the fvs_make_keyfiles will take this list of parameters and make a
  #separate key file for each record in the FVS_StandInit table
    df_params = fvs_protype_params()
    df_params[1:nrow(stand_data),]=NA
    df_params[,"stand_id"] = fvs_stands$stand_id
    df_params[,"invyr"] = fvs_stands$invyr
    df_params[,"timeint"] = 1
    df_params[,"numcycle"] = 1
    df_params[,"input_db"] = "c:/temp/fordata.db"
    df_params[,"fvs_path"] = "C:/FVSbin/FVSca.exe"
    df_params[,"tree_table"] = "FVS_treeinit"
    df_params[,"stand_table"] = "FVS_standinit"
    df_params
    
  #prepare prototype key file - the fvs_prototype_keyfile function just puts together a 
  #series of text strings. Some common keywords are suggested (e.g. notriple, nodgl, and dgstdev). 
  #If the parameter is NULL, then the keyword is not included, and other keywords can be supplied with "other_keywords" parameter
    
    #By default, notriple = NULL, so these two are equivalent
    key_proto = fvs_prototype_keyfile(notriple="NoTriple")
    key_proto = fvs_prototype_keyfile(other_keywords="NoTriple")
    
  #convert prototype key file into series of key files associated with each stand_id
    df_keys = fvs_make_keyfiles(df_params, key_proto = key_proto, cluster = clus1)
  
  #lastly, actually run fvs - if processing in parallel this function creates multiple output dbs, by default these are merged and temp files deleted
    fvs_run(df_keys, cluster = clus1, do_merge=T, merge_db=dir_fvs_out)
  
  #release resources assigned to cluster  
    parallel::stopCluster(clus1);rm(clus1)

```

# RForInvt NVEL example
``` 
        require(RForInvt)
        
        #grab list of species
        if(!"dfSpp" %in% ls()){
          library(RSQLite)
          db0 = dbConnect(RSQLite::SQLite(), system.file("misc/NBEL/BiomassEqns.db", package="RForInvt"))
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

