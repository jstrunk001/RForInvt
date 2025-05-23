% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NVEL_voleq.R
\name{NVEL_voleq}
\alias{NVEL_voleq}
\title{Select Appropriate Volume equation for Region, Forest, District, Tree species from National Volume Estimator Library (NVEL)}
\usage{
NVEL_voleq(
  dfTL = list(NA, data.frame(spcd = 201, dbh = 5, ht = 5, region = 0, forest = "01",
    district = "01"))[[1]],
  region = NA,
  forest = NA,
  district = NA,
  spcd = NA,
  regionNm = "region",
  forestNm = "forest",
  districtNm = "district",
  spcdNm = "spcd",
  dll_64 = system.file("lib/VolLibDll20231106/vollib-64bits/vollib.dll", package =
    "RForInvt"),
  dll_32 = system.file("lib/VolLibDll20231106/vollib-32bits/vollib.dll", package =
    "RForInvt"),
  dll_func_voleq = "getvoleq_r",
  load_dll = T
)
}
\arguments{
\item{dfTL}{data.frame with tree records}

\item{region}{(optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm}

\item{forest}{(optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm}

\item{district}{(optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm}

\item{regionNm}{(optional) column name in DFTL:provide region, forest, district for every tree in dfTL}

\item{forestNm}{(optional) column name in DFTL:provide region, forest, district for every tree in dfTL}

\item{districtNm}{(optional) column name in DFTL: provide region, forest, district for every tree in dfTL}

\item{spcdNm}{(required) column name in DFTL: USFS species code}

\item{dll_64}{path to 64bit dll}

\item{dll_32}{path to 64bit dll}

\item{dll_func_voleq}{name of volume equation chooser function call in NVEL .dll}

\item{load_dll}{T/F should dll be loaded (in case it is already loaded)}
}
\value{
reformatted tree list with with a new column volume equation codes for NVEL - "voleq"
}
\description{
Select Appropriate Volume equation for Region, Forest, District, Tree species from National Volume Estimator Library (NVEL)
}
\details{
Select Appropriate Volume equation for Region, Forest, District, Tree species from National Volume Estimator Library (NVEL)

\cr
Revision History
\tabular{ll}{
1.0 \tab 5/19/2020 Function created \cr
}
}
\examples{

        #look up volume equations
         library(RSForInvt)
         NVEL_voleq(region = 2, forest = "01",district = "01", spcd=951)
         NVEL_voleq(region = 2, forest = "01",district = "01", spcd=951)
         NVEL_voleq(region = 2, forest = "01",district = "01", spcd=rep(c(951,201),2))
         NVEL_voleq(dfTL=data.frame(region = 6, forest = "01",district = "01", spcd=rep(c(951,201),2)))

        #grab list of species
        if(!"dfSpp" \%in\% ls()){
          library(RSQLite)
          db0 = dbConnect(RSQLite::SQLite(), "code/BiomassEqns.db")
          dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
          dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
          dbDisconnect(db0)
        }

        #build a fake tree list
        if(!"dfSpp" \%in\% ls()){
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
        NVEL_voleq( dfTL = df_fake )





}
\seealso{
\code{\link{NVEL_volume}}\cr \code{\link{NVEL_biomass}}\cr
}
\author{
Jacob Strunk <someone@somewhere.com>
}
