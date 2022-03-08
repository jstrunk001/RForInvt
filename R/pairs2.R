#
#'@title 
#' xy plot of variables in two dataframes
#'
#'@description 
#'
#'accepts two dataframes and prepares pairplots, results in fewer combinations than plot(xy_df)
#'
#'@details
#'
#'<Delete and Replace>
#'
#'\cr
#'
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2015 Jan 29 Roxygen header prepared \cr
#'1.1 \tab 2022 March 03 added to RForInvt and added combination alternative \cr
#'}
#'
#'
#'@author
#'Jacob Strunk <strunky@g@mail.com> 

#'
#'@param x first data.frame
#'@param y second data.frame
#'@param str_max number of characters per name to display
#'@param oneLine T/F add one to one line?
#'@param mai_edge "mai" parameters for composite figure
#'@param tile optional figure title
#'@param combination T/F combinations of x and y inputs or x[,1] vs y[,1] and x[,2] vs y[,2]
#'@param square T/F force graphs to be square (same x and y lims), only use if units match
#'@param ... additional arguments to plot
#'
#'@return
#'<Delete and Replace>
#'
#'@examples
#'
#' pairs2(mtcars,mtcars,combination=T)
#' pairs2(mtcars[,1:4],mtcars[5:8],combination=F)
#'
#'@seealso \code{\link{plot}}\cr 
#'
#'



#'@export
pairs2=function(
                x
                ,y
                ,str_max = 16
                ,oneLine = F
                ,mai_fig = c(.3,.3,.3,.1)
                ,mai_edge = c(0,0,0,0) 
                ,title = NA
                #,lattice = F
                ,pdf_out = NA
                ,pdf_ht = 8
                ,pdf_wd = 11
                ,las.x=1
                ,las.y=1
                ,xlab_fig = NA
                ,ylab_fig = NA
                ,combination = F
                ,square=F
                ,...
                ){

          #par_in = par()
          
          if(!is.na(pdf_out)) pdf( file = pdf_out , height = pdf_ht , width = pdf_wd )
          
        
          #layout.show(ly_in)

          if(combination){
            
            par( omi = mai_fig )
            
            ncols = length(names(x))
            nrows = length(names(y))
            col_widths = c(.9,rep(.9,ncols-1))
            row_heights = c(rep(.9,nrows-1),.9)
            
            ly_in = layout(matrix(1:(ncols*nrows), nrows , ncols , byrow = TRUE),
                           widths = col_widths,  heights = row_heights
            )
            
            nm_combn = data.frame(expand.grid(col_id = 1:length(names(x)),row_id=1:length(names(y))))
            mapply(.plot_ij, nm_combn[,1] ,nm_combn[,2] , MoreArgs = list(x=x,y=y,mai_edge, ncols=ncols, nrows=nrows,oneLine=oneLine,str_max=str_max,las.x=las.x,las.y=las.y))
            
          }else{

            par( omi = mai_fig )
            
            min_cols = min(length(names(x)),length(names(y)))
            #sides = ceiling(sqrt(min_cols))
            ncols = floor(sqrt(min_cols))
            nrows = ceiling(sqrt(min_cols))
            if( nrows*ncols < min_cols ) ncols = ncols + 1
            
            
            col_widths = c(.9,rep(.9,ncols-1))
            row_heights = c(rep(.9,nrows-1),.9)
            
            ly_in = layout(matrix(1:(ncols*nrows), nrows , ncols , byrow = TRUE),
                           widths = col_widths,  heights = row_heights
            )
            nm_combn = data.frame(col_id = 1:min_cols,row_id=1:min_cols)
            mapply(.plot2_ij, nm_combn[,1] ,nm_combn[,2] , MoreArgs = list(x=x,y=y,mai_edge, ncols=ncols, nrows=nrows,oneLine=oneLine,str_max=str_max,square=square))
            
          }
          

          if(!is.na(title)){
                  par(xpd=F)
                  mtext(title, outer = TRUE, cex = 1.5 , line = .5)
          }
          
          if(!is.na(xlab_fig)) mtext(xlab_fig,1, outer = TRUE, cex = 1 , line = 2)
          if(!is.na(ylab_fig)) mtext(ylab_fig,2, outer = TRUE, cex = 1 , line = 2)
          
          if(!is.na(pdf_out)) dev.off()
          
          #par(par_in)

}

#function to iterate across y
.plot_ij=function( i, j , y , x , mai_edge , ncols, nrows,oneLine,str_max,las.x,las.y){
  
  #prepare margins
  v_mai=c(0,0,0,0)
  if( i==1 ) v_mai[2] = mai_edge[2]
  if( i==ncols ) v_mai[4] = mai_edge[4]
  if( j==1 ) v_mai[3] = mai_edge[3]
  if( j==nrows ) v_mai[1] = mai_edge[1]
  par( mai=v_mai )
  
  #plot data
  plot(x[,i],y[,j],xaxt="n",yaxt="n",xlab="",ylab="")
  if(oneLine){
    par(xpd=F)
    lines(c(-10e6,10e6),c(-10e6,10e6))
    par(xpd=T)
  }                
  
  #print y axis when in position 1
  if( i==1 ) mtext(substr(names(y)[j],1,str_max),2,1,las = las.y)
  
  #print x axis when in last posituin
  if (j==nrows) mtext(substr(names(x)[i],1,str_max),1,1,las = las.x)
  
}

.plot2_ij=function( i, j , y , x , mai_edge , ncols, nrows,oneLine,str_max,square){
  
  #prepare margins
  v_mai=c(0,0,0,0)
  if( i==1 ) v_mai[2] = mai_edge[2]
  if( i==ncols ) v_mai[4] = mai_edge[4]
  if( j==1 ) v_mai[3] = mai_edge[3]
  if( j==nrows ) v_mai[1] = mai_edge[1]
  par( mai=v_mai )
  
  #plot data
  if(square){
    rng_in = range(c(x[,i],y[,j]))
    plot(x[,i],y[,j],xaxt="n",yaxt="n",xlab="",ylab="",xlim=rng_in, ylim=rng_in)
    
    }  else plot(x[,i],y[,j],xaxt="n",yaxt="n",xlab="",ylab="")
  if(oneLine){
    par(xpd=F)
    lines(c(-10e6,10e6),c(-10e6,10e6))
    par(xpd=T)
  }                
  
  mtext(substr(names(y)[j],1,str_max),side=2,line=-1,las = 0)
  mtext(substr(names(x)[i],1,str_max),side=1,line=-1,las = 0)

}
# 
# rng_print = 1:9
# pairs3(
#   plt_vs[, sort( vec_names_y[-c(1:2)][rng_print] ,T ) ]
#   ,
#   plt_vs[,sort(vec_names_x[-c(1)][rng_print],T )]
#   ,mai_fig =  c(.5,.5,.3,.1)
#   ,las.x=1
#   ,las.y=3
#   ,xlab_fig = "kNN Predictions"
#   ,ylab_fig = "Measurements"
#   ,oneLine = T
#   ,square=T
# )