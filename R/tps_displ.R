#' tps_displ
#'
#' This function performs define a new matrix via TPS.
#' @param tar numeric: matrix k x 3, matrix to be warped.
#' @param ref numeric: matrix k x 3, reference matrix.
#' @param mag numeric: PCscore to be plotted on x axis.
#' @param sel numeric: PCscore to be plotted on y axis.
#' @return Variance numeric: table of the explained Variance by the PCs
#' @return PCscores numeric: PC scores
#' @return PCs numeric: Principal Components
#' @return mshape numeric: sample meanshape
#' @return ori_array numeric: input k x 3 x n real array, where k is the number of points and n is the sample size
#' @author Antonio Profico, Paul O'Higgins
#' @export

tps_displ<-function(tar,ref,mag,sel){
  if(is.null(sel)==FALSE){
    subsel<-vcgKmeans(ref,k= sel,getClosest=TRUE)
    refs<-ref[subsel$selected,]
    tars<-tar[subsel$selected,]
    refm<-refs+((tars-refs)*mag )
    # tarm<-tars+((refs-tars)*mag )
  }else{
    refs<-ref
    tars<-tar
    refm<-refs+((tars-refs)*mag )
  }
  
  warped<-tps3d(tar,tars,refm)
  return(warped)
}