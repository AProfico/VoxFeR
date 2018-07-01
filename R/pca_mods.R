#' pca_mods
#'
#' This function performs and plots a pca.
#' @param array numeric: input k x 3 x n real array, where k is the number of points and n is the sample size.
#' @param scale logical: indicating if scaling is requested to minimize the General Procrustes distance.
#' @param PCx numeric: PCscore to be plotted on x axis.
#' @param PCy numeric: PCscore to be plotted on y axis.
#' @param subsel logical: if TRUE use a Windows shell folder widget to choose a file interactively in which a subselection of coordinates is stored.
#' @return Variance numeric: table of the explained Variance by the PCs
#' @return PCscores numeric: PC scores
#' @return PCs numeric: Principal Components
#' @return mshape numeric: sample meanshape
#' @return ori_array numeric: input k x 3 x n real array, where k is the number of points and n is the sample size
#' @author Antonio Profico, Paul O'Higgins
#' @export

pca_mods<-function(array,scale=T,PCx=1,PCy=2,subsel=FALSE){
  
  if(subsel==TRUE){
    cat("please, select the file where the selected voxels are stored","\n")
    fil<-file.choose()
    sel_fem<-as.matrix(read.table(fil))
    cat("please, select the original file from which you made the subselection","\n")
    fil1<-file.choose()
    sel_fem2<-as.matrix(read.table(fil1))
    sel_pos3<-vcgKDtree((sel_fem2),(sel_fem), 2, nofPoints = 16, maxDepth = 64, threads = 1)
    sel_pos4<-sel_pos3$index[,1]
    
    cat("\n","this is the subselection of voxels","\n")
    plot3d(sel_fem2[unique(sel_pos4),],aspect = FALSE,col=2)
    array<-array[unique(sel_pos4),,]
  }
  
  
  if(scale==TRUE){
    pca<-procSym(array)
  }
  if(scale==FALSE){
    pca<-procSym(array,scale=FALSE,CSinit = FALSE)
  }
  labels<-dimnames(array)[[3]]
  PCX<-pca$PCscores[,PCx]
  PCY<-pca$PCscores[,PCy]
  
  xlim<-extendrange(range(PCX))
  ylim<-extendrange(range(PCY))
  VarPCX<-round(pca$Variance[PCx,2],2)
  VarPCY<-round(pca$Variance[PCy,2],2)
  
  x11()
  plot(PCX,PCY,xlim=xlim,ylim=ylim,asp=1,main="PCA",
       xlab=paste("PC",PCx," (",VarPCX,"%)",sep=""),
       ylab=paste("PC",PCy," (",VarPCY,"%)",sep=""))
  text(PCX,PCY,labels = labels,pos=1)
  
  out<-list("Variance"=pca$Variance,"PCscores"=pca$PCscores,
            "PCs"=pca$PCs,"mshape"=pca$mshape,"ori_array"=array)
}
