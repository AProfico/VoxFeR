#' pca_shape_v
#'
#' This function visualize shape variations using the output of the pca_mods function
#' @param pca_mods_object list: output of the pca_mods function.
#' @param magnification numeric: magnification factor for shape variations.
#' @param interactive_plot logical: if TRUE the user can click on the PCA to visualize the associated shape.
#' @param PCx numeric: PCscore to be plotted on x axis.
#' @param PCy numeric: PCscore to be plotted on x axis.
#' @param PCXs numeric: insert the values of PCscore for the shape variation requested.
#' @param PCYs numeric: insert the values of PCscore for the shape variation requested.
#' @author Antonio Profico, Paul O'Higgins
#' @export

pca_shape_v<-function(pca_mods_object,magnification=100,
                      interactive_plot=TRUE,
                      PCx=1,PCy=2,PCXs=NULL,PCYs=NULL,
                      PCscores=NULL){
  
  pca_mods_object<-analyse_1
  if(interactive_plot==TRUE){
    
    labels<-rownames(pca_mods_object$PCscores)
    PCX<-pca_mods_object$PCscores[,PCx]
    PCY<-pca_mods_object$PCscores[,PCy]
    xlim<-extendrange(range(PCX))
    ylim<-extendrange(range(PCY))
    VarPCX<-round(pca_mods_object$Variance[PCx,2],2)
    VarPCY<-round(pca_mods_object$Variance[PCy,2],2)
    
    x11()
    plot(PCX,PCY,xlim=xlim,ylim=ylim,asp=1,main="PCA",
         xlab=paste("PC",PCx," (",VarPCX,"%)",sep=""),
         ylab=paste("PC",PCy," (",VarPCY,"%)",sep=""))
    text(PCX,PCY,labels = labels,pos=1)  
    sel_poi<-locator(n=1)
    PCXs<-sel_poi$x
    PCYs<-sel_poi$y
    mag<-magnification
    if(is.null(magnification)==TRUE){
      mag<-1
    }
    PCXs_s<-showPC(PCXs*mag, pca_mods_object$PCs[,PCx], 
                   pca_mods_object$mshape)
    PCYs_s<-showPC(PCYs*mag, pca_mods_object$PCs[,PCy], 
                   pca_mods_object$mshape)
    
    
    open3d()
    plot3d(PCXs_s,aspect = FALSE,col="green",
           main=paste("PC",PCx, "at", round(PCXs*mag,3)))
    open3d()
    plot3d(PCYs_s,aspect = FALSE,col="red",
           main=paste("PC",PCy, "at", round(PCYs*mag,3)))
    
  }
  
  if(is.null(PCscores)==TRUE){
    if(interactive_plot==FALSE){
      
      labels<-rownames(pca_mods_object$PCscores)
      PCX<-PCXs
      PCY<-PCYs
      
      mag<-magnification
      if(is.null(magnification)==TRUE){
        mag<-1
      }
      
      PCXs_s<-showPC(PCXs*mag, pca_mods_object$PCs[,PCx], 
                     pca_mods_object$mshape)
      PCYs_s<-showPC(PCYs*mag, pca_mods_object$PCs[,PCy], 
                     pca_mods_object$mshape)
      
      
      open3d()
      plot3d(PCXs_s,aspect = FALSE,col="green",
             main=paste("PC",PCx, "at", round(PCXs*mag,3)))
      open3d()
      plot3d(PCYs_s,aspect = FALSE,col="red",
             main=paste("PC",PCy, "at", round(PCYs*mag,3)))
      
    }
  }
  if(is.numeric(PCscores)==TRUE){
    
    mag<-magnification
    if(is.null(magnification)==TRUE){
      mag<-1
    }
    
    PC_min<-showPC(min(pca_mods_object$PCscores[,PCscores])*mag, pca_mods_object$PCs[,PCscores], 
                   pca_mods_object$mshape)
    PC_max<-showPC(max(pca_mods_object$PCscores[,PCscores])*mag, pca_mods_object$PCs[,PCscores], 
                   pca_mods_object$mshape)
    
    
    open3d()
    plot3d(PC_min,aspect = FALSE,col="red",
           main=paste("PC",PCscores, "at", round(min(pca_mods_object$PCscores[,PCscores])*mag,3)))
    open3d()
    plot3d(PC_max,aspect = FALSE,col="green",
           main=paste("PC",PCscores, "at", round(max(pca_mods_object$PCscores[,PCscores])*mag,3)))
    
    
  }
  
}