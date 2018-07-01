#' export_displacement
#'
#' This function export the displacement file to be visualized into Paraview in an interactive way
#' @param pca_mods_object list: output of the pca_mods function.
#' @param magnification numeric: magnification factor for shape variations.
#' @param interactive_plot logical: if TRUE the user can click on the PCA to visualize the associated shape.
#' @param dimx numeric: x voxel size.
#' @param dimy numeric: y voxel size.
#' @param dimz numeric: z voxel size.
#' @author Antonio Profico, Paul O'Higgins
#' @export

export_displacement<-function(pca_mods_object, magnification=NULL,
                              dimx=0,dimy=0,dimz=0){
  
  cat("Actually, the function export_displacemnt","\n")
  cat("works fine only using the entire model","\n")
  cat("So, be careful. Check that","\n")
  cat("the argument subsel is set on FALSE","\n")
  cat("when you run the function pca_mods","\n")
  cat("As soon as possible I'll write a function","\n")
  cat("to save the displacement file","\n")
  cat("when only a subselection of voxels is requested","\n")
  
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
  
  print(cbind(1:length(labels),labels))
  cat("please type a number between 1 and",
      length(labels),"\n")
  ans<-readline("which is the unloaded model?")
  ans<-as.numeric(ans)
  
  plot3d(PCXs_s,aspect =FALSE)
  plot3d(pca_mods_object$ori_array[,,ans],aspect =FALSE,col=2,add=TRUE)
  plot3d(just_rot_PCXs,aspect =FALSE,col=3,add=TRUE)
  
  just_rot_mean<-rotonmat(pca_mods_object$mshape,pca_mods_object$mshape,pca_mods_object$ori_array[,,ans])
  just_rot_PCXs<-rotonmat(PCXs_s,PCXs_s,pca_mods_object$ori_array[,,ans])
  just_rot_PCYs<-rotonmat(PCYs_s,PCYs_s,pca_mods_object$ori_array[,,ans])
  
  disp_PCXs<-just_rot_PCXs-just_rot_mean
  disp_PCYs<-just_rot_PCYs-just_rot_mean
  
  X_tab<-cbind(c(0:(dim(disp_PCXs)[1]-1)),format(disp_PCXs, scientific = TRUE))
  Y_tab<-cbind(c(0:(dim(disp_PCXs)[1]-1)),format(disp_PCYs, scientific = TRUE))
  
  cat(paste("nnodes  ",dim(min_tab)[1], "\n", sep = ""), 
      file = "disp_PCXs.txt", 
      append = TRUE, sep = "")
  cat(paste("dim",dimx, dimy, dimz, "\n", sep = " "),
      file = "disp_PCXs.txt", 
      append = TRUE, sep = "")
  cat(paste("materials materials.txt","\n", sep = " "),
      file = "disp_PCXs.txt", 
      append = TRUE, sep = "")
  write.table(X_tab, 
              file = "disp_PCXs.txt", 
              sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, 
              col.names = FALSE, na = "")
  
  cat(paste("nnodes  ",dim(min_tab)[1], "\n", sep = ""), 
      file = "disp_PCYs.txt", 
      append = TRUE, sep = "")
  cat(paste("dim",dimx, dimy, dimz, "\n", sep = " "),
      file = "disp_PCYs.txt", 
      append = TRUE, sep = "")
  cat(paste("materials materials.txt","\n", sep = " "),
      file = "disp_PCYs.txt", 
      append = TRUE, sep = "")
  write.table(X_tab, 
              file = "disp_PCYs.txt", 
              sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, 
              col.names = FALSE, na = "")
}