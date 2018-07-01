#' export_displacement
#'
#' This function export the displacement file to be visualized into Paraview in an interactive way
#' @param pca_mods_object list: output of the pca_mods function.
#' @param magnification numeric: magnification factor for shape variations.
#' @param interactive_plot logical: if TRUE the user can click on the PCA to visualize the associated shape.
#' @param dimx numeric: x voxel size.
#' @param dimy numeric: y voxel size.
#' @param dimz numeric: z voxel size.
#' @param from logical: if TRUE through an interactive way select the reference label.
#' @param to logical: if TRUE through an interactive way select the reference label
#' @param GPA logical: if TRUE a GPA step will be performed
#' @author Antonio Profico, Paul O'Higgins
#' @export

export_displacement<-function(pca_mods_object, magnification=NULL,
                              dimx=0,dimy=0,dimz=0,
                              from=NULL,to=NULL,GPA=NULL){
  if(from==TRUE & to==TRUE){ 
    labels<-rownames(pca_mods_object$PCscores)
    print(cbind(1:length(labels),labels))
    cat("please type a number between 1 and",
        length(labels),"\n")
    ans_from=readline("which is the unloaded model?")
    ans_from<-as.numeric(ans_from)
    ans_to=readline("which is the target model?")
    ans_to<-as.numeric(ans_to)
if(GPA==TRUE){
temp_PCA<-procSym(pca_mods_object$ori_array,scale=FALSE,CSinit = FALSE)$rotated
just_rot_to<-rotonmat(temp_PCA[,,ans_to],temp_PCA[,,ans_to],pca_mods_object$ori_array[,,ans_from],scale=FALSE)}else{
just_rot_to<-pca_mods_object$ori_array[,,ans_to]
    } 
    disp_to<-just_rot_to-pca_mods_object$ori_array[,,ans_from]
    to_tab<-cbind(c(0:(dim(disp_to)[1]-1)),format(disp_to, scientific = TRUE))
    
    cat(paste("nnodes  ",dim(to_tab)[1], "\n", sep = ""), 
        file = "disp_to.txt", 
        append = TRUE, sep = "")
    cat(paste("dim",dimx, dimy, dimz, "\n", sep = " "),
        file = "disp_to.txt", 
        append = TRUE, sep = "")
    cat(paste("materials materials.txt","\n", sep = " "),
        file = "disp_to.txt", 
        append = TRUE, sep = "")
    write.table(to_tab, 
                file = "disp_to.txt", 
                sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, 
                col.names = FALSE, na = "")
  }else{
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
  
  just_rot_PCXs<-rotonmat(PCXs_s,PCXs_s,pca_mods_object$ori_array[,,ans],scale=FALSE)
  just_rot_PCYs<-rotonmat(PCYs_s,PCYs_s,pca_mods_object$ori_array[,,ans],scale=FALSE)
  
  disp_PCXs<-just_rot_PCXs-pca_mods_object$ori_array[,,ans]
  disp_PCYs<-just_rot_PCYs-pca_mods_object$ori_array[,,ans]
  
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
}
