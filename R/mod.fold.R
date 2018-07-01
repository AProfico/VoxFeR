#' mod.fold
#'
#' This function stores into an array the model and displacement files from Paraview
#' @param folder.path path of the folder. If NULL use a Windows shell folder widget to choose a folder interactively
#' @return position numeric: a vector of the row indices 
#' @return data numeric: an array containing the model/displacement files exported from Paraview 
#' @author Antonio Profico, Paul O'Higgins
#' @export

mod.fold<-function(folder.path){
  if(is.null(folder.path)==TRUE){
  dir<-choose.dir()}else{
  dir<-folder.path
  }
  temp<-as.matrix(read.table(paste(dir,list.files(dir)[1],sep="/")))
  data<-array(NA,dim=c(dim(temp)[1],3,length(list.files(dir))))
  for(i in 1:length(list.files(dir))){
    data[,,i]<-as.matrix(read.table(paste(dir,list.files(dir)[i],sep="/")))
  }
  dimnames(data)[[3]]<-list.files(dir)
  temp<-NULL
  print("you imported these files:")
  for(i in 1:length(list.files(dir))){
    cat(list.files(dir)[i],"\n")
  }
  return(data)
}