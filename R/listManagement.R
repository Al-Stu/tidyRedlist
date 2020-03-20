#-------------------------------------------------------------------------#
#---------------------------- LIST MANAGEMENT ----------------------------#
#-------------------------------------------------------------------------#

#' Export list of dataframes to any format, requires \link[rio] for non-csv filetypes
#'
#' @param list the list to be exported
#' @param directory the directory the list is to be exported to
#' @param prefix the prefix that will be added to file names
#' @param fileType the intended export filetype e.g. 'csv'
#' @examples
#' exportList(speciesData,'/exported_files','species_data','csv')

exportList <- function(list, directory, prefix, fileType){
  current_wd <- getwd()
  setwd(directory)
  if(fileType == 'csv'){
    for(i in 1:length(list)){
      directory <- as.character(directory)
      write.csv(x = list[i], file = paste(prefix,'_',names(list)[i],'.csv',sep=''))
    }
  } else {
    if(any(grepl('rio',installed.packages()))){
      rio::export(list[i], paste(prefix,'_',names(list)[i],'.',fileType,sep=''))
    } else {
      stop('package rio required to export to filetypes other than .csv')
    }
  }
  setwd(current_wd)
}

#' Export list of dataframes to any format, requires \link[rio] for non-csv filetypes
#'
#' @param directory the directory the list is to be exported to
#' @param prefix the prefix included in all names of files to be imported,
#' '' if all files in directory
#' @param fileType the filetype of files to be imported, '' if not all one type
#' @examples
#' importList('/exported_files','species_data','csv')

importList <- function(directory, prefix, fileType){
  current_wd <- getwd()
  setwd(directory)
  files <- list.files(pattern = paste(prefix, ".*",
      paste(fileType,'$', sep=''), sep=''))
  if(fileType == 'csv'){
    result <- lapply(files, readr::read_csv)
  } else {
    if(any(grepl('rio',installed.packages()))){
      result <- lapply(files, rio::import)
    } else {
      stop('package rio required to export to filetypes other than .csv')
    }
  }
  names(result) <- gsub(x = files, pattern = paste('[.]',fileType,sep=''),replacement = '')
  names(result) <- gsub(x = names(result), pattern = prefix, replacement = '')
  for(i in 1:length(result)){
    colnames(result[[i]]) <- gsub(x = colnames(result[[i]]),
        pattern = paste(names(result)[i],'[.]',sep=''), replacement = '')
    result[[i]] <- result[[i]][,-1]
  }
  setwd(current_wd)
  return(result)
}


