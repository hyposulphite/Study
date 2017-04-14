my_file_gen = function(folder, file_num_limit=NULL){
  path = paste("C:/Users/n0240416/Documents/R/study/TM/data/",folder,sep="")
  textdata = NULL
  all_files = list.files(path)
  if (!is.null(file_num_limit)) all_files = all_files[1:file_num_limit]
  for (file in all_files) {
    textdata_temp = data.frame(t(t(readLines(file.path(path, file)))))
    names(textdata_temp) <- "text"
    textdata_temp$text = as.character(textdata_temp$text)
    textdata_temp$book = gsub("\\..{3}$", "", file)
    textdata = rbind(textdata, textdata_temp)
  }
  return(as_data_frame(textdata))
}