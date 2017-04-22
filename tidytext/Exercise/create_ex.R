for (chapter in 1:6){
  lines = scan(paste("./tidytext/Chapter",chapter,".R",sep=""), what=character(0), sep="\n")
  ans = NULL
  print_ind = FALSE
  for (line in lines){
    if (length(grep(pattern="^#_#", line))>0) print_ind = TRUE
    else if (length(grep(pattern="^#__#", line))>0) print_ind = FALSE
    
    if (print_ind) {
      ans = paste(ans, line, sep="\n")
    } else {
      if (length(grep(pattern="^###|^#\\?#", line))>0) ans = paste(ans, line, sep="\n\n")
      else if (length(grep(pattern="^library", line))>0) ans = paste(ans, line, sep="\n")
    }
  }
  write(ans, paste("./tidytext/Exercise/Ex_Chapter",chapter,".R",sep=""))
}