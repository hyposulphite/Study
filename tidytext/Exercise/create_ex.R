for (chapter in 1:6){
  lines = scan(paste("./tidytext/Chapter",chapter,".R",sep=""), what=character(0), sep="\n")
  ans = NULL
  for (line in lines){
    if (length(grep(pattern="^###|^#\\?#", line))) ans = paste(ans, line, sep="\n\n")
    if (length(grep(pattern="^library", line))) ans = paste(ans, line, sep="\n")
  }
  write(ans, paste("./tidytext/Exercise/Ex_Chapter",chapter,".R",sep=""))
}