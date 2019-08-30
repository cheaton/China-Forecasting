remove_table_tags <- function(directory)
{
  ### Reads in .tex files that contain tables and removes everything that doesn't 
  ### lie withing the \begin{tabular} and \end{tabular} commmands. This is useful
  ### if you want to use the \input command to add the tables to a .tex file
  ### but to specify captions, labels, etc in the main .tex file rather than taking
  ### them from the file containing the table.
  ###
  ### e.g. remove_table_tags("../Output/Error_tables")
  
  all.filenames <- dir(directory)
  tex.filenames <- all.filenames[grep(".tex",all.filenames,fixed=TRUE)]
  tex.filenames.without.extensions <- unlist(strsplit(tex.filenames,".tex"))
  for (i in 1:length(tex.filenames))
  {
    this.file <- tex.filenames[i]
    old.contents <- readLines(paste(directory,"/",this.file,sep=""))
    tabular.lines <- grep("{tabular}",old.contents,fixed=TRUE)
    new.contents <- old.contents[tabular.lines[1]:tabular.lines[2]]
    writeLines(new.contents,paste(directory,"/",tex.filenames.without.extensions[i],"_tabular.tex",sep=""))
  }
}
