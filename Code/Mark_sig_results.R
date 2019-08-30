# Go through all the LaTeX p-value tables. If a result indicates that a forecast is statistically significantly worse than the benchmark, then the p-value is changed to italics. If it is significantly better, then the p-value is changed to bold.

if (whereami){cat("\nMarking up p-value tables to show statistical significance")}

############## Functions ###################
LaTeX.table.2.matrix <- function(table.in)
{
  first.line <- grep("\\midrule",table.in,fixed=TRUE) + 1
  last.line <- grep("\\bottomrule",table.in,fixed=TRUE) - 1
  content <- table.in[first.line:last.line]
  
  content <- gsub("\\\\","",content,fixed=TRUE)
  content.df <- read.table(text=content,sep="&")
  content.matrix <- as.matrix(content.df[,2:dim(content.df)[2]])
  rownames(content.matrix) <- trimws(as.character(content.df[,1]))
  return(content.matrix)
}

matrix.2.LaTeX.table <- function(p.table.char,table.in)
{
  mu.p.table <- NULL
  for (i in 1:dim(p.table.char)[1])
  {
    mu.p.table <- c(mu.p.table,paste(rownames(p.table.char)[i]," & ",paste(p.table.char[i,],collapse=" & ")," \\\\",sep=""))
  }
  first.line <- grep("\\midrule",table.in,fixed=TRUE) + 1
  last.line <- grep("\\bottomrule",table.in,fixed=TRUE) - 1
  table.out <- table.in
  table.out[first.line:last.line] <- mu.p.table
  return(table.out)
}


########## Main Program ###########

for (variable in variable.names)
{
  for (crit in c("MSE","MAE","MAPE"))
  {
    for (type in c("levels","differences","changes"))
    {
      for (unco in c("conditional","unconditional"))
      {
        cmd <- paste("p.value.table.in <- readLines('../Output/pvalue_tables/",variable,"_",crit,"_",type,"_",unco,".pvalues.tex')",sep="")
        eval(parse(text=cmd))
        cmd <- paste("rel.table.in <- readLines('../Output/Error_tables/rel_",crit,"_",type,"_",variable,".tex')",sep="")
        eval(parse(text=cmd))
        
        p.table <- LaTeX.table.2.matrix(p.value.table.in) # Get the bit of LaTeX containing the table elements as a matrix.
        rel.table <- LaTeX.table.2.matrix(rel.table.in) # Get the bit of LaTeX containing the table elements as a matrix.
        rel.table <- rel.table[!rownames(rel.table) %in% benchmark, ] # Remove the benchmark from rel.table.
        
        # Figure out the indexes of elements in p-table that are significantly worse or better than the benchmark.
        worse.indexes <- which((p.table<alpha)&(rel.table>=1), arr.ind = TRUE)
        better.indexes <- which((p.table<alpha)&(rel.table<1), arr.ind = TRUE)
        num.worse <- dim(worse.indexes)[1]
        num.better <- dim(better.indexes)[1]
        
        # For those elements that are significantly worse, put the p-value in italics.
        
        p.table.char <- p.table; p.table.char[1,1] <- as.character(p.table.char[1,1])
        if (num.worse>0)
        {
          for (i in 1:num.worse)
          {
            p.table.char[worse.indexes[i,1],worse.indexes[i,2]] <- paste("\\it{",p.table.char[worse.indexes[i,1],worse.indexes[i,2]],"}",sep="")
          }
        }
        
        # For those elements that are significantly better, put the p-value in bold.
        
        if (num.better>0)
        {
          for (i in 1:num.better)
          {
            p.table.char[better.indexes[i,1],better.indexes[i,2]] <- paste("\\bf{",p.table.char[better.indexes[i,1],better.indexes[i,2]],"}",sep="")
          }
        }
        
        finished.table <- matrix.2.LaTeX.table(p.table.char,p.value.table.in)  # Convert the marked up matrix into text.
        cmd <- paste("writeLines(finished.table,'../Output/pvalue_tables/",variable,"_",crit,"_",type,"_",unco,".pvalues.tex')",sep="")
        eval(parse(text=cmd))
       }
    }
  }
}
