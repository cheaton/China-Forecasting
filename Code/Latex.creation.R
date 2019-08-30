#######################################
# Some functions for writing LaTex files from R.
#
# write.Latex.preamble(title,author,abstract,file.name)
# write.Latex.section(section.name,file.name)
# write.Latex.text(text,file.name)
# write.Latex.subsection(subsection.name,file.name)
# write.Latex.subsubsection(subsubsection.name,file.name)
# write.Latex.pdfbookmark(level,text,name,file.name)
# write.Latex.table.file(table.filename,file.name)
# write.Latex.plot(plot.filename,width,height,file.name)
# write.end.Latex.document(bibiographystyle=apa,bibtex.filename=NULL,file.name)
# write.Latex.FloatBarrier(file.name)
#
# Example:
# write.Latex.preamble("Plots and Tables for China Macro-Forecasting Paper","Chris Heaton","Some output for the China macro forecasting project","Output_dump.tex")
# write.Latex.section("MSEs and p-values","Output_dump.tex")
# write.Latex.pdfbookmark(2,"MSE for CPI","MSECPI","Output_dump.tex")
# write.Latex.table.file("latex.CPI.rel.MSE.table.tex","Output_dump.tex")
# write.Latex.plot("Lossplot_CPI_Naive_h1_MSE.pdf",12,7,"Output_dump.tex")
# write.end.Latex.document("apa","referencesHPZ.tex","Output_dump.tex")


write.Latex.preamble <- function(title,author,abstract,file.name)
{
# Creates a file called file.name that contains a preamble for a Latex article with title, author
# and abstract.
#
#                                                  Chris Heaton, February 2017.

  preamble <- paste("\\documentclass[12pt,english,a4paper,reqno]{article}
  \\usepackage[]{graphicx}
  \\usepackage[]{color}
  \\usepackage{amssymb}
  \\usepackage[pdfborder={0 0 0 [3 3]},pagebackref=true]{hyperref}
  \\usepackage{amsmath}
  \\usepackage{mathrsfs}
  \\usepackage[authoryear,round]{natbib}
  \\usepackage[hmargin=3cm,top=2cm,bottom=3cm]{geometry}
  \\usepackage{latexsym}
  \\usepackage{enumitem}
  \\usepackage{babel}
  \\usepackage{setspace}
  \\usepackage{booktabs}
  \\usepackage{graphicx}
  \\usepackage{color}
  \\usepackage{titlesec}
  \\usepackage{longtable}
  \\usepackage{placeins}
  \\usepackage{booktabs}
  \\usepackage{subfig}
  \\usepackage{caption}
  \\usepackage[normalem]{ulem}
  \\usepackage{placeins}
  
  
  
  \\doublespacing
  
  \\newcommand{\\blue}[1]{{\\color{blue}#1}}					%Used to get blue text
  \\newcommand{\\red}[1]{{\\color{red}#1}}					%Used to get red text
  \\newcommand{\\green}[1]{{\\color{green}#1}}				%Used to get green text
  \\newcommand{\\field}[1]{\\mathbb{#1}}
  \\newcommand{\\e}{\\varepsilon}
  \\newcommand{\\s}{\\sigma}
  \\newcommand{\\bt}{\\beta}
  \\newcommand{\\n}{\\eta}
  \\newcommand{\\g}{\\gamma}
  \\newcommand{\\dt}{\\delta}
  \\newcommand{\\sumiN}{\\sum \\limits_{i=1}^N}
  \\newcommand{\\sumtT}{\\sum \\limits_{t=1}^T}
  \\newcommand{\\sumin}{\\sum \\limits_{i=1}^{n_{j}}}
  \\newcommand{\\sumjm}{\\sum \\limits_{j=1}^m}
  \\newcommand{\\sumkn}{\\sum \\limits_{k=1}^{n_{j}}}
  
  \\title{",title,"}
  \\begin{document}
  
  \\author{",author,"}
  
  \\maketitle
  \\begin{abstract}
  ",abstract,"
  \\end{abstract}
  \\date{}\n\n",sep="")
  
  cat(preamble,file=file.name,sep="",append=FALSE)
}


write.Latex.section <- function(section.name,file.name)
{
  # Appends \section{section.name} to the file file.name.
  #
  #                                                  Chris Heaton, February 2017.
  
  cat(paste("\\section{",section.name,"}\n\n",sep=""),sep="",file=file.name,append=TRUE)
}

write.Latex.text <- function(text,file.name)
{
  # Appends the character in text to the file file.name
  #
  #                                                 Chris Heaton, March 2018.
  cat(text,file=file.name,append=TRUE)
}

write.Latex.subsection <- function(subsection.name,file.name)
{
  # Appends \subsection{subsection.name} to the file file.name.
  #
  #                                                  Chris Heaton, February 2017.
  
  cat(paste("\\subsection{",subsection.name,"}\n\n",sep=""),sep="",file=file.name,append=TRUE)
}


write.Latex.subsubsection <- function(subsubsection.name,file.name)
{
  # Appends \subsubsection{subsubsection.name} to the file file.name.
  #
  #                                                  Chris Heaton, February 2017.
  
  cat(paste("\\subsubsection{",subsubsection.name,"}\n\n",sep=""),sep="",file=file.name,append=TRUE)
}

write.Latex.pdfbookmark <- function(level,text,name,file.name)
{
  # Appends a pdfbookmark to the file file.name. level is an integer (1 being the highest level, 2 being a sublevel,
  # 3 a subsublevel, etc. text is the text that will appear in the bookmark and name is a unique label for the
  # bookmark.
  #
  #                                                  Chris Heaton, February 2017.
  
  cat(paste("\\pdfbookmark[",level,"]{",text,"}{",name,"}",sep=""),file=file.name,sep="",append=TRUE)
}


write.Latex.table.file <- function(table.filename,file.name)
{
  # A latex table has been created with xtable and saved to the file table.filename. This function
  # appends the table to the file file.name.
  #
  #                                                  Chris Heaton, February 2017.
  
  cat(paste("\\input{",table.filename,"}\n\n",sep=""),file=file.name,sep="",append=TRUE)
}

write.Latex.plot <- function(plot.filename,caption,label,width,height,file.name)
{
  # A pdf plot is in the file plot.filename. This function reads it in and appends it to the file filename.
  # width and height are the width and height of the plot in cm. The plot will be centred.
  #
  #                                                     Chris Heaton, February 2017.
out <- paste("\n\n\\begin{figure}[ht]
\\caption{",caption,"}
\\label{",label,"}
\\centering
\\includegraphics[width=",width,"cm,height=",height,"cm]{",plot.filename,"}
\\end{figure}\n\n",sep="")
cat(out,file=file.name,append=TRUE)
#  cat("\\begin{center}\n\n",file=file.name,sep="",append=TRUE)
#  cat(paste("\\includegraphics[width = ",width,"cm, height = ",height,"cm]{",plot.filename,"}\n\n",sep=""),file=file.name,sep="",append=TRUE)
#  cat("\\end{center}\n\n",file=file.name,sep="",append=TRUE)
}

write.end.Latex.document <- function(bibiographystyle="apa",bibtex.filename=NULL,file.name)
{
  # Appends the file file.name with \end{document}. If bibtex.filename is not NULL then the appropriate
  # bibiography{} command is also included with the bibiographystyle specified.
  #
  #                                                      Chris Heaton, February 2017.

  if (is.null(bibtex.filename))
  {
    cat(paste("\\bibliographystyle{",bibiographystyle,"}\n",sep=""),sep="",file=file.name,append=TRUE)
    cat(paste("\\bibliography{",bibtex.filename,"}\n",sep=""),sep="",file=file.name,append=TRUE)
  }
  cat("\\end{document}\n",sep="",file=file.name,append=TRUE)
}

write.Latex.FloatBarrier <- function(file.name)
{
  # Appends \FloatBarrier to the file file.name
  #                                             Chris Heaton, March 2017.
  cat("\\FloatBarrier\n\n",sep="",file=file.name,append=TRUE)
}
