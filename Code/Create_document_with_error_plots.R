source("Latex.creation.R")
file.name <- "../Output/Error_Analysis/Error_plots.tex"
abstract <- "This document contains plots of the mean squared forecasting errors for every variable, forecasting model and forecasting horizon in the paper ``Forecasting Models for the Chinese Macroeconomy: The Simpler the Better?'' PDF bookmarks have been used throughout the document so you can navigate using the index in your PDF viewer. The plots show a 24 month centred moving average of the squared forecasting error"
write.Latex.preamble("Error Plots for ``Forecasting Models for the Chinese Macroeconomy: The Simpler the Better?''","Anomymized for review",abstract,file.name)


write.Latex.section("Forecast Errors",file.name)
for (variable in variable.names)
{
    write.Latex.subsection(variable,file.name)
    for (h in horizons)
    {
        write.Latex.subsubsection(paste("Forecast Horizon = ",h,sep=""),file.name)
        for (model in forecast.models)
        {
            write.Latex.FloatBarrier(file.name)
            title <- paste("Errors of ",h,"-step-ahead ",model," forecast of ",variable,sep="")
            cmd <- paste("write.Latex.plot(plot.filename='Error_plot_",variable,"_",model,"_h_",h,".pdf',caption=title,label='",variable,"_h=",h,"_",model,"',width=12,height=7,file.name)",sep="")

            eval(parse(text=cmd))
            # write.Latex.pdfbookmark(3,paste(variable,".h = ",h,".",model,sep=""),paste("Raw.plot",variable,sep=""),file.name)
            write.Latex.pdfbookmark(4,model,paste(variable,h,model,"_error",sep=""),file.name)
            
            write.Latex.FloatBarrier(file.name)
        }
    }
}

write.Latex.section("MSPEs",file.name)
for (variable in variable.names)
{
    write.Latex.subsection(variable,file.name)
    for (h in horizons)
    {
        write.Latex.subsubsection(paste("Forecast Horizon = ",h,sep=""),file.name)
        for (model in forecast.models)
        {
            write.Latex.FloatBarrier(file.name)
            title <- paste("Mean squared forecasting errors of ",h,"-step-ahead ",model," forecast of ",variable,sep="")
            cmd <- paste("write.Latex.plot(plot.filename='MSPE_",variable,"_",model,"_h_",h,".pdf',caption=title,label='",variable,"_h=",h,"_",model,"',width=12,height=7,file.name)",sep="")

            eval(parse(text=cmd))
            # write.Latex.pdfbookmark(3,paste(variable,".h = ",h,".",model,sep=""),paste("Raw.plot",variable,sep=""),file.name)
            write.Latex.pdfbookmark(4,model,paste(variable,h,model,sep=""),file.name)
            
            write.Latex.FloatBarrier(file.name)
        }
    }
}

write.Latex.section("24 month moving average of MSPEs",file.name)
for (variable in variable.names)
{
    write.Latex.subsection(variable,file.name)
    for (h in horizons)
    {
        write.Latex.subsubsection(paste("Forecast Horizon = ",h,sep=""),file.name)
        for (model in forecast.models)
        {
            write.Latex.FloatBarrier(file.name)
            title <- paste("24 month moving average of mean squared forecasting errors of ",h,"-step-ahead ",model," forecast of ",variable,sep="")
            cmd <- paste("write.Latex.plot(plot.filename='MSPE_24_",variable,"_",model,"_h_",h,".pdf',caption=title,label='",variable,"_h=",h,"_",model,"',width=12,height=7,file.name)",sep="")

            eval(parse(text=cmd))
            # write.Latex.pdfbookmark(3,paste(variable,".h = ",h,".",model,sep=""),paste("Raw.plot",variable,sep=""),file.name)
            write.Latex.pdfbookmark(4,model,paste(variable,h,model,"24",sep=""),file.name)
            
            write.Latex.FloatBarrier(file.name)
        }
    }
}


write.Latex.section("Difference of MSPE from baseline MSPE",file.name)
for (variable in variable.names)
{
    write.Latex.subsection(variable,file.name)
    for (h in horizons)
    {
        write.Latex.subsubsection(paste("Forecast Horizon = ",h,sep=""),file.name)
        if (variable=="PPI")
        {
            baseline <- "Naive"
        }else
        {
            baseline <- "Mean"
        }
        for (model in setdiff(forecast.models,baseline))
        {
            write.Latex.FloatBarrier(file.name)
            title <- paste("Mean squared forecasting errors of ",h,"-step-ahead ",model," forecast of ",variable,": Difference from baseline",sep="")
            cmd <- paste("write.Latex.plot(plot.filename='Diff_MSPE_",variable,"_",model,"_h_",h,".pdf',caption=title,label='",variable,"_h=",h,"_",model,"',width=12,height=7,file.name)",sep="")

            eval(parse(text=cmd))
            # write.Latex.pdfbookmark(3,paste(variable,".h = ",h,".",model,sep=""),paste("Raw.plot",variable,sep=""),file.name)
            write.Latex.pdfbookmark(4,model,paste("diff",variable,h,model,sep=""),file.name)
            
            write.Latex.FloatBarrier(file.name)
        }
    }
}





write.Latex.section("24 month moving average of difference of MSPE from baseline MSPE",file.name)
for (variable in variable.names)
{
    write.Latex.subsection(variable,file.name)
    for (h in horizons)
    {
        write.Latex.subsubsection(paste("Forecast Horizon = ",h,sep=""),file.name)
        if (variable=="PPI")
        {
            baseline <- "Naive"
        }else
        {
            baseline <- "Mean"
        }
        for (model in setdiff(forecast.models,baseline))
        {
            write.Latex.FloatBarrier(file.name)
            title <- paste("24 month moving average of mean squared forecasting errors of ",h,"-step-ahead ",model," forecast of ",variable,": Difference from baseline",sep="")
            cmd <- paste("write.Latex.plot(plot.filename='Diff_MSPE_24_",variable,"_",model,"_h_",h,".pdf',caption=title,label='",variable,"_h=",h,"_",model,"',width=12,height=7,file.name)",sep="")

            eval(parse(text=cmd))
            # write.Latex.pdfbookmark(3,paste(variable,".h = ",h,".",model,sep=""),paste("Raw.plot",variable,sep=""),file.name)
            write.Latex.pdfbookmark(4,model,paste("diff",variable,h,model,"24",sep=""),file.name)
            
            write.Latex.FloatBarrier(file.name)
        }
    }
}

write.Latex.section("Structural Stability Tests",file.name)
write.Latex.subsection("GEFP Tests",file.name)
for (variable in variable.names)
{
    write.Latex.subsubsection(variable,file.name)
    write.Latex.FloatBarrier(file.name)
    write.Latex.pdfbookmark(4,paste("p-values for GEFP Tests: ",variable,sep=""),paste("GEFP.",variable,sep=""),file.name)
    write.Latex.table.file(paste("../../Output/Error_Analysis/GEFP_table_",variable,".tex",sep=""),file.name)
    write.Latex.FloatBarrier(file.name)
}

write.Latex.subsection("supF Tests",file.name)
for (variable in variable.names)
{
    write.Latex.subsubsection(variable,file.name)
    write.Latex.FloatBarrier(file.name)
    write.Latex.pdfbookmark(4,paste("p-values for supF Tests: ",variable,sep=""),paste("supF.",variable,sep=""),file.name)
    write.Latex.table.file(paste("../../Output/Error_Analysis/supF_table_",variable,".tex",sep=""),file.name)
    write.Latex.FloatBarrier(file.name)
}


write.end.Latex.document("apa","referencesHPZ.tex",file.name)

setwd("../Output/Error_Analysis")
system("pdflatex Error_plots.tex")
system("pdflatex Error_plots.tex")
setwd("../../Code")