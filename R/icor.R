#' @import data.table
#' @import tibble
#' @import Hmisc
#' @import Matrix
#' @import ppcor
#" @export
icor = function(data_,verbose=F,pValueMin_=0.05,seuil_=0.3,graph=T,normul=F,signeOK=T,pcorOK=T,criticalSup=T){
    data=data_
    if(!is.matrix(data)) data=as.matrix(data)
    
    
    suppressMessages(library("data.table"))
    #source("./pcor.R")
    suppressMessages(library("tibble"))
   suppressMessages( library("Hmisc"))
    suppressMessages(library("Matrix"))
    seuil=seuil_
    seuilPcor=seuil
    p_valueMin=pValueMin_
    
    #Cor total
    dataCor_=rcorr(data)
    dataCor=dataCor_$r
    dataCorP=dataCor_$P
     if(normul){
         #p_valueMin=1
          #seuil=0
        #signeOK=F
         pcorOK=F
         #criticalSup=F
     }
    if(criticalSup){
        if (seuil_!=0.3) print("WHEN criticalSup, is set, seuil_, is not considerer")
      seuil = icor.critical.r(nrow(data)) 
      seuilPcor = icor.critical.r(nrow(data)-1) 
        cat(paste("seuil: ",seuil,"\nseuilPcor",seuilPcor,"\n"))
      }
    
    dataColsName = colnames(data)
    #On enleve les pas signif
    dataCor[upper.tri(dataCor,diag = T) | (dataCorP > p_valueMin)]=NA
   
    dataCorSum=Matrix::summary(as(dataCor,"dgCMatrix"))
    data.table::setDT(dataCorSum)
    # print(class(dataCorSum))
    #On garde les cor >= seuil
    dataCorT= dataCorSum[!is.na(x) & abs(x)>=seuil,]
    #print("ici")
    nbCol = ncol(data)
    km=1:nbCol
    nbRow=nrow(data)
    nbCorT=nrow(dataCorT)
    signifRel=data.frame()
    PASsignifRel=data.frame()
    keys=rep(NA,nbCol)
    keysName=rep(NA,nbCol)
    keysi=0
    
    #on loop les cor total respectant le seuil et p_value....
    #on regarde si les corPartielle avec chaque var est du meme signe, et sup > seuilPcor, et p_value bien
    for(i in 1:nbCorT){
        elem=dataCorT[i,]
        elemX=elem[,3]
        elemCol=as.integer(elem[,2])
        elemRow=as.integer(elem[,1])
        kmI=km[km!=elemRow & km!=elemCol]
        pb=F
        pcors=list()
        if(verbose)print(elem)
        for(j in kmI){
            if(verbose)print(j)    
            tryCatch({ pcorIJ=pcor.test(data[,elemCol],data[,elemRow],data[,j]) },
                         error = function(err) {
                                print("error")
                                print(err)
                                print(list(data[,elemCol],data[,elemRow],data[,j]))
                         }
                    )
            pcorIJVal=pcorIJ$estimate
            pcorIJP=pcorIJ$p
            pcors=c(pcors,list(pcorIJ))
            if(verbose)print(pcorIJ)
            #check p_value, signe, seuil
            if((pcorOK && (pcorIJP >= p_valueMin || (signeOK && pcorIJVal*elemX < 0) || abs(pcorIJVal) < seuilPcor))){
                if(verbose)print("fini")
                pb=T
                PASsignifRel=rbind(PASsignifRel,
                                      data.frame(i = dataColsName[elemCol],
                                        j = dataColsName[elemRow],
                                       cor=as.numeric(elemX),
                                       cor_pVal=dataCorP[dataColsName[elemCol],dataColsName[elemRow]],
                                       pcor=paste(unlist(lapply(pcors,function(x)paste(x$estimate,"_",x$p))),collapse="|")
                                                 ))
                break
            }
        }
        
        if(!pb){
            if(verbose)print(paste(i,"ok"))
            i_=keys[elemCol]
            if(is.na(i_)){
                keysi=keysi+1
                i_=keysi
                keys[elemCol]=i_
                keysName[i_]=dataColsName[elemCol]
            }
            j_=keys[elemRow]
            if(is.na(j_)){
                keysi=keysi+1
                j_=keysi
                keys[elemRow]=j_
                keysName[j_]=dataColsName[elemRow]
            }
            signifRel=rbind(signifRel,
                            data.frame(i = dataColsName[elemCol],
                                       i_=i_,
                                       j_=j_,
                                       j = dataColsName[elemRow],
                                       cor=as.numeric(elemX),
                                       cor_pVal=dataCorP[dataColsName[elemCol],dataColsName[elemRow]],
                                       pcor=pcors
                                       )
                           )
        }
    }
    if(graph){
        graphO=icor.graph(signifRel,keysName)
    }
    invisible(list(graph=graphO,pasSignif=PASsignifRel,signif=signifRel,dataCorT=dataCorT,dataCorP=dataCorP,dataCor=dataCor,dataCor_=dataCor_$r))
    
}
icor.graph = function(dataSet,names){
    library("visNetwork")
        signif=dataSet
        nodes <- data.frame(id=1:max(signif[,c("i_","j_")]),label=names[!is.na(names)])
    
        edges <- tibble(from=integer(),to=integer(),dashes=logical(),label=numeric(),width=numeric())
        for(i in 1:nrow(signif)){
            elem=signif[i,]
            #print()
             edges=add_row(edges,from=c(elem[,"i_"]),to=c(elem[,"j_"]),dashes=c(elem[,"cor"]<0),label=round(elem[,"cor"],2),width=exp(abs(elem[,"cor"])*2.5))
            
        }
        return(visNetwork(nodes,edges,width = "100%"))
}
#get minimun corr pour etre signif en fonction de n et alpha
icor.critical.r <- function( n, alpha = .05 ) {
  df <- n - 2 # car estimationde muX et muY
  critical.t <- qt( alpha/2, df, lower.tail = F ) # pour avoir le t, correpondant a (a gauche et a droite, alpha/2)
  critical.r <-  icor.studentToCorr(critical.t,df) # pour passer de t à r # critical.t / sqrt( (critical.t^2) + df )
  return( critical.r )
}

#table to html table                                     
icor.dfToHTML = function(df){
    suppressMessages(library("googleVis"))
    IRdisplay::display_html(icor.repr_html.gvis(gvisTable(df,options=list(page='enable'))))
}
icor.repr_text.gvis <- function(obj, ...) 'gvis cannot be represented in plain text (need html)'

icor.repr_html.gvis <- function(obj, ...){
    if (!requireNamespace('googleVis', quietly = TRUE))
        stop('repr_html.gvis called without loadable googleVis')

    htmlfile <- tempfile(fileext = '.html')
    on.exit(unlink(htmlfile))

    print(obj, tag='chart', file=htmlfile)
    readChar(htmlfile, file.info(htmlfile)$size)
}
icor.repr_latex.gvis <- function(obj, ...) NULL
icor.repr_markdown.gvis <- function(obj, ...) NULL
                                                  
icor.studentToCorr = function(t,dll){
    t / sqrt( (t^2) + dll )
}
icor.corrToStudent = function(r,dll){
    r * sqrt(dll/(1-(r^2)))
}
                                                  blib=base::library
library = function(pkg){
    if(nchar(pkg) > 4) {
        if(substr(pkg,1,4)=="git:"){
            pkg=substr(pkg,5,nchar(pkg))
            devtools::install_github(pkg)
            tg(blib(basename(pkg),character.only = TRUE, quietly = TRUE))
            return(NULL)
        }
    }
    if(!require(pkg,character.only = TRUE)){
        install.packages(pkg)
    }
    tg(blib(pkg,character.only = TRUE, quietly = TRUE))
}
tg = function(f){
    suppressMessages(suppressPackageStartupMessages(suppressWarnings(f)))
}
Lib <- function(){
    x <- list()
    class(x) <- "Lib"
    return(x)
}
print.Lib <- function(e1,e2=NULL) {
  return("OK")
}

is.Lib <- function(obj) {
  return(class(obj) == "Lib")
}

plus <- function(e1, e2){
    print(e1)
    tg(library(e2))
    return(Lib())
     #NextMethod(e1,e2)
}
lib = Lib()



plotWH= function(w,h)options(repr.plot.width=w,repr.plot.height=h)
showWarning=function(f)options(warn=ifelse(f,0,-1))
lapplys = function(data,...){a=data;for(i in list(...)){ a=i(a)};return(a)}
startsWithGet=function(tab,pat)tab[startsWith(tab,pat)]
`%.%` = function(a,b){
    paste0(a,b)
}                                                 
update = function(){
    devtools::install_github("luluperet/icor")
 }
embed = function(x, height) {
    library(IRdisplay)
    tmp = tempfile(fileext = ".html")
    htmlwidgets::saveWidget(x, tmp)
    rawHTML = base64enc::dataURI(mime = "text/html;charset=utf-8", file = tmp)
    display_html(paste("<iframe src=", rawHTML, "width=100% height=", height, "id=","igraph", "scrolling=","yes","seamless=","seamless", "frameBorder=","0","></iframe>", sep = "\""))
    unlink(tmp)
}
                                                  
                                                  
EQ=function(a,b,ops="+"){
  x <- list()
  x$ops=ops
  x$a=a
  x$b=b
    class(x) <- "EQ"
    return(x)
}

sup <- function(e1, e2){
    #print(class(e1))
    #print(class(e2))
    
    return(do.call(e1$ops,list(e1$a > e2,e1$b > e2)))
     #NextMethod(e1,e2)
}
eq <- function(e1, e2){
    return(do.call(e1$ops,list(e1$a == e2,e1$b == e2)))
     #NextMethod(e1,e2)
}
supEq <- function(e1, e2){
    return(do.call(e1$ops,list(e1$a >= e2,e1$b >= e2)))
     #NextMethod(e1,e2)
}
infEq <- function(e1, e2){
    return(do.call(e1$ops,list(e1$a <= e2,e1$b <= e2)))
     #NextMethod(e1,e2)
} 
inf <- function(e1, e2){
    #print(class(e1))
    #print(class(e2))
    
    return(do.call(e1$ops,list(e1$a < e2,e1$b < e2)))
     #NextMethod(e1,e2)
}
`%&%` = function(rhs,lhs){
  #print(rhs)
   #print(lhs)
   return(EQ(rhs,lhs,"&"))
}
`%|%` = function(rhs,lhs){
  #print(rhs)
   #print(lhs)
   return(EQ(rhs,lhs,"|"))
}
`%&&%` = function(rhs,lhs){
  #print(rhs)
   #print(lhs)
   return(EQ(rhs,lhs,"&&"))
}
`%||%` = function(rhs,lhs){
  #print(rhs)
   #print(lhs)
   return(EQ(rhs,lhs,"||"))
}
                                                  
.onLoad <- function(...) {
  registerS3method("+", "Lib", plus)
  registerS3method(">", "EQ", sup)
  registerS3method(">=", "EQ", supEq)
    registerS3method("==", "EQ", eq)
    registerS3method("<", "EQ", inf)
    registerS3method("<=", "EQ", infEq)
}
                                                  
each = function(lst,fn){
  sapply(lst,fn)
}
`%each%` = each
`%map%` = each
