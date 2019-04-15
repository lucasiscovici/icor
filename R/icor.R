#' @import data.table
#' @import tibble
#' @import Hmisc
#' @import Matrix
#' @import ppcor
#' @import tidyverse
#" @export
.onAttach <- function(...) {
 suppressPackageStartupMessages({
 library("magrittr")
 library("tidyverse")
 library("wrapr")
 library("rlist")
  library("data.table")
 })
#h <- sapply(seq(1, nchar(templateLucas_plan), by=2), function(x) substr(templateLucas_plan, x, x+1))
#templateLucas_plan_str=rawToChar(as.raw(strtoi(h, 16L)))
}
templateLucas_plan="7467203D2066756E6374696F6E2866297B0A2020202073757070726573734D657373616765732873757070726573735061636B616765537461727475704D657373616765732873757070726573735761726E696E677328662929290A7D0A626C69623D626173653A3A6C6962726172790A6C696272617279203D2066756E6374696F6E28706B672C2E2E2E297B0A20202020706B67673D706B670A202020202069662869732E6C69737428706B676729207C7C206C656E67746828706B6729203E2031297B0A202020202020202020206C6170706C7928706B67672C6C696272617279290A2020202020207D656C73657B0A202020206966286E6368617228706B6729203E203429207B0A202020202020202069662873756273747228706B672C312C34293D3D226769743A22297B0A202020202020202020202020706B673D73756273747228706B672C352C6E6368617228706B6729290A202020202020202020202020646576746F6F6C733A3A696E7374616C6C5F67697468756228706B672C2E2E2E290A202020202020202020202020746728626C696228626173656E616D6528706B67292C6368617261637465722E6F6E6C79203D20545255452C2071756965746C79203D205452554529290A20202020202020202020202072657475726E284E554C4C290A20202020202020207D0A202020207D0A20202020696628217265717569726528706B67672C6368617261637465722E6F6E6C79203D205452554529297B0A2020202020202020696E7374616C6C2E7061636B6167657328706B67672C2E2E2E290A202020207D0A20202020746728626C696228706B67672C6368617261637465722E6F6E6C79203D20545255452C2071756965746C79203D20545255452C2E2E2E29290A2020202020207D0A7D0A0A6E616D653D227B7B6E616D657D7D220A0A6C756361735F706C616E5F696D706F7274203D2066756E6374696F6E286E616D65297B0A202023613D6E616D650A20206E616D3D70617374653028226C75636173506C616E5F222C6E616D65290A202065723D706173746530286E616D2C222E7461722E677A22290A202073797374656D2870617374653028226D6B646972202D7020222C6E616D29290A202073797374656D287061737465302822637020222C65722C2220222C6E616D2C222F2229290A202073797374656D287061737465302822636420222C6E616D2C2220262620746172202D787A6620222C657229290A20206F3D6C6F616428706173746530286E616D2C222F222C6E616D2C222E52446174612229290A202023756E7461722870617374653028226C75636173506C616E5F222C6E616D652C222E7461722E677A22292C2066696C6573203D206328645B5B325D5D2C657229290A2020613D676574286F290A20206C6170706C7928615B5B345D5D2C66756E6374696F6E2869296C69627261727928692C6C69622E6C6F63203D206765747764282929290A20206C6170706C79286E616D657328615B5B315D5D292C66756E6374696F6E28692961737369676E28692C20615B5B315D5D5B5B695D5D2C20656E766972203D20676C6F62616C656E76282929290A2020610A7D0A613D6C756361735F706C616E5F696D706F7274286E616D65290A7265703D6C617A795F6576616C28615B5B335D5D290A73617665287265702C66696C653D70617374653028226C75636173506C616E5F222C6E616D652C222F222C226C75636173506C616E5F222C6E616D652C225F7265702E5244617461222929"
h <- sapply(seq(1, nchar(templateLucas_plan), by=2), function(x) substr(templateLucas_plan, x, x+1))
templateLucas_plan_str=rawToChar(as.raw(strtoi(h, 16L)))
 loadPkgUsefull=function(){
  suppressPackageStartupMessages({
 library("purrr")
 library("Hmisc")
  library("Matrix")
    library("MASS")
 library("dplyr")
 library("withr") 
 library("R6") 
 library("future")
      })                    
 
 }

#a finir, avec individus en col, et interaction entre col
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
dfToHTML = function(df){
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
library = function(pkg,...){
    pkgg=pkg
     if(is.list(pkgg) || length(pkg) > 1){
          lapply(pkgg,library)
      }else{
    if(nchar(pkg) > 4) {
        if(substr(pkg,1,4)=="git:"){
            pkg=substr(pkg,5,nchar(pkg))
            devtools::install_github(pkg,...)
            tg(blib(basename(pkg),character.only = TRUE, quietly = TRUE))
            return(NULL)
        }
    }
    if(!require(pkgg,character.only = TRUE)){
        install.packages(pkgg,...)
    }
    tg(blib(pkgg,character.only = TRUE, quietly = TRUE,...))
      }
}
load=library                                            
tg = function(f){
    suppressMessages(suppressPackageStartupMessages(suppressWarnings(f)))
}
Lib <- function(){
    x <- list()
    class(x) <- "Lib"
    return(x)
}
                                                  
by = function(right,left){
    rr=right
  #if(is.list(right))rr=rr[[names(rr)[1]]]
  return(base::split(rr,left))
}
`%by%` = by 
 byGrp = function(right,left){
    rr=right
  #if(is.list(right))rr=rr[[names(rr)[1]]]
  graphCatCon(rr,left)
  return(by(rr,left))
}
`%byGraph%` = byGrp
                                                  
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


plotWHDefault=function(w,h){list(repr.plot.width=w,repr.plot.height=h)}
plotWH.= function(w=NULL,h=NULL){
 ww=if(is.null(w))defaultW() else w
 hh=if(is.null(h))defaultH() else h
 options(repr.plot.width=ww,repr.plot.height=hh)
 }
plotWH = function(w=NULL,h=NULL){
 saved=plotWH.(w,h)
 return(invisible(function(){
   options(saved)
 }))
 }
defaultW=.%>%{getOption("repr.plot.width")}
defaultH=.%>%{getOption("repr.plot.height")}
                                                  
with_plotWH = function(w,h,...)with_options(plotWHDefault(w,h),...)
                                                  
                                                  
show_hideWarning.=function(f)options(warn=ifelse(f,0,-1))
showWarning=function()show_hideWarning.(TRUE)
hideWarning=function()show_hideWarning.(FALSE)
toggleWarning=function()show_hideWarning.(ifelse(getOption("warn")==0,FALSE,TRUE))
lapplys = function(data,...){a=data;for(i in list(...)){ a=i(a)};return(a)}
startsWithFromList=function(tab,pat)tab[startsWith(tab,pat)]
`%.%` = function(a,b){
    paste0(a,b)
}                                                 
update = function(upgrade=F,...){
    devtools::install_github("luluperet/icor",upgrade=upgrade,...)
 }
embed = function (x, height="100%",width="100%") 
{
    tg(library("IRdisplay"))
    tmp = tempfile(fileext = ".html")
    htmlwidgets::saveWidget(x, tmp)
    rawHTML = base64enc::dataURI(mime = "text/html;charset=utf-8", 
        file = tmp)
    display_html(paste("<iframe src=", rawHTML, "width=",width,"height=", 
        height, "id=", "igraph", "scrolling=", "yes", "seamless=", 
        "seamless", "frameBorder=", "0", "></iframe>", sep = "\""))
    unlink(tmp)
}
embedDT=function(dt,height="100%",width="100%",...){
 tg(lib + "DT")
    embed(DT::datatable(dt,...),height,width)
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
   
       registerS3method("-", "StrCls", minusStrCls)

}
                                                  
   
detachFast = function(name){
    detach("package:"%.%name, unload=TRUE,character.only=TRUE)
 }  
#L = function(le,xc){
#  x <- list()
#  x$list = le
#  x$c=xc
#  class(x) <- "L"
#  return(x)
#}
#l=L(list(),1L)
#is.L <- function(obj) {
#  return(class(obj) == "L")
#}
#plusL = function(left,rigth){
  
#  leftt=ifelse(is.L(left),left$list,ifelse(is.list(left),left,list(left)))
  #append(leftt,rigth)
  
#  leftt[[left$c]]=rigth
#  return(L(leftt,left$c+1L))
#}       
#print.L = function(obj){
#    obj$list  
##  }
graphCatCon <- function(x, gpe) { 
    stripchart(x ~ gpe) 
    points(tapply(x, gpe, mean), 1:length(levels(gpe)), col = "red", pch = 19, cex = 1.5) 
    abline(v = mean(x), lty = 2) 
    moyennes <- tapply(x, gpe, mean) 
    traitnf <- function(n) segments(moyennes[n], n, mean(x), n, col = "blue", lwd = 2) 
    sapply(1:length(levels(gpe)), traitnf) 
}
#http://www.pacea.u-bordeaux1.fr/IMG/pdf/rapport_correl.pdf
vartot <- function(x) {
    sum((x - mean(x))^2)/length(x)
}
varinter <- function(x, gpe) {
    moyennes <- tapply(x, gpe, mean)
    effectifs <- tapply(x, gpe, length)
    (sum(effectifs * (moyennes - mean(x))^2))/length(x)
}
signif.corrCatCon = function(corr,cat,con){
    n=length(con)
    p=length(levels(cat))
    #n ind, p groupes
    K= (corr*(n-p))/((p-1)*(1-corr))
    quantilF=qf(0.05,p-1,n-p,lower.tail=FALSE)
    pval=pf(K,p-1,n-p, lower.tail = FALSE)
    list(K=K,quantileF=quantilF,reject=quantilF<K,pval=pval)
}    
corrCatCon = function(cat,con,signif=FALSE){
    res=varinter(con, cat)/vartot(con)
    if(signif)return(list(corr=res,signif=signif.corrCatCon(res,cat,con)))
    else res
}

`%filter%` = function(left,right){
    left %>% {Filter(right,.)}
}
                                                  
filterCol   = function(a,func){
  a %>% Filter(func,.)
}
numericCol = function(a,b=NULL){
  a %>% filterCol(is.numeric)
}
notNumericCol = function(a,b=NULL){
  a %>% filterCol(. %>% {!is.numeric(.)})
}
catCol = function(a,b=NULL){
  a %>% filterCol(is.factor)
}
notCatCol = function(a,b=NULL){
  a %>% filterCol(. %>% {!is.factor(.)})
}                                   
`%numericCol%`= numericCol
`%!numericCol%`= notNumericCol
`%catCol%`= catCol  
                                    
`%!catCol%`= notCatCol
                                

ancientBail = function(ll,rr){
  listElems=if(is.list(ll) && inherits(ll,"Args")) ll
            else list(ll)
  class(listElems) = append(class(listElems),"Args")
  if(substitute(rr)==".") return(listElems)
  listElems %<>% list.append(.,rr)
    class(listElems) = append(class(listElems),"Args")
  return(listElems)
}

`%...>%` = function(ll,rr){
  if(is.list(ll)) do.call(rr,ll)
  else do.call(rr,list(ll))
}
 `%..._>%` = function(ll,rr){
  listll = if(is.list(ll)) ll else list(ll)
  names(listll)=NULL
  do.call(rr,listll)
}

 reloadIcor = function(){
    detachFast("icor")
    blib("icor") 
  }
                                    
  updateReloadIcor=function(...){
    update(...)
      reloadIcor()
   }
ll=function(...)l.(l.(...))                               
mapFns = function(left,right){
     fns=right
  datas=left
  if(!is.list(datas) && !(length(datas)>1) ) datas=list(datas)
  else if(!is.list(datas) ) datas=as.list(datas)
      
    invoke_map(lapply(right,as_mapper),left)
    }
                                  
`%mapFns%`=mapFns
toDF.=function(left,rr=NULL){
  if(!is.null(rr) && substitute(rr)==".")rr=NULL
  right=rr
  if(!is.null(right) && substitute(rr)==".")right=NULL
  if(is.matrix(left))return(if(!is.null(right) && (rigth=="t" || substitute(rr)=="t")) t(as.data.frame(left))else as.data.frame(left) )
  if(!is.null(right) && (rigth=="t" || substitute(rr)=="t"))
    return(toDFt(left))
  df=data.frame(matrix(unlist(left),nrow=length(left), byrow=T),stringsAsFactors=FALSE)
  row.names(df)=names(left)
  return(df)
}
toDF=function(left,rr=NULL){
  df=data.frame(matrix(unlist(left),ncol=length(left), byrow=F),stringsAsFactors=FALSE)
  colnames(df)=names(left)
  return(t(df))
}
`%toDF%`=toDF
toDFt=function(left,right=NULL){
  df=data.frame(matrix(unlist(left),ncol=length(left), byrow=F),stringsAsFactors=FALSE)
  colnames(df)=names(left)
  return(df)
}
`%toDFt%` = toDFt

                            
add_row_with_name = function(data,name="X",...){
  d=nrow(data)
  data2=data
  dd=list(...)
  if(is.list(dd[[1]]))dd=dd[[1]]
  data2=rbind(data2,dd)
  rownamess=rownames(data2)
  rownamess[d+1]=name
  rownames(data2)=rownamess
  return(data2)
}

int.hist = function(x,ylab="Frequency",perc=FALSE,...) {
  tb=table(factor(x,levels=min(x):max(x)))
  ptb=prop.table(tb)
  ims=function(i)if(perc){100*i}else{i}
  percc=if(perc)"%"else ""
  ll=c(min(x),max(x))
  names(tb) = lapply(1:length(ll),function(l)ll[l]%.%"("%.%ims(ptb[l])%.%percc%.%")")
  barplot(tb,space=0,ylab=ylab,...)
}
                     
Aleatoire <- R6Class("Aleatoire",
                     public = list(
                       initialize = function(a=16807,
                                             b=0,
                                             m=0x7FFFFFFF) {
                         private$m_a=a
                         private$m_b=b
                         private$m_m=m
                         private$m_nombre=as.numeric(Sys.time());
                       },
                       generer = function(min=0.0,max=private$m_m,double=FALSE) {
                         private$m_nombre = (private$m_a*private$m_nombre + private$m_b) %% private$m_m;
                         m_nombre2=if(double){private$m_nombre/(as.double(0x7FFFFFFF))*(max-min)+min} else {as.integer(private$m_nombre)%%as.integer(max-min) + min}
                         m_nombre2
                       },
                       unif = function(min=0,max=1){
                         U=self$generer(0,1,double=TRUE)
                         (max-min)*U + min
                       },
                       exp= function(lambda){
                         #https://stats.stackexchange.com/questions/234544/from-uniform-distribution-to-exponential-distribution-and-vice-versa
                         U = self$unif();
                         -(1/lambda)*log(U)
                       },
                       poisson =function(lambda=1){
                         U = self$unif();
                         -log(U)/lambda
                       },
                       gamma = function(a,b=1){
                         if(b==1) return(private$gamma_Without_b(a))
                         N=1
                         #http://www.douillet.info/~douillet/simul/Simulations.pdf
                         if(is.integer(a) && a > 1 ) {
                           k = 0 
                           A = 0
                           w = sqrt(2 * a - 1) 
                           while(k < N){
                             u = pi * (self$unif() - 1/2) 
                             y = tan(u)
                             x = w * y + a - 1
                             if (x> 0 && self$unif() < (1 + y^2)*exp((a - 1)* log(x/(a - 1)) - w * y)){
                               k = k + 1
                               A=x
                             }
                           }
                           return(A)
                         }else{
                           #https://books.google.fr/books?id=dogHCAAAQBAJ&lpg=PA252&ots=tfg2xwcUt7&hl=fr&pg=PA194#v=onepage&q&f=false
                           k = 0
                           A = 0
                           b = 1 + a / exp (-1)
                           while( k < N) {
                             p = b * self$unif() 
                             v = self$unif()
                             if (p > 1){
                               x = -log((b - p) /a)
                               if(v <= (x^(a - 1))) {
                                 k = k + 1
                                 A = x
                               }
                             }else {
                               x = p^(1/(a))
                               if (v < exp(-x)){ 
                                 k = k + 1 
                                 A= x
                               }
                             }
                           }
                           A
                         }
                       },
                       normal=function(m=0,sigma=1,nb=1){
                         if(nb==1){
                           x=self$unif();
                           return ((self$normal(nb=2)[1])*sigma+m)
                         }else if(nb==2){
                           x=self$unif();
                           x2=self$unif();
                           return(c(sqrt(-2*log(x))*cos(2*pi*x2),sqrt(-2*log(x))*sin(2*pi*x2)))
                         }
                       },
                       
                       loi_discrete=function(loi,taille=NULL){
                         if (is.null(taille)) taille=length(loi)
                         i=1;
                         x=self$unif();
                         somme=loi[i];
                         
                         while(somme < x && i < taille){
                           somme = somme+ loi[i+1];
                           i=i+1
                         }
                         return(i);
                       },
                       bernouli = function(p){
                         self$loi_discrete(c(1-p,p))-1
                       },
                       binomial=function(n,p){
                         U = matrix(lapply(1:n,.%>%{self$unif()}),nrow=1)
                         Y = (U < p);
                         X = sum(Y )
                         X
                       },
                       geom =function(p){
                         X = 1;
                         U = self$unif();
                 
                                 while(U > p){
                           X = X + 1; 
                           U = self$unif();
                         }
                         X
                       }
                       
                           ),
                           private = list(
                             m_a=integer(),
                             m_b=integer(),
                             m_m=integer(),
                             m_nombre=integer(),
                             gamma_Without_b = function(a) { 
                               #loi expo -> F(x) =1-exp(x)
                               #r=1-exp(-x)
                               #-ln(r)
                              
                               return(-log(prod(sapply(1:a,self$unif))))
                             }
                           )
                             ) 


      dfRowToList = . %>% t %>% as.data.frame %>% as.list
      densityPlt= . %>% density %>% plot
      densityLines= function(a,...) lines(density(a),...)
      
    listToDotsFn =function(liste,fn){
      do.call(fn, liste)
    }
    listToDotsFn_ =function(liste,fn){
      names(liste)=NULL
      do.call(fn, liste)
    }
    `%listToDotsFn_%` = listToDotsFn_
    `%listToDotsFn%` = listToDotsFn
    test_same_distrib = function(sample1,sample2){
      return(ks.test(sample1,sample2)$p)
    }
    capturePrint = function(w){
      capture.output(print(w))
    }
                      curry = function(...){
  a=match.call()
  parent <- parent.frame()
  env    <- new.env(parent = parent)
  a2=a[[-1]]
  argss=as.list(a2)
  function_name = argss[[1]]
  argsFunc=argss[-1]
  #print(argsFunc)
  e=eval(as.call(c(purrr::partial,function_name,argsFunc)),env,env)
  e
}
    runFnXtimes = function(fn,Xtimes=100){
      aa=match.call()
      aa2=aa[[2]]
      #Xti=Xtimes
      #print(capturePrint(aa2))
      if(str_detect(toString(aa2),"^[0-9]+$")){
        aa2=aa[[3]]
        Xti=fn
        #print("d")
      }else{
        Xti=Xtimes
      }
      parent <- parent.frame()
      env    <- new.env(parent = parent)
      dfn=aa2
      p=splitArgsl_(dfn,env,parent)
      if(is.null(p)) p = fn
      .p.=p
        l(1:Xti) %each% .p.
    }
    `%Xtimes%` = runFnXtimes
    eachFn = function(a,b,dontShow=F){
      aa=match.call()
      parent <- parent.frame()
      env    <- new.env(parent = parent)
      bb=splitArgsl_(aa[[3]],env,parent)
      listNames=names(a)
      pp=list()
      for(i in 1:length(a)){
        k=listNames[i]
        if(is.null(listNames[i]) || listNames[i]==""){
          k=i
        }
        aq= do.call(bb[[i]],list(a[[k]]))
       pp=rlist::list.append(pp,aq)
      }
     if(!dontShow)pp
    }
    `%eachFn%` = eachFn
   `%eachFnTg%` = function(a,b){
    
      do.call(eachFn,list(substitute(a),substitute(b),dontShow=T)) 
    }
    
    is.doubledot=function(str){
      if(length(str)==3 && str[[1]] %in% c("$","@"))str=str[[3]]
      str_detect(str,"^\\..+\\.$")
    }
    
    
    eachRowCol = function(ll,rr,INDEX){
      n=if(INDEX==2)colnames(ll)else rownames(ll)
      lapply(1:length(n),function(i){
                                     if(c("...",".y") %in% {args(rr) %>% as.list %>% names} %>% any) rr(if(INDEX==2)ll[,i]else ll[i,],.y=n[i])
                                     else rr(if(INDEX==2)ll[,i]else ll[i,])
                                    }
            )
    }
    eachCol = function(ll,rr){
      eachRowCol(ll,rr,2)
    }
    eachRow = function(ll,rr){
      eachRowCol(ll,rr,1)
    }
    `%eachCol%` = eachCol
    `%eachRow%` = eachRow
     
      eachRowCol2 = function(ll,rr,INDEX){
      n=if(INDEX==2)colnames(ll)else rownames(ll)
      lapply(1:length(n),function(i){
           z= if(c("...",".y") %in% {args(rr) %>% as.list %>% names} %>% any) rr(if(INDEX==2)ll[,i]else ll[i,],.y=n[i]) else rr(if(INDEX==2)ll[,i]else ll[i,])
                                  l(.(n[i])%:=%list(.(z))) %getElem% 1
                                  # wrapr::`:=`(bquote(.(n[i])) ,bquote(.(z)))
                                    }
            )
    }
             `%eachCol2%` = curry(eachRowCol2(INDEX=2))
             `%eachRow2%` = curry(eachRowCol2(INDEX=1))
                     
    eachRowCol. = function(ll,rr,INDEX){
       apply(ll,INDEX,rr)
    }
    `%eachRowCol.%` = eachRowCol.
    
    test_normal = function(sample1){
      return(ks.test(sample1,"pnorm",mean=0,sd=1)$p)
    }
    
  
    map = function(lst,fn,env=NULL){
      d=match.call()
      if (is.null(env)){ 
        parent <- parent.frame()
        env    <- new.env(parent = parent)
      }
      dfn=d[[3L]]
     
     if (is_funexpr(dfn)){
       #pls=as.list(d)
        p=splitArgs(list(dfn),env,parent,withDotP=T)
       #p=l_(dfn,env=env,parent=parent)
       }else{
       p=splitArgsl_(dfn,env,parent)
      }
      
      if(is.null(p)) p = eval(fn,env,env)
      fns=p
      datas=lst
      if(!is.list(datas) && !(length(datas)>1) ) datas=list(datas)
      else if(!is.list(datas) ) datas=as.list(datas)
      
      if(!is.list(fns)) fns=list(fns)
      lapply(datas,function(data)lapply(fns,function(fn)fn(data)))
    }
    
    `%map%` = map
    
    splitArgsl_=function(dfn,env,parent){
      ae=callToString(dfn)
      if(is.doubledot(ae))return(NULL)
      if(!stringr::str_detect(ae,"^l__?_?.*$")){
        p=splitArgs(list(dfn),env,parent)
      }else{
        p=eval(dfn)
      }
      return(p)
    }
    each = function(lst,fn,env=NULL){
      d=match.call()
      if (is.null(env)){ 
        parent <- parent.frame()
        env    <- new.env(parent = parent)
      }
      dfn=d[[3L]]
      if (is_funexpr(dfn)){
       pls=as.list(d)
       p=splitArgs(list(pls[[3L]]),env,parent,withDotP=F)
       #p=l_(dfn,env=env,parent=parent)
       }else{
        p=splitArgsl_(dfn,env,parent)
       }
      
      
      if(is.null(p)) p = eval(fn,env,env)
      fns=p
      datas=lst
      if(!is.list(datas) && !(length(datas)>1) ) datas=list(datas)
      else if(!is.list(datas) ) datas=as.list(datas)
      
      if(!is.list(fns)) fns=list(fns)
      sapply(datas,function(data)sapply(fns,function(fn)fn(data)))
    } 
    
    `%each%` = each
    
    callToString  = function(call){
      as.character(lazyeval::as_name(call))
    }
    splitArgs = function(calls,env,parent,withDotP=F){
      rhss   <- list()
      i <- 1L 
      stop=F
      namesL=names(calls)
      if(is.null(namesL))namesL=1:length(calls)
      for(ii in 1:length(calls)) {
        rhs=calls[[ii]]
        formula=F
        if (is_parenthesized(rhs))
          rhs <- eval(rhs, env, env)
        rhs <- 
          if (is_funexpr(rhs) || withDotP)
            rhs
        else if(rlang::is_formula(rhs)){
          formula=T
          as.formula(rhs)
        }
        else if (is_function(rhs) || is_colexpr(rhs))
          prepare_function(rhs)
        else if (is_first(rhs)) 
          prepare_first(rhs)
        else {
          stop=F
          rhs
        }
        if(stop)break
        
        if(formula==F){
          rhs=lazyeval::f_capture(rhs)
        }
        rhss[[namesL[i]]]=purrr::as_mapper(rhs)
        i = i + 1L
      }
      if(stop)return(NULL)
      return(rhss)
    }
    
                                        
`%call%` = function(fn,args){
  return(do.call(fn,args))
}
removeParamsInCall=function(params,calls){
  a=calls
  al=as.list(a)
  #print(al)
  anames=names(al)
  #print(anames)
  if (!is.null(anames)){
  for(i in 1:length(anames)){
    if(anames[i] %in% params){
      al=al[-i]
    }
  }
  }
  al
}

lintern=function(...,x=NULL,n=NULL,i=1){
  env=new.env(parent=parent.frame())
  a=match.call()
  fn=a[[1]]
  fns=lazyeval::as_name(fn)
  amoins1=a[-1]
  amoins1=removeParamsInCall(c("x","n"),amoins1)
  #print(fns)
  what=NULL
  if(stringr::str_detect(fns,"^l1.*$"))
    what=1
  else if(stringr::str_detect(fns,"^lx.*$"))
    what=x
  else if(stringr::str_detect(fns,"^ln.*$"))
    what=i:n
  #print(fns)

   lo=as.list(amoins1)
   if(stringr::str_detect(fns,"^l[1xn]?_{0,4}\\.$")){
    lo=list.append(lo,noQuote=TRUE)
    fn.=stringr::str_replace(fns,"\\.","")
    if(is.null(what)){
       fn=fn.
     }else{
      fns=fn. 
     }
    }
   
   if(!is.null(what)){
    fn=stringr::str_replace(fns,"[1xn]","")
    #fn=lazyeval::as_call(fn)
  }
  #print(fn)
  cc=do.call(fn,lo,envir=parent.frame())
             if(!is.null(what)){
                if(length(what)>1)
                  return(cc%getElems%what)
               else
                 return(cc%getElem%what)
             }
             return(cc)
} 
                                        
#l = lintern
ln = lintern
lx = lintern
l1 = lintern
#l_ = lintern
ln_ = lintern
lx_ = lintern
l1_ = lintern
#l__ = lintern
ln__ = lintern
lx__ = lintern
l1__ = lintern
#l___ = lintern
ln___ = lintern
lx___ = lintern
l1___ = lintern
#l____ = lintern
ln____ = lintern
lx____ = lintern
l1____ = lintern
ln. = lintern
lx. = lintern
l1. = lintern
ln_. = lintern
lx_. = lintern
l1_. = lintern
ln__. = lintern
lx__. = lintern
l1__. = lintern
ln___. = lintern
lx___. = lintern
l1___. = lintern
ln____. = lintern
lx____. = lintern
l1____. = lintern
                                        
#l.=list
                                        
l<- function(...,noQuote=FALSE) {
    .env=parent.frame()
 u=if(noQuote) list(...) else eval(do.call(bquote, list(substitute(list(...)),
                                                   where = .env),
                                     envir = .env),.env,.env)
  u
}
                                        
l_ = function(...,.env = parent.frame(),noQuote=FALSE){
      calls  <- match.call()
      parent <- parent.frame()
      
      env    <- new.env(parent = parent)
      
      if(length(calls)<2){
        return(list())
      }
      .call2 <- if(noQuote) calls else do.call(bquote, list(substitute(list(...)),
                                                   where = .env),
                                     envir = .env)
      pls=as.list(.call2)
      splitArgs(pls[-1],env,parent)
    }
    l__ = function(...,noQuote=FALSE,.env = parent.frame()){
      calls  <- match.call()
      parent <- parent.frame()
      
      env    <- new.env(parent = parent)
      
      if(length(calls)<2){
        return(list())
      }
     .call2 <- if(noQuote) calls else do.call(bquote, list(substitute(list(...)),
                                                   where = .env),
                                     envir = .env)
      pls=as.list(.call2)
      splitArgs(pls[-1],env,parent,T)
    }
    l___=function(...,noQuote=FALSE){
        a=match.call()
       lapply(a[-1],function(a){eval(call("curry",a))})
     }
    l____=function(...,noQuote=FALSE){
         do.call(l__,list(substitute(...)))
     }
                                        
    l. = lintern                                        
    l_. = lintern                                        
    l__. = lintern                                        
    l___. = lintern                                        
    l____. = lintern                                    
                                        
    prepare_function <- function(f)
    {
      as.call(list(f, quote(.)))
    }
    is_placeholder <- function(symbol)
    {
      identical(symbol, quote(.))
    }
    
    is_function <- function(expr)
    {
      is.symbol(expr) || is.function(expr)
    }
    is_colexpr <- function(expr)
    {
      is.call(expr) &&
        (identical(expr[[1L]], quote(`::`)) || identical(expr[[1L]], quote(`:::`)))
    }
    is_funexpr <- function(expr)
    {
      is.call(expr) && identical(expr[[1L]], quote(`{`))
    }
    is_parenthesized <- function(expr)
    {
      is.call(expr) && identical(expr[[1L]], quote(`(`))
    }
    
    prepare_first <- function(expr)
    {
      as.call(c(expr[[1L]], quote(.), as.list(expr[-1L])))
      
    }
    is_first <- function(expr)
    {
      !any(vapply(expr[-1L], identical, logical(1L), quote(.)))
    }
      
    getElem = function(datas,row){
     if(length(row) == 1) row=l(row)
     f=datas
     for(i in row){
      
      f=f[[i]]
     }
     return(f)
    }
  getElem2 = function(datas,row){
  if(length(row) == 1) row=c(row)
  f=datas
  for(id in row){
    i=id
    if (inherits(i,"StrCls")) {
      i=if(inherits(i,"StrCls"))i$str else i
      if(length(i) > 1) {
        ind=lapply(i,function(e){
          last=length(f)  
          nn=stringi::stri_match(e,regex = "[0-9]")
          #print(nn)
          if(length(nn)>0 && !is.na(nn[[1]])){
            last=last-as.numeric(nn)
          }
        })
        ind=as.numeric(ind)
        f=f[ind]
      }else{
        last=length(f)  
        nn=stringi::stri_match(i,regex="[0-9]")
        if(length(nn)>0 && !is.na(nn[[1]])){
          last=last-as.numeric(nn)
        }
        f=f[as.numeric(last)]
      }

    }else if(stringi::stri_detect_regex(i,"^[0-9]+$")){
      f=f[[i]]
    }else if(length(i) > 1 || stringi::stri_detect_regex(i,"^-[0-9]+$")){
      f=f[i]
    }
  }
  return(f)
}
"%getElem2%"=getElem2
 `%getElem%` = getElem
    smth = function(...){
      a=list(...)
      str(a)
    }
                                        

                                        StrCls=function(a=""){
  d=list()
  d$str=a
  class(d) = append(class(d),"StrCls")
  d
}
 .last=StrCls("last")

`%.=%` = function(a,b){
  aa=match.call()
  par=parent.frame()
  aa2=aa%getElem%2
  aa3=aa%getElem%3
  #print(l("<-",as.character(aa2),as.character(aa3)))
  eval(call("<-",as.character(aa2),as.character(a)%.%as.character(aa3)),par,par)
}

StrCls.print=function(l){
  l$str
}
minusStrCls=function(strcls,l){
  strcls$str =  strcls%.%"-"%.%l
  return(strcls)
}
getElems = function(datas,row){
  if(length(row) == 1) row=c(row)
  f=datas
  res=list()
  for(i in row){
    f=getElem(datas,i)
    res=append(res,l(f))
  }
  return(res)
}
"%getElems%"=getElems      
   
   getElems2 = function(datas,row){
  if(length(row) == 1) row=c(row)
  f=datas
  res=list()
  for(i in row){
    f=getElem2(datas,i)
    res=append(res,l(f))
  }
  return(res)
}
"%getElems2%"=getElems2  
   
 captureListRegex.=function(stri){
  str_extract_all(stri,"~~[^:]:?[^:]+|~[^:]:?[^:]+|[-'\"+*/.0-9a-zA-Z ]+")
}
   
captureCat. = function(a){
  capture.output(cat(a))
}
formulatoList..= function(ee){
  l3=as.list(ee)
  myList=list()
  l32=if(class(ee)=="call")capturePrint(ee) else captureCat(ee)
  lso=captureListRegex(l32)%getElem%1
  #print(l32)
  for(ii in lso){
    #print(ii)
   #print(str_detect(ii,"^~[^:]:?[^:]+$"))
    if(str_detect(ii,"^[0-9.]+$")){
      myList=append(myList,as.numeric(ii))
    }else if(str_detect(ii,"^[0-9.]:[0-9.]$")){
      myList=append(myList,eval(lazyeval::as_call(ii)))
    }else if(str_detect(ii,"^~[^:]:?[^:]+$")){
      dd=str_match(ii,"~([^:]:?[^:]+)")
      #return(dd)
      dd=dd[[2]]
      #print(dd)
      myList=append(myList,eval(lazyeval::as_call(dd)))
      
    }else if(str_detect(ii,"^~~[^:]:?[^:]+$")){
      dd=str_match(ii,"~~([^:]:?[^:]+)")
      dd=dd%getCol%2
      myList=append(myList,as.list(list(eval(lazyeval::as_call(dd)))))
      
    }
  }
  if(str_detect(l32,"~~")) return(myList)
  myList %each% l__(.)
}

toMatchCall.=function(..){
  match.call()[[2]]
}
 captureCat = function(a){
  capture.output(cat(a))
}
captureListRegex=function(stri){
  #print(stri)
  #print(str_extract_all(stri,"~[^:]+:[^:]+"))
  str_extract_all(stri,"(~~[^:]+:?[^:]+)|(~[^:]+:[^:]+)|([-/'\"+/*/.0-9a-zA-Z ]+)")
}
formulatoList.= function(ee){
  l3=as.list(ee)
  myList=list()
  l32=if(class(ee)=="call")capturePrint(ee) else captureCat(ee)
  lso=captureListRegex(l32) %getElem% 1
  #print(lso)
  for(ii in lso){
    #print(ii)
   #print(str_detect(ii,"^~[^:]:?[^:]+$"))
    if(str_detect(ii,"^[0-9.]+$")){
      myList=append(myList,as.numeric(ii))
    }else if(str_detect(ii,"^[0-9.]:[0-9.]$")){
      myList=append(myList,eval(lazyeval::as_call(ii)))
    }else if(str_detect(ii,"^~[^:]+:[^:]+$")){
      dd=str_match(ii,"~([^:]+:[^:]+)")
      #return(dd)
      dd=dd[[2]]
      #print(dd)
      myList=append(myList,eval(lazyeval::as_call(dd)))
      
    }else if(str_detect(ii,"^~~[^:]+:[^:]+$")){
      dd=str_match(ii,"~~([^:]+:[^:]+)")
      dd=dd%getCol%2
      myList=append(myList,as.list(list(eval(lazyeval::as_call(dd)))))
      
    }
  }
  if(str_detect(l32,"~~")) return(myList)
  myList %each% l__(.)
}

toMatchCall=function(a){
 m= match.call()[[2]]
 mm=if(class(m)=="call")capturePrint(m) else captureCat(m)
 #print(m)
 #print(mm)
  if(str_detect(mm,"^\\..+\\.$")){
    capturePrint(a)
  }else{
    m
  }
}




.matchCall = StrCls("matchCall")
.list=StrCls("list")
to = function(tow,what){
  #print(tow)
  .what.=match.call()[[3]]
  switch (tow$str,
    matchCall = toMatchCall(.what.),
    list=formulatoList.(toMatchCall(.what.))
  )
}
"%from%" = to
"%each:%" = function(a,b){
  bb=match.call()[[3]]
  #print(bb)
  cpp=capturePrint(bb)
  cpp=str_sub(cpp,start=2)
  csl=lapply(str_split(cpp,":")[[1]],function(e)as.formula("~"%.%e))
  llee="l_("%.%paste(csl,collapse = ",")%.%")"
  #print(lazyeval::as_name(llee))
  #env=new.env()
  #assign("llee", llee, envir = env)
  eval(call("%each%",a,lazyeval::as_call(llee)))
 # do.call(`%each%`,list(a,quote(lazyeval::as_name(llee))), envir = env)
}
             
`%future%` = function(aa,i){
    a=match.call()
    a=do.call("future",list(a[[3]]))
    value(a)
}
             
suppressWarningsGgplot =  function(s) tg(print(s))

formulaToList = function(a,e){
  ee=match.call()
  formulatoList.(ee[[3]])
  #print("ecalle")
}
"%vtl%"=formulaToList
 loadPlotUsefull = function(){
  tg(lib + "ggiraph" +
    "DT"+
    "git:thomasp85/patchwork")
  }
  getCol. = function(datas,col){
   ar=match.call()
   #print(formulatoList.(ar))
      datas[,formulatoList.(ar[[3]])]
    }
  getCol = function(datas,col){
      datas[,col]
   }
    `%getCol%` = getCol
     `%getCol.%` = getCol. 
    getRow. = function(datas,row){
     ar=match.call()
      datas[formulatoList.(ar[[3]]),]
    }
              getRow = function(datas,row){
      datas[row,]
    }
    `%getRow%` = getRow
     `%getRow.%` = getRow.
  qplotSameGraphEachCol = function(d,...){ 
    qplot(x = ind, y = values,data=stack(d),...)
  } #boxplot, violin
hidePlot= curry(with_(l1__(pdf(NULL)),l1__(invisible(dev.off())))(.))
             
 reduce = function(x,ops){
    Reduce(x,f=ops)
} 
`%reduce%`=reduce
             
 addNamesToList=function(vec,names){
    names(vec)=names
    vec
}


nothing=function(...){}
             
             
             build_matrix <- function(..., cf_eval_environment = parent.frame()) {
  v <- as.list(substitute(list(...))[-1])
  force(cf_eval_environment)
  lv <- length(v)
  # inspect input
  if(lv<1) {
    return(data.frame())
  }
  # unpack
  unpack_val <- function(vi) {
    if(length(vi)<=0) {
      stop("wrapr::build_frame unexpected NULL/empty element")
    }
    if(is.name(vi)) {
      viv <- cf_eval_environment[[as.character(vi)]]
      if(is.name(viv)) {
        stop(paste("wrapr::build_frame name",
                   vi,
                   "resolved to another name:",
                   viv))
      }
      if(is.call(viv)) {
        stop(paste("wrapr::build_frame name",
                   vi,
                   "resolved to call",
                   viv))
      }
      if(length(viv)<=0) {
        stop(paste("wrapr::build_frame name",
                   vi,
                   "resolved to NULL"))
      }
      vi <- viv
    }
    if(is.call(vi)) {
      if((length(vi)==3) && (is_infix(vi[[1]]))) {
        vi <- list(unpack_val(vi[[2]]),
                   as.name("sep"),
                   unpack_val(vi[[3]]))
      } else {
        viv <- eval(vi,
                   envir = cf_eval_environment,
                   enclos = cf_eval_environment)
        if(is.name(viv)) {
          stop(paste("wrapr::build_frame eval",
                     vi,
                     "resolved to another name:",
                     viv))
        }
        if(length(viv)<=0) {
          stop(paste("wrapr::build_frame eval",
                     vi,
                     "resolved to NULL"))
        }
        vi <- viv
      }
    }
    Reduce(c, lapply(vi, as.list))
  }
  vu <- lapply(v, unpack_val)
  vu <- Reduce(c, lapply(vu, as.list))
  ncol <- length(vu)
  if(ncol<1) {
    stop("wrapr::build_frame() zero columns")
  }
  is_name <- vapply(vu, is.name, logical(1))
  if(any(is_name)) {
    ncol <- which(is_name)[[1]]-1
    vu <- vu[!is_name] # filter out names
  }
  nrow <- (length(vu)/ncol) - 1
  if(abs(nrow - round(nrow))>0.1) {
    stop("wrapr::build_frame confused as to cell count")
  }
  matrix(vu,byrow=T,ncol=ncol)
}

is_infix <- function(vi) {
  vi <- as.character(vi)
  if(nchar(vi)<=0) {
    return(FALSE)
  }
  if(substr(vi,1,1)=="`") {
    vi <- substr(vi,2,nchar(vi)-1)
  }
  if(nchar(vi)<=0) {
    return(FALSE)
  }
  if(substr(vi,1,1)=="%") {
    return(TRUE)
  }
  syms <- c("::", "$", "@", "^", ":",
            "*", "/", "+", "-",
            ">", ">=", "<", "<=",  "==", "!=",
            "&", "&&",
            "|", "||",
            "~",
            "->",  "->>",
            "=",
            "<-", "<<-")
  if(vi %in% syms) {
    return(TRUE)
  }
  return(FALSE)
}

layout_build_matrix = function(...){
    a=match.call()
    a[[1]]=build_matrix
    b=eval(a)
    layout(b)
}
"%rep%" = function(ll,rr){
    rep(ll,rr) %>% paste0(collapse = '')
}
 "%join%" = function(ll,rr){
    paste0(ll,collapse = rr)
}
 concatList = function(ll,rr,l1=list,l2=list,env=parent.frame()){
   .env <- new.env(parent=env)

        #print(substitute(l2))
        call1=as.call(list(substitute(l1),substitute(ll)))
        call2=as.call(list(substitute(l2),substitute(rr)))
        #print(do.call(substitute(l2),list(substitute(rr)),envir = .env))
        eval1=eval(call1,.env,.env)
        eval2=eval(call2,.env,.env)
        if(class(eval1)=="list"){
            list.append(eval1,eval2)
        }else{
            list(eval1,eval2)
        }
    
 }

lConcat_ <- function(e1, e2,l1=l1,l2=l1,noQuoteLeft=F,noQuoteR=F,env=parent.frame()) {
  .env <- new.env(parent=env)
    lsub=list()
    if(!noQuoteLeft)
        lsub=list.append(lsub,do.call(bquote,list(substitute(e1),where=.env)))
    if(!noQuoteR)
        lsub=list.append(lsub,do.call(bquote,list(substitute(e2),where=.env)))
    #print(lsub)
 
    if(noQuoteLeft)
        lsub=list.append(substitute(e1),lsub)
    if(noQuoteR)
        lsub=list.append(lsub,substitute(e2))
    #concatList2=curry(concatList(l1=l1,l2=l2,both=both))
   
     #lsm=(called)
     #print(lsm)
     lsub[["l1"]]=substitute(l1)
     lsub[["l2"]]=substitute(l2)
     lsub[["env"]]=.env
 
     #print(lsm)
 do.call(concatList,lsub,envir = .env)
}
l1=function(...){l(...)%getElem%1}
sepConcat = function(ll,rr,callOp=NULL){
 .env=new.env(parent=parent.frame())
    `%.%` = icor::`%.%`
    a=match.call()
    aa=a[[1]]
    aaa=if(is.null(callOp))as.character(aa)else callOp
    aaString=str_sub(aaa,2,str_length(aaa)-1)
    l1i=l1
    l2i=l1
    noQuoteLeft=if(str_detect(aaString,"\\._{0,4},_{0,4}\\.?$")) T else F
    noQuoteR=if(str_detect(aaString,"^\\.?_{0,4},_{0,4}\\.$")) T else F
    #print(aaString)
    if(!str_detect(aaString,"^\\.?_{0,4},_{0,4}\\.?$")) stop("^\\.?_{0,4},_{0,4}\\.?$")
    if(str_length(aaString)>1){
        splits=str_split(aaString,",")
        splits=splits%getElem%1
        split1=splits%getElem%1
        split2=splits%getElem%2
        #print(str_length(split1))
        split1nb=str_length(split1) - ifelse(noQuoteLeft,1,0)
        split2nb=str_length(split2) - ifelse(noQuoteR,1,0)
        
        lfn1="_" %rep% split1nb
        lfn2="_" %rep% split2nb
        lfn1OK="l1"%.%lfn1%.%ifelse(noQuoteLeft,".","")
        lfn2OK="l1"%.%lfn2%.%ifelse(noQuoteR,".","")
        
        l1i=as.name(lfn1OK)
        l2i=as.name(lfn2OK)
    }
    do.call(lConcat_,list(substitute(ll),substitute(rr),l1=l1i,l2=l2i,noQuoteLeft=noQuoteLeft,noQuoteR=noQuoteR,env=.env))
   # lConcat_(l1=l1i,l2=l2i,ll,rr)
}
#"%.%"=icor::`%.%`
             
"%,%" = sepConcat
"%_,%" = sepConcat
"%__,%" = sepConcat
"%___,%" = sepConcat
"%____,%" = sepConcat
             
"%,_%" = sepConcat
"%_,_%" = sepConcat
"%__,_%" = sepConcat
"%___,_%" = sepConcat
"%____,_%" = sepConcat    
             
"%,__%" = sepConcat
"%_,__%" = sepConcat
"%__,__%" = sepConcat
"%___,__%" = sepConcat
"%____,__%" = sepConcat
             
"%,___%" = sepConcat
"%_,___%" = sepConcat
"%__,___%" = sepConcat
"%___,___%" = sepConcat
"%____,____%" = sepConcat
"%____,___%" = sepConcat

"%,____%" = sepConcat
"%_,____%" = sepConcat
"%__,____%" = sepConcat
"%___,____%" = sepConcat
"%____,____%" = sepConcat
             
"%.,%" = sepConcat
"%.,_%" = sepConcat
"%.,__%" = sepConcat
"%.,___%" = sepConcat
"%.,____%" = sepConcat
"%._,%" = sepConcat
"%._,_%" = sepConcat
"%._,__%" = sepConcat
"%._,___%" = sepConcat
"%._,____%" = sepConcat
"%.__,%" = sepConcat
"%.__,_%" = sepConcat
"%.__,__%" = sepConcat
"%.__,___%" = sepConcat
"%.__,____%" = sepConcat
"%.___,%" = sepConcat
"%.___,_%" = sepConcat
"%.___,__%" = sepConcat
"%.___,___%" = sepConcat
"%.___,____%" = sepConcat
"%.____,%" = sepConcat
"%.____,_%" = sepConcat
"%.____,__%" = sepConcat
"%.____,___%" = sepConcat
"%.____,____%" = sepConcat

"%,.%" = sepConcat
"%,_.%" = sepConcat
"%,__.%" = sepConcat
"%,___.%" = sepConcat
"%,____.%" = sepConcat
"%_,.%" = sepConcat
"%_,_.%" = sepConcat
"%_,__.%" = sepConcat
"%_,___.%" = sepConcat
"%_,____.%" = sepConcat
"%__,.%" = sepConcat
"%__,_.%" = sepConcat
"%__,__.%" = sepConcat
"%__,___.%" = sepConcat
"%__,____.%" = sepConcat
"%___,.%" = sepConcat
"%___,_.%" = sepConcat
"%___,__.%" = sepConcat
"%___,___.%" = sepConcat
"%___,____.%" = sepConcat
"%____,.%" = sepConcat
"%____,_.%" = sepConcat
"%____,__.%" = sepConcat
"%____,___.%" = sepConcat
"%____,____.%" = sepConcat

"%.,.%" = sepConcat
"%.,_.%" = sepConcat
"%.,__.%" = sepConcat
"%.,___.%" = sepConcat
"%.,____.%" = sepConcat
"%._,.%" = sepConcat
"%._,_.%" = sepConcat
"%._,__.%" = sepConcat
"%._,___.%" = sepConcat
"%._,____.%" = sepConcat
"%.__,.%" = sepConcat
"%.__,_.%" = sepConcat
"%.__,__.%" = sepConcat
"%.__,___.%" = sepConcat
"%.__,____.%" = sepConcat
"%.___,.%" = sepConcat
"%.___,_.%" = sepConcat
"%.___,__.%" = sepConcat
"%.___,___.%" = sepConcat
"%.___,____.%" = sepConcat
"%.____,.%" = sepConcat
"%.____,_.%" = sepConcat
"%.____,__.%" = sepConcat
"%.____,___.%" = sepConcat
"%.____,____.%" = sepConcat
             
print.rlang_lambda_function <- function(x, ...) {
  #cat_line("<lambda>")
  #x%>%str
    #UseMethod(x,"fn")
  #srcref <- attr(x, "srcref")
   # print(srcref)
  attributes(x) <- NULL
  x <- structure(x)
  a=capturePrint(x)
  a %getElems% 1:(length(a)-1)%each% {.} %>% cat(sep = "\n") %->% catJ
  cat("<icor_list>\n")
  catJ
}
lucas_plan = function(...){
    
    dots=lazyeval::lazy_dots(...)
    depsp=lapply(dots,function(f)list(deps=deps_code(f$expr),env=f$env))
    #print(depsp)
    #envi=depsp[[1]]$env
    ad=tempfile()
    b=new.env(parent = parent.frame())
    system(paste0("mkdir -p ",ad))             
    #packrat::init(a,restart = F,enter = F,infer.dependencies = F)
    a=new.env()
    #fe=list()
   #packrat::on()
                 #print()
    fe  = lapply(depsp,function(depsl){
    
        fep= lapply(depsl$deps$name,function(x){
            qa=NULL
            #print(x)
            try({
            qa=get(x, envir = b)
                },T)

            #print(find(x))
            if(!is.null(qa)){
                #print(x)
                au=find(x)
                ao=lapply(au,function(d){
                        if(d!=".GlobalEnv"){
                            u=stringr::str_sub(d,9)
                            #print(u)
                            u
                        }
                    })
                if(length(au)==1 && au[[1]]==".GlobalEnv"){
                     a[[x]]=qa
                }
                #t=names(getNamespaceImports(au))
                #install.packages(au)

                return(ao)
                }

                })
        return(fep)
          })
                 #print(fe)
                # packrat::off()
    pkg=Reduce("c",Reduce("c",Reduce("c",fe)))
    pkg=c(pkg,"lazyeval")
     pkg=pkg[!duplicated(pkg)]
    ds=askHavePkg(pkg)
    pkg2=pkg[!ds]
    o=lapply(pkg2,function(u){
        #package_dependencies
        uu=find.package(u)
        system(paste0("cp -R ",uu," ",ad,"/",u))
        tools::package_dependencies(u,recursive = T)[[u]]
    })
        pkg3=unlist(o[!duplicated(o)])
        ds2=askHavePkg(pkg3)   
        pkg4=pkg3[!ds2]
             

    o=lapply(c(pkg4),function(u){
        #package_dependencies
        uu=find.package(u)
        system(paste0("cp -R ",uu," ",ad,"/",u))
    })
     #uu=find.package("lazyeval")
     #system(paste0("cp -R ",uu," ",ad,"/","lazyeval"))
     list(a,ad,dots,pkg)
}
lucas_plan_export = function(d,name){
    er=paste0("lucasPlan_",name,".RData")
    oo=paste0("lucasPlan_",name,".tar.gz")
    save(d,file = er)
    fd=getwd()
    fq=paste0("mv ",er," ",d[[2]],"/ && cd ",d[[2]]," && tar -zcf ",oo," * ",er," && mv ",oo," ",fd,"/")
    #print(fq)
    system(fq)
    system(paste0("mkdir -p lucasPlan_",name," && mv ",oo," lucasPlan_",name,"/"))
    #fq
    paste0("lucasPlan_",name)
}
lucas_plan_import = function(name){
  #a=name
  nam=paste0("lucasPlan_",name)
  er=paste0(nam,".tar.gz")
  system(paste0("mkdir -p ",nam))
  system(paste0("cp ",er," ",nam,"/"))
  system(paste0("cd ",nam," && tar -xzf ",er))
  o=load(paste0(nam,"/",nam,".RData"))
  #untar(paste0("lucasPlan_",name,".tar.gz"), files = c(d[[2]],er))
  
  a=get(o)
  lapply(a[[4]],function(i)library(i,lib.loc = getwd(),character.only = T))
  lapply(names(a[[1]]),function(i)assign(i, a[[1]][[i]], envir = globalenv()))
  a
}
         tg. = function(f){
    suppressMessages(suppressPackageStartupMessages(suppressWarnings(f)))
}
          tg = function(f){
    s=capture.output(suppressMessages(suppressPackageStartupMessages(suppressWarnings(f))))
}
templateToR=function(name,f=templateLucas_plan_str){
    x <- f
  y <- gsub( "\\{\\{name\\}\\}", name, x )
  cat(y, file=paste0("lucasPlan_",name,"/lucas_plan_",name,".R"), sep="\n")
    paste0("lucas_plan_",name,".R")
}

sshConnectMoi=function(...){
      login=getOption("icor-ssh-login")
      pass==getOption("icor-ssh-pass")
        if (is.null(login) || is.null(pass) ){
            stop("you have to set icor::login and icor::pass")
        }
        ssh_connect(login,passwd=pass,...)
}
 askHavePkg = function(pkgs){
     dd=paste0("\"",unlist(pkgs),"\"",collapse =",")

        dd2=paste0("c(",dd,")")
     ed=tempfile()
     cat(paste0("cat(",dd2,"%in% rownames(installed.packages()))"),file=ed)
     #print(cat(readLines(ed)))
    tg({session <- sshConnectMoi(verbose = F)})
    scp_upload(session,ed,verbose = F)
    a=ssh_exec_wait(session, command =c(
        paste0("Rscript ",basename(ed)),
        paste0("rm -rf ",basename(ed))
        ),std_out=ed,std_err=ed)
     ssh_disconnect(session)
     a=tg.(readLines(ed))
     system(paste0("rm -rf ",ed))
     b=lapply(strsplit(a[[1]]," "),as.logical)
     b[[1]]

 }
sendToSsh=function(name,printOut=T,printErr=T,globally=T){
    #system(paste0("mkdir -p ","lucasPlan_",name))
    fileName=paste0("lucasPlan_",name,".tar.gz")
    template=templateToR(name)
    tg({session <- sshConnectMoi(verbose = F)})
    tg(scp_upload(session,paste0("lucasPlan_",name,"/",fileName),verbose = F))
    tg(scp_upload(session,paste0("lucasPlan_",name,"/",template),verbose = F))
    r=ssh_exec_wait(session, command = c(
    paste0('Rscript ',template) )
                 ,std_out = paste0("lucasPlan_",name,"/lucasPlan_",name,".log"),std_err = paste0("lucasPlan_",name,"/lucasPlan_",name,"_err.log"))
    tg(scp_download(session,paste0("lucasPlan_",name,"/lucasPlan_",name,"_rep.RData"),paste0("lucasPlan_",name,"/"),verbose=F))
    tg(ssh_exec_wait(session,command=c(
        paste0("rm -rf lucasPlan_",name," lucasPlan_",name,".tar.gz lucas_plan_",name,".R")
    )))
    ssh_disconnect(session)
    envi=new.env()
       # print(paste0("lucasPlan_",name,"/lucasPlan_",name,"_rep.RData"))
   # print(file.exists(paste0("lucasPlan_",name,"/lucasPlan_",name,"_rep.RData")))
    if(file.exists(paste0("lucasPlan_",name,"/lucasPlan_",name,"_rep.RData"))){
    u=load(paste0("lucasPlan_",name,"/lucasPlan_",name,"_rep.RData"),envir = envi,verbose = F)
        #print(u)
        p=get(u,envir = envi)[[1]]
        system(paste0("rm -rf ","lucasPlan_",name))
        
        #print(p)
        if(globally){
                    
        for(i in names(p)){
            assign(i,p[[i]],envir = globalenv())
            
        }
        }else{
             return(p)
        }
        }else{
         if(printOut)cat(cat(file = paste0(name,".log")))
        if(printErr){
            cat(cat("ERROR\n"))
            cat(readLines(paste0(name,"_err.log")))
        }
        return(NULL)
        }
    
    
}
randomString <- function(n=1, lenght=12)
{
    randomString <- c(1:n)                  # initialize vector
    for (i in 1:n)
    {
        randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                 lenght, replace=TRUE),
                                 collapse="")
    }
    return(randomString)
}
doInSSH = function(...,printName=F,printErr=F,printOut=F,noSSH=F){
    #print(list(substitute(list(...))))
    name=randomString()
    #return(substitute(list(...)))
    d=do.call(lucas_plan,list(substitute(list(...))))
    dd=lucas_plan_export(d,name)
    if(noSSH){
        return(dd)
    }
    #cat("ssh...\n")
    tg.({rep=sendToSsh(name,printErr=printErr,printOut=printOut)})
    if(printName)print(name)
    invisible(return(invisible(rep)))
    #dd
}
deps=function(...)deps_code(substitute(...))
         
doAndSkip =function(data,fn,env=parent.frame()){ 
        a=match.call()
        eval(a[[3]],envir = env,enclos = env)
        data
}
`%>skip>%` = doAndSkip
`%-|skip|->%` = doAndSkip    

             #al = Aleatoire$new()
#al$generer()
#al$generer(max=10)
#{1:100 %each% ~al$generer(max=10,double = T) }%>% hist

#1:100 %each% ~al$loi_discrete(c(0.05,0.05,0.1,0.6,0,0.2))
#{1:500 %each% ~rexp(1) }%>% density %>% plot ; {1:500 %each% ~al$exp(1) }%>% density %>% lines(col="red")
#doInSSH(noSSH = F,printErr = T,printOut = T,
#    dataSimDescribe=questionr::describe()
#    )
