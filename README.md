# ICOR, Correlation and Usefull Functions, Operators and Classes

# EQ</br>
 *if vector first is tester*</br>
 <, <=, ==, >=, ></br>
 </br>
 %||%<br></br> 
 ```R
> c(4,2,1) %||% c(9,4,0)  < 3
[1] FALSE
  ```
 %|%<br></br>
  ```R
  > c(4,2,1) %|% c(9,4,0)  < 3
[1] FALSE  TRUE  TRUE
    ```
 %&&%<br></br>
  ```R
  > c(4,2,1) %&&% c(9,4,0)  < 3
[1] FALSE
    ```
 %&%<br></br>
  ```R
  > c(4,2,1) %&% c(9,4,0)  < 3
[1] FALSE FALSE  TRUE
  ```
 </br>
 </br>
 # Params Func</br>
 %,%<br></br>
 %->%<br></br>
 </br>
 # String Concat</br>
 %.=%<br></br>
 %.%<br></br>
 </br>
 # Usefull</br>
 curry<br></br>
 smth<br></br>
 </br>
 </br>
 # Usefull DataFrame</br>
 toDF %toDF% toDFt %toDFt%<br></br>
 add_row_with_name<br></br>
 dfRowToList<br></br>
 dfToHTML<br></br>
 </br>
 # Aleatoire</br>
 Aleatoire</br>
 </br>
 # Categorical</br>
 %by% %byGraph%<br></br>
 corrCatCon<br></br>
 graphCatCon<br></br>
 int.hist<br></br>
 </br>
 # Usefull Utils</br>
 ## capture</br>
 captureCat capturePrint</br>
 ## embed</br>
 **embed**(x, height="100%",width="100%") </br>
 *Display widget x, in html in jupyter notebook*</br>
 *(see DT::datatable)*</br></br>
 ```R
graph=dataSim %>% ggplot(aes(x=time,y=bin1)) + geom_point() + geom_point_interactive(aes(data_id=rownames(dataSim)), size = 2) + theme_minimal()
graphWidget= graph %>% girafe(ggobj = .) %>% girafe_options(opts_hover(css = "fill:red;r:4pt;"))
embed(graphWidget,"500px","70%")
```
![Embed](https://raw.githubusercontent.com/luluperet/icor/master/img/embed.png)
*(see ggirafe)*</br></br>
 **embedDT**(dt,height="100%",width="100%",...)</br>
 *Display a data frame in html beautiful table interactive in **jupyter notebook***</br>
  *DT::datatable(dt,...)*
  ```R 
  embedDT(dataSim,"500px","100%",filter="top")
  #if Rstudio
  DT::datatable(iris,filter="top")
  ```
  ![EmbedDT](https://raw.githubusercontent.com/luluperet/icor/master/img/embedDT.png)
 *(see embed)*</br>
 ## plotWH
 **plotWH**(w=NULL,h=NULL)-> reset(function)</br>
 *Change Plot Width/height*</br>
 ```R 
 resetWH = plotWH(w=10)
 ... #plot graphique
 resetWH() # when finish with modified width/height
 ```
 *(options repr.plot.(width/height))*</br>
 ## Warnings 
 **showWarning()**</br>
 **hideWarning()**</br>
 **toggleWarning()**</br>
 ```R 
 > testit <- function() warning("testit")
 > testit() #Warn
Warning message:
In testit() : testit
> hideWarning()
> testit() #not Warn
> showWarning()
> testit() #Warn
Warning message:
In testit() : testit
> toggleWarning()
> testit() #not Warn
> toggleWarning()
> testit() #Warn
Warning message:
In testit() : testit
 ```
 **tg:**(smth)</br>
 *hide/suppress warnings and messages*</br>
 ```R 
 tg(smthWithWarningsOrMessages)
 ```
 **suppressWarningsGgplot**(ggplotToPlot)</br>
 *hide ggplots warnings*
 ```R 
 suppressWarningsGgplot(ggplotPlot)
 ```
 </br>
 # Usefull plot Density </br>
 densityLines densityPlt</br>
 </br>
 # Usefull Library</br>
 lib</br>
 load</br>
 print.Lib</br>
 </br>
 # Usefull Packages Icor</br>
 reloadIcor</br>
 update</br>
 updateReloadIcor</br>
 detachFast</br>
 </br>
 # Each + Map</br>
 %each:% %each%</br>
 %eachCol% %eachRow%</br>
 %eachFn%</br>
 %map%</br>
 %mapFns%</br>
 %Xtimes%</br>
 </br>
 # List Sequence Formula Customize</br>
 formulatoList.</br>
 %from%</br>
 </br>
 # Getter </br>
 %getCol% %getRow%</br>
 %getElem% %getElem2% %getElems%</br>
 </br>
 </br>
 # Icor</br>
 icor</br>
 icor.corrToStudent</br>
 icor.critical.r</br>
 icor.studentToCorr</br>
 icor.graph</br>
 </br>
 # Lists</br>
 l</br>
 ll</br>
 l_ l1_</br>
 l__</br>
 l___</br>
 %listToDotsFn_% %listToDotsFn%</br>
 </br>
 </br>
 # Select Col</br>
 catCol %catCol% notCatCol %!catCol%</br>
 numericCol %numericCol% notNumericCol %!numericCol%</br>
 </br>
 # StrCls</br>
 StrCls</br>
 </br>
 startsWithGet ?? lapplys ??</br>
 </br>
 # Tests </br>
 test_normal</br>
 test_same_distrib</br>
 </br>
 # Future</br>
 (HenrikBengtsson/future)</br>
 %future%</br>
 </br>
 # Usefull Plot </br>
 loadPlotUsefull</br>
 
//e= d %>% girafe(ggobj = .) %>% girafe_options(opts_hover(css = "fill:red;r:4pt;"))
