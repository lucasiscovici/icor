# EQ</br>
 %||%<br></br>
 %|%<br></br>
 %&&%<br></br>
 %&%<br></br>
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
 **embed:** function (x, height="100%",width="100%") </br>
 *Display widget x, in html in jupyter notebook*</br>
 *(DT::datatable)*</br></br>
 `graph=dataSim %>% ggplot(aes(x=time,y=bin1)) + geom_point() + geom_point_interactive(aes(data_id=rownames(dataSim)), size = 2) + theme_minimal()
 
 graphWidget= graph %>% girafe(ggobj = .) %>% girafe_options(opts_hover(css = "fill:red;r:4pt;"))`</br></br>
`embed(graphWidget,"500px","70%")`</br>
*(ggirafe)*</br></br>
 **embedDT:** function(dt,height="100%",width="100%",...)</br>
 *Display a data frame in html beautiful table interactive in jupyter notebook*</br>
  `embedDT(dataSim,"500px","100%",filter="top")`</br>
 *(embed)*</br>
 **plotWH**</br>
 **showWarning**</br>
 **tg**</br>
 **suppressWarningsGgplot**</br>
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
