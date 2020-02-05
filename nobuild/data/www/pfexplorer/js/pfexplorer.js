<!--
// Function to show and hide div layers
// and change plus/minus signs
function show( which, num ) {
   // START: CSS STYLES - must match those defined in the stylesheet
   var parentDiv = "parent" ; // content div name
   var pfsDivName = "pfsDiv" ; // pfsDiv content (left-side) div name
   var pfContentClassName = "pfContentClass" ; // pfContent (right-side) div name
   var pfsDivBkgrd = "#ff9" ; // .pfsDiv.background-color
   var pfsDivBorder = "none" ; // .pfsDiv.border
   var pfsDivTexDec = "none" ; // .pfsDiv.text-decoration
   var pfsDivSelectedButtonsBkgrd = "white" ; // pfsDiv selected item style.background-color
   var pfsDivSelectedButtonsBorder = "2px solid black" ; // pfsDiv selected item style.border
   var pfsDivHighlightText="-"; // pfsDiv selected item highlight text
   var pfsDivNormalText="+"; // pfsDiv selected item normal text
   // END: CSS STYLES

   var main = document.getElementById( parentDiv ) ;

   var divs = main.getElementsByTagName("div") ;
   for( i=0; i<divs.length; i++ ) {
      if( divs[i].className == pfContentClassName ) {
         divs[i].style.visibility="hidden" ;
      }
   }
   document.getElementById( which ).style.visibility="visible" ;

   var tree = document.getElementById( pfsDivName ) ;
   var listItem = tree.getElementsByTagName( "li" ) ;
   for( j=0; j<listItem.length; j++ ) {
      listItem[j].style.backgroundColor = pfsDivBkgrd ;
      listItem[j].style.border = pfsDivBorder ;
      listItem[j].style.textDecoration = pfsDivTexDec ;
      var status = listItem[j].getElementsByTagName("a").item(0).firstChild ;
      status.nodeValue=status.nodeValue.replace( pfsDivHighlightText, pfsDivNormalText ) ;
   }
   var myStatus = listItem.item( num ).getElementsByTagName("a").item(0).firstChild ;
   myStatus.nodeValue = myStatus.nodeValue.replace( pfsDivNormalText, pfsDivHighlightText ) ;
   listItem.item( num ).style.backgroundColor = pfsDivSelectedButtonsBkgrd ;
   listItem.item( num ).style.border = pfsDivSelectedButtonsBorder ;
}
// -->
