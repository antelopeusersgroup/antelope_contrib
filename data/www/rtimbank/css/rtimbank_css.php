<?php
if( !extension_loaded( 'Datascope' ) ) {
	dl( 'Datascope.so' ) ; # load Datascope library
}
$pf = "../rtimbank" ;

$image_settings = pfget( $pf, "image_settings" ) ; # Image Settings

// declare the output of the file as CSS
header('Content-type: text/css');
?>
/*
Realtime Imagebank Dynamically Generated CSS Stylesheet
*/

#content {
   background: white ;
   border-top: 1px solid black ;
   border-left: 1px solid black ;
   border-right: 1px solid black ;
   padding: 0em ;
   text-align: left ;
   margin-left: auto ;
   margin-right: auto ;
}
#footer {
   clear: both ;
   background: #<?php echo $image_settings['footer_color'] ; ?> url(<?php echo $image_settings['footer_bkgrd'] ; ?>) no-repeat top right ;
   border: 1px solid black ;
   padding: 5px ;
}
#banner {
   background: white url(<?php echo $image_settings['header_bkgrd'] ; ?>) repeat top right ;
   height:50px;
   voice-family: "\"}\"";
   voice-family: inherit;
   height:49px;
   border-bottom: 1px solid black ;
}
html>body #banner {
   height:49px;
}
#left {
   position: absolute;
   left:20px;
   top:100px;
   width:213px;
   background:#fff;
}
#center {
   background:#fff;
   margin-left: 225px;
   margin-right:23px;
   top:100px;
   voice-family: "\"}\"";
   voice-family: inherit;
   margin-left: 227px;
   margin-right: 25px;
}
html>body #center {
   top:100px;
   margin-left: 227px;
   margin-right: 25px;
}
#imagewindow {
   margin: 0px auto ;
}

/* Redefine HTML tags */

html {
   min-width: 900px;
}

body {
   font-family: "Trebuchet MS",arial,sans-serif ;
   background: white url(<?php echo $image_settings['page_bkgrd'] ; ?>) repeat top right;
   margin: 5px;
   padding: 10px;
   color: #FFFFFF ;
   text-align: center ;
}
h1 {
   font-family: "Trebuchet MS",arial,sans-serif ;
   font-size: 1.3em ;
   color: #0F2C3D ;
   text-transform: capitalize ;
   text-align: left ;
   border-bottom: 1px solid gray;
}
h2 {
   font-family: "Trebuchet MS",arial,sans-serif ;
   font-size: 1.2em ;
   color: #6C8AA7 ;
   text-transform: capitalize ;
   text-align: left ;
   border-bottom: 1px solid gray;
}
table {
   border-style: hidden ;
   margin: 1.0em, auto ;
}
th {
   font-family: "Trebuchet MS",arial,sans-serif ;
   font-size: 1.0em ;
   color: #FFFFFF ;
   border: 1px solid black ;
   padding: 3px ;
   background: #0F2C3D;
}
td {
   font-family: "Trebuchet MS",arial,sans-serif ;
   font-size: 1.0em ;
   color: #000000 ;
   border: 1px solid black ;
   padding: 3px ;
   background: #5A738B ;
   text-align: center ;
}
p {
   font-family: "Trebuchet MS",arial,sans-serif ;
   font-size: 1.0em ;
   color: #000000 ;
}
#footer p {
   font-family: "Trebuchet MS",arial,sans-serif ;
   font-size: 11px ;
   color: #FFFFFF ;
}
li {
   color: black;
   font: normal 1.0em "Trebuchet MS",arial,sans-serif ;
   text-decoration: none;
   list-style: square ;
}
fieldset {
   border: 1px black solid ;
   background: #FFC ;
   margin: 0 auto 2.0em auto ;
}
legend {
   font: bold 1.1em "Trebuchet MS",arial,sans-serif ;
   color: #000000 ;
}
hr {
   display: block ;
   clear: left ;
   margin: -0.66em 0 ;
   visibility: hidden ;
}
#clearer {
   clear: left;
   line-height: 0;
   height: 0;
}

/* CSS selectors */

#content a:link {
   text-decoration : underline;
   color: gray; 
   background: transparent; 
}  
#content a:visited {
   text-decoration : underline;
   color: #333;
   background: transparent;
}
#content a:hover {
   text-decoration : none;
   color: navy;
   background: transparent;
}
#content a:active {
   text-decoration : underline;
   color: gray ;
   background: transparent;
}

.arrow a:link {
   text-decoration: none ;
}
.arrow a:visited {
   text-decoration: none ;
}
.arrow a:hover {
   text-decoration: none ;
}
.arrow a:active {
   text-decoration: none ;
}

#footer a:link {
   text-decoration : underline;
   color: #9CF ; 
   background: transparent; 
}  
#footer a:visited {
   text-decoration : underline;
   color: #9CF;
   background: transparent;
}
#footer a:hover {
   text-decoration : none;
   color: #9CF ;
   background: transparent;
}
#footer a:active {
   text-decoration : underline;
   color: #9CF ;
   background: transparent;
}

/* Custom tags */

.titleCamera {
   color: #900 ;
   border: none ;
}
.arrow {
   background-color: white ;
   border: 1px solid black ;
   font-weight: bold ;
   font-size: 1.0em ;
   padding: 2px ;
   text-decoration: none ;
}
.highlight {
   text-align: center ;
   background-color: #6C8AA7 ;
   border: 1px solid black ;
   color: black ;
   margin-left: auto ;
   margin-right: auto ;
   padding: 0.5em ;
   font-weight: bold ;
}
.white {
   color: white ;
}
.dropbox {
   font: 11px "Trebuchet MS",arial,sans-serif ;
   color: #102132 ;
   background-color: #D7E5F2;
   border: 1px solid #284279;
}
.full_res_link {
   font: 11px "Trebuchet MS",arial,sans-serif ;
   font-weight: bold ;
   text-decoration: underline ;
   color: #102132 ;
   background-color: #D7E5F2;
   border: 1px solid #284279;
   text-align: center ;
   padding: 1.0em ;
}
