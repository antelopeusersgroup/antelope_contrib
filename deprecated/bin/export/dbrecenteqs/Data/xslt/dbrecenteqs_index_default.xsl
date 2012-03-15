<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="html"/>

<xsl:variable name="newline">
<xsl:text>
</xsl:text>
</xsl:variable>

<xsl:template match="/">
  <xsl:apply-templates select="dbrecenteqs_main"/>
</xsl:template>

<xsl:template match="dbrecenteqs_main">
 <html>
 <head>
 <title> <xsl:value-of select="page_title"/> </title>
  <base href="{page_base}"/> 
  <meta http-equiv="refresh" content="{page_refresh_seconds}"/>
 </head>
 <body bgcolor="white">
 <map name="{pixmap/clientside_mapname}">
  <xsl:apply-templates select="quakelist" mode="clientside_imagemap"/> 
 </map>
 <center>
 <A href="{institute_url}">
 <img src="{institute_logo_url}" align="center" alt="{institute_description}"/>
 </A> 
 </center>
 <h1 align="center"> 
 	<A href="{page_base}">
        <img align="top" src="{wiggle_href}" alt="Link to {page_title}"/>
        <xsl:value-of select="map_description"/>
 	</A> 
 </h1>
 <center><img src="{pixmap/file}" usemap="#{pixmap/clientside_mapname}" align="center"/>
 </center>
 <br />
 <br />
 <center><img src="{legend_url}" align="center" alt="{legend_description}" />
 </center>
 <xsl:apply-templates select="other_maps"/>
 <xsl:apply-templates select="quakelist" mode="quaketable"/>
 <hr/>
 <xsl:apply-templates select="other_regions"/>
 <hr/>
 <xsl:apply-templates select="credits"/>
 </body>
 </html>
</xsl:template>

<xsl:template match="othermap">
  <tr><td><A HREF="{./href}"><xsl:value-of select="./text"/></A></td></tr>
</xsl:template>

<xsl:template match="credits">
  <h2>Credits:</h2>
  <pre>
  <xsl:apply-templates select="./credit"/>
  </pre>
</xsl:template>

<xsl:template match="other_regions">
  <center>
  <table border="6" cellspacing="6" bgcolor="beige">
  <tr><td bgcolor="white">Other seismic regions:</td></tr>
  <xsl:apply-templates match="./region"/>
  </table> 
  </center>
</xsl:template>

<xsl:template match="region">
  <tr><td><A HREF="{./href}"><xsl:value-of select="./text"/></A></td></tr>
</xsl:template>

<xsl:template match="credit">
<A href="{./href}"><xsl:value-of select="./text"/></A>
<xsl:value-of select="$newline"/>
</xsl:template>

<xsl:template match="quake" mode="clientside_imagemap">
  <area SHAPE="{./shape}" COORDS="{./coords}" HREF="{./href}"/> 
</xsl:template>

<xsl:template match="other_maps">
  <center>
  <table border="6" cellspacing="6" bgcolor="beige">
  <tr><td bgcolor="white">Other maps:</td></tr>
  <xsl:apply-templates select="./othermap"/>
  </table> 
  </center>
</xsl:template>

<xsl:template match="quake" mode="quaketable">
  <tr>
    <td><A href="{./href}"><xsl:value-of select="./localtime_string"/></A></td>
    <td><A href="{./href}"><xsl:value-of select="./mag_string"/></A></td>
    <td><A href="{./href}"><xsl:value-of select="./region_string"/></A></td>
  </tr>
</xsl:template>

<xsl:template match="quakelist" mode="clientside_imagemap">
  <xsl:apply-templates match="./quake" mode="clientside_imagemap"/>
</xsl:template>

<xsl:template match="quakelist" mode="quaketable">
  <center>
  <table border="6" rules="rows" cellspacing="6" bgcolor="beige">
  <tr><td colspan="3" bgcolor="white">
  <center>
  <xsl:value-of select="count(./quake)"/> Earthquakes Shown on This Page:
  </center>
  </td></tr>
  <tr>
    <td><center>Local Time</center></td>
    <td><center>Magnitude</center></td>
    <td><center>Region</center></td>
  </tr>
  <xsl:apply-templates match="./quake" mode="quaketable"/>
  </table>
  </center>
</xsl:template>

</xsl:stylesheet>
