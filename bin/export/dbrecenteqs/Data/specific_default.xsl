<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="html"/>

<xsl:variable name="newline">
<xsl:text>
</xsl:text>
</xsl:variable>


<xsl:template match="/">
  <xsl:apply-templates select="specific_quake"/>
</xsl:template>

<xsl:template match="specific_quake">
  <html>
  <head>
  <base href="{page_base}"/> 
  <title> <xsl:value-of select="page_title"/> </title>
  <meta http-equiv="refresh" content="{page_refresh_seconds}"/>
  </head>
  <body bgcolor="white">
  <map name="{pixmap/clientside_mapname}">
   <xsl:apply-templates select="origins" mode="clientside_imagemap"/>
  </map>
  <center>
  <A href="{institute_url}">
  <img src="{institute_logo_url}" align="center" alt="{institute_description}"/>
  </A> 
  </center>
  <br/>
  <center>
  <h1>
  <A href="{dbrecenteqs_base}">
  <img align="top" src="{wiggle_href}" alt="Link to {page_title}"/>
  </A>
  <xsl:value-of select="region_string"/>
  </h1>
  </center>
  <br/>
  <center>
  <img src="{pixmap/file}" align="center" usemap="#{pixmap/clientside_mapname}"/>
  </center>
  <br/>
  <center>
  <xsl:apply-templates select="origins" mode="origintables"/>
  </center>
  <br/>
  <center>
  <xsl:apply-templates select="nearest"/>
  </center>
  <br/>
  <xsl:apply-templates select="other_maps"/>
  <hr/>
  <xsl:apply-templates select="credits"/>
  </body>
  </html>
</xsl:template>

<xsl:template match="nearest">
  <h2>This earthquake was:</h2>
  <table border="6" cellspacing="6" bgcolor="#99CCFF">
  <xsl:apply-templates select="nearby_place"/>
  </table>
</xsl:template>

<xsl:template match="nearby_place">
  <tr><td><xsl:value-of select="."/></td></tr>
</xsl:template>

<xsl:template match="origins" mode="origintables">
  <xsl:apply-templates select="origin[@type = 'prefor']" mode="origintable"/>
  <xsl:apply-templates select="origin[@type = 'nonprefor']" mode="origintable"/>
</xsl:template>

<xsl:template match="origins" mode="clientside_imagemap">
  <xsl:apply-templates match="./origin" mode="clientside_imagemap"/>
</xsl:template>

<xsl:template match="origin" mode="origintable">

  <xsl:choose>
       <xsl:when test="@type = 'prefor'">
          <A name="prefor"/><h2>Preferred Hypocentral Solution:</h2>
       </xsl:when>
       <xsl:otherwise>
          <A name="{@name}"/><h2>Alternate Hypocentral Solution:</h2>
       </xsl:otherwise>
  </xsl:choose>
  <xsl:variable name="bgcolor">
    <xsl:choose>
       <xsl:when test="@type = 'prefor'">
    	  <xsl:text>beige</xsl:text>
       </xsl:when>
       <xsl:otherwise>
    	  <xsl:text>white</xsl:text>
       </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <table border="6" cellspacine="6" bgcolor="{$bgcolor}">
  <tr>
     <td>Local Date:</td>
     <td><xsl:value-of select="./localdate_string"/></td>
  </tr>
  <tr>
     <td>Local Time:</td>
     <td><xsl:value-of select="./localtime_string"/></td>
  </tr>
  <tr>
     <td>Universal Time:</td>
     <td><xsl:value-of select="./utc_string"/></td>
  </tr>
  <tr>
     <td>Magnitude:</td>
     <td><xsl:value-of select="./mag_string"/></td>
  </tr>
  <tr>
     <td>Latitude:</td>
     <td><xsl:value-of select="./lat"/></td>
  </tr>
  <tr>
     <td>Longitude:</td>
     <td><xsl:value-of select="./lon"/></td>
  </tr>
  <tr>
     <td>Depth:</td>
     <td><xsl:value-of select="./depth_string"/></td>
  </tr>
  <tr>
     <td>Author:</td>
     <xsl:choose>
	<xsl:when test="auth_href != ''">
           <td><A href="{./auth_href}"><xsl:value-of select="./auth"/></A></td>
     	</xsl:when>
	<xsl:otherwise>
       	   <td><xsl:value-of select="./auth"/></td>
	</xsl:otherwise>
     </xsl:choose>
  </tr>
  </table>
</xsl:template>

<xsl:template match="origin" mode="clientside_imagemap">

  <xsl:variable name="href">
     <xsl:choose>
       <xsl:when test="@type = 'prefor'">
          <xsl:text>prefor</xsl:text>
       </xsl:when>
       <xsl:otherwise>
          <xsl:value-of select="./@name"/>
       </xsl:otherwise>
     </xsl:choose>
  </xsl:variable>

  <area SHAPE="{./shape}" COORDS="{./coords}" HREF="#{$href}"/> 

</xsl:template>

<xsl:template match="credits">
  <h2>Credits:</h2>
  <pre>
  <xsl:apply-templates select="./credit"/>
  </pre>
</xsl:template>

<xsl:template match="credit">
<A href="{./href}"><xsl:value-of select="./text"/></A>
<xsl:value-of select="$newline"/>
</xsl:template>

<xsl:template match="other_maps">
  <center>
  <table border="6" cellspacing="6" bgcolor="beige">
  <tr><td bgcolor="white">Other maps:</td></tr>
  <xsl:apply-templates select="./othermap"/>
  </table> 
  </center>
</xsl:template>

<xsl:template match="othermap">
  <tr><td><A HREF="{./href}"><xsl:value-of select="./text"/></A></td></tr>
</xsl:template>

</xsl:stylesheet>
