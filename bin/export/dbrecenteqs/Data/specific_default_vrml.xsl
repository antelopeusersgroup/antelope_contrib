<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text"/>

<xsl:variable name="newline">
<xsl:text>
</xsl:text>
</xsl:variable>

<xsl:template match="/">
  <xsl:apply-templates select="specific_quake"/>
</xsl:template>

<xsl:template match="specific_quake">
<xsl:text>#VRML V2.0 utf8
Background {
    skyColor [
        0.0 0.2 0.7,
        0.0 0.5 1.0,
        1.0 1.0 1.0
    ]
    skyAngle [ 1.309, 1.571 ]
    groundColor [
        0.1 0.10 0.0,
        0.4 0.25 0.2,
        0.6 0.60 0.6,
    ]
    groundAngle [ 1.309, 1.571 ]
}

Transform {
        translation </xsl:text>
	<xsl:value-of select="floor(./pixmap/width div 2)"/>
	<xsl:text> </xsl:text>
	<xsl:value-of select="floor(./pixmap/height div 2)"/>
	<xsl:text> 0
        children [
                Shape {
                        appearance Appearance {
                                material Material {emissiveColor 1 0 0}
                                texture ImageTexture {url "</xsl:text>
				    <xsl:value-of select="pixmap/file"/>
				<xsl:text>"}
                        }
                        geometry Box {
                                size </xsl:text>
				<xsl:value-of select="pixmap/width"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="pixmap/height"/>
				<xsl:text> </xsl:text>
				<xsl:value-of select="./pixmap/ypixperkm"/>
				<xsl:text>
                        }
                }
        ]
}

Group {
  children [ 
	Appearance {
	},
	  
	PointSet {
	  coord 
	  Coordinate {
		point [ </xsl:text>
		<xsl:apply-templates select="origins"/> 
		<xsl:text>              ]
	  }
	} ]
</xsl:text>
}
</xsl:template>

<xsl:template match="origins">
  <xsl:apply-templates match="./origin"/>
</xsl:template>

<xsl:template match="origin">
  <xsl:text>                   </xsl:text>
  <xsl:value-of select="./x"/>
  <xsl:text> </xsl:text>
  <xsl:value-of select="../../pixmap/height - ./y"/>
  <xsl:text> </xsl:text>
  <xsl:value-of select="-1 * ./depth_km"/>
  <xsl:text>,</xsl:text>
</xsl:template>

</xsl:stylesheet>
