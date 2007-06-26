<!-- $Id$ -->
<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes" />

<xsl:template match="/">
	<xsl:variable name="alldataloggers">
	<alldataloggerlist>
		<xsl:apply-templates select="document( sources/source/@href, .)/pfarr/pfarr/pfarr" />
	</alldataloggerlist>
	</xsl:variable>

	<xsl:variable name="dataloggers">
	<dataloggerlist>
		<xsl:for-each select="$alldataloggers/alldataloggerlist/datalogger">
			<xsl:sort select="@sortorder" order="ascending" />
			<xsl:copy-of select="."/>
		</xsl:for-each>
	</dataloggerlist>
	</xsl:variable>

	<xsl:copy-of select="$dataloggers"/>

</xsl:template>

<xsl:template match="pfarr">
	<xsl:variable name="runstat" select="pfstring[@name = 'con']" />
	<xsl:variable name="sortorder">
		<xsl:choose>
			<xsl:when test="$runstat = 'yes'">4</xsl:when>
			<xsl:when test="$runstat = 'waiting'">4</xsl:when>
			<xsl:when test="$runstat = 'su'">1</xsl:when>
			<xsl:when test="$runstat = 'reg'">1</xsl:when>
			<xsl:when test="$runstat = 'sleeping'">1</xsl:when>
			<xsl:when test="$runstat = 'hibernating'">1</xsl:when>
			<xsl:when test="$runstat = 'no'">1</xsl:when>
			<xsl:when test="$runstat = 'stopped'">0</xsl:when>
			<xsl:otherwise>1</xsl:otherwise>
		</xsl:choose>
	</xsl:variable>
	<xsl:variable name="orderandname" select="concat( $sortorder,substring-after(@name, '_'))" />
	<datalogger name="{@name}" sortorder="{$orderandname}">
		<xsl:apply-templates select="pfstring"/>
	</datalogger>
</xsl:template>

<xsl:template match="pfstring">
	<xsl:element name="{@name}">
		<xsl:value-of select="." />
	</xsl:element>
</xsl:template>

</xsl:stylesheet>
