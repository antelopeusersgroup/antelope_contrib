<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="xml" indent="yes"/>

<xsl:template match="/">

  <xsl:apply-templates select="contents"/>

</xsl:template>

<xsl:template match="contents">

  <EQMessage>

     <Source><xsl:value-of select="eqmessage_source"/></Source>
     <Sent><xsl:value-of select="sent"/></Sent>
     <xsl:apply-templates select="events"/>

  </EQMessage>

</xsl:template>

<xsl:template match="events">
  
  <xsl:apply-templates select="hypocenter"/>

</xsl:template>

<xsl:template match="hypocenter">

   <Event>
     <DataSource><xsl:value-of select="../../eqmessage_source"/></DataSource>
     <EventID><xsl:value-of select="evid"/></EventID>
     <Version><xsl:value-of select="orid"/></Version>
     <Origin>
     <Latitude><xsl:value-of select="lat"/></Latitude>
     <Longitude><xsl:value-of select="lon"/></Longitude>
     <Depth><xsl:value-of select="depth"/></Depth>
     <Time><xsl:value-of select="time"/></Time>
     <Magnitude>
     <TypeKey><xsl:value-of select="magtype"/></TypeKey>
     <Value><xsl:value-of select="mag"/></Value>
     </Magnitude>
     </Origin>
   </Event>

</xsl:template>

</xsl:stylesheet>
