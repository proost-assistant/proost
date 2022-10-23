<?xml version="1.0"?>

<xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="xml"/>

    <xsl:template match="node() | @*">
        <xsl:copy>
            <xsl:apply-templates select="node() | @*"/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="line">
        <xsl:copy>
            <xsl:apply-templates select="@*"/>
            <xsl:attribute name="hits">
                <xsl:choose>
                    <xsl:when test="conditions/condition[@coverage='0']">0</xsl:when>
                    <xsl:when test="@hits='0'">0</xsl:when>
                    <xsl:otherwise>1</xsl:otherwise>
                </xsl:choose>
            </xsl:attribute>
            <xsl:apply-templates select="node()"/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>
