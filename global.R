library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(plotly)

ppvHTML <- '<div style="display: inline-block">

<span  style="float: left; padding: 1em 0">
PPV =&nbsp;&nbsp;&nbsp;
</span>
<table style="float: left;">
<tr>
  <td style="padding: 2px;border: 1px solid black;color:white;background-color:#008C48">TP</td>
  <td style="padding: 2px;border: 1px solid black;">FP</td>
</tr>
<tr>
  <td style="padding: 2px;border: 1px solid black;">FN</td>
  <td style="padding: 2px;border: 1px solid black;">TN</td>
</tr>
</table>
<span  style="float: left; padding: 1em 0">
&nbsp;&nbsp;&nbsp;/&nbsp;&nbsp;&nbsp;
</span>
<table style="float: left;">
<tr>
  <td style="padding: 2px;border: 1px solid black;color:white;background-color:#008C48">TP</td>
  <td style="padding: 2px;border: 1px solid black;color:white;background-color:#EE2E2F">FP</td>
</tr>
<tr>
  <td style="padding: 2px;border: 1px solid black;">FN</td>
  <td style="padding: 2px;border: 1px solid black;">TN</td>
</tr>
</table>
</div>'
