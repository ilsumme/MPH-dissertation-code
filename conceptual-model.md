conceptual model
================

to show output for saving image = To undo this behavior, open
RStudio-\>Preferences, and select the R Markdown group on the left.
Uncheck the box that says “Show output inline for all R Markdown
documents”.

``` r
library(DiagrammeR) 
library(DiagrammeRsvg)  # for conversion to svg
library(rsvg)
```

    ## Linking to librsvg 2.48.4

<http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html>

\#CODE to save grViz as a png \# 2. Convert to SVG, then save as png x =
export\_svg(x) x = charToRaw(x) \# flatten rsvg\_png(x, “g2.png”) \#
saved graph as png in current working directory

``` r
#to save as an object do logic = grViz then will be saved as a grViz name "logic" but won't output in the side window 

#key
#subgraph for key and outcomes

#inspired by lit review, first principles, Heise 2019

grViz(" 
      
      digraph 2.0 {
  #appearance
 
      graph[
  label = 'Conceptual Model
  '
  fontname = Helvetica
  labelloc = t
  fontsize = 30
      nodesep= 0.3,
      ranksep= 0.8,
      ]
      node[
      shape = box,
      fontcolor = Black
      fontname = Helvetica
      fontsize = 18
      color = LightSlateGrey
      ]
      
      edge[ 
      color = LightSlateGrey
      ]
      
  #nodes
      
      node[style = filled, fillcolor = Bisque]
      gsn [label = 'biased gender \n social norms', fontsize = 20]
      
      node[penwidth = 1, fillcolor= Bisque]
      pol [label = 'gender bias in \n political domain']
      edu [label = 'gender bias in \n educational domain']
      eco [label = 'gender bias in \n economic domain']
      phy [label = 'gender bias in \n physical domain']
      
      node[penwidth = 1, color = LightSlateGrey, style = notfilled]
      pol2[label = 'gendered differences \n in power']
      edu2[label = 'gendered differences \n in education']
      eco2[label = 'gendered differences in \n work']
      phy2[label = 'intimate partner violence \n and lack of \n reproductive rights', style = filled, fillcolor = White]
      
      men[label = 'pressure to confrom to \n societal expectations']
      pov[label = 'gendered risk of poverty']
     
      acc[label = 'gendered differential \n accesss to healthcare']
      birth[label = 'low birthweight \n and large for gestational age \n    babies', style = filled, fillcolor = White]
      cult[label= 'gender biased health systems, \n institutions and research']
      
      RF[label = 'risk factors for CVD (smoking, \n alcohol, obesity, physical activity, nutrition)', style = filled, fillcolor = White]
      stress[label = 'stress and mental \n health problems', style = filled, fillcolor = White]
      
      
#confounding
      node[style = filled, fillcolor = HoneyDew]
       
      confhealthcare[label = 'population level \n healthcare resource']
      confMMR[label = 'maternal mortality \n ratio']
      confGDP[label = 'GDP \n per capita']
      confedu[label = 'population level of \n education']
      
#outcomes
      node[color = LightSlateGrey, style = filled, penwidth = 1, fillcolor = White]
      cvdrate[label = 'population rates of CVD']
      
      cvdmort[label = 'population CVD mortaltiy \n and female CVD mortality', fillcolor= MistyRose]
      
      LE[label = 'female life expectancy', fillcolor= MistyRose
      ]
      

    
      
#edges
      
      gsn -> {pol edu eco phy} [arrowhead = none]
      
      edu->edu2 [dir=both]
      eco->eco2 [dir=both]
      phy->phy2 [dir=both]
      pol->pol2 [dir=both]
      
      {pol2 edu2 eco2} -> men
      
      {pol2 edu2 eco2} -> pov [dir=both]
      
      pol2 -> {cult edu2 eco2}[dir=both]
      
      phy2 -> pol2 [dir=both]
      
{edu2 eco2} -> cult
      

      
      {edu2 eco2 phy2} -> acc
      
      acc -> birth
      
      cult -> acc
      
      acc -> RF
      
      {pol2 edu2 eco2 phy2} -> RF
      
      {eco2 phy2} -> stress
      
      pov -> RF [dir=both]
      
      pov -> stress [dir=both]
      
      men -> {RF stress}
      
      pol2 -> acc
      
      phy2 -> cult [dir=both]
      
      birth -> RF
      
      stress -> RF [dir=both]
      
      {RF stress acc birth cult} -> cvdrate
      
      cvdrate -> cvdmort
      
      cvdmort -> LE
      
      cult -> cvdmort
      
      acc -> cvdmort
      
      pov -> acc
      
      {confhealthcare confMMR} -> LE
      {confhealthcare confGDP confedu} -> cvdmort
      {confhealthcare confGDP confedu} -> cvdrate
      
      confGDP -> {confedu confhealthcare confMMR}
      confhealthcare -> confMMR
      confedu -> confhealthcare
    
      
      subgraph cluster_gsn {
     graph [color = Bisque
     label = Outcomes 
     fontcolor = White
     penwidth = 2
     fontsize = 0
     style=invis
     ] 
     gsn pol eco edu phy
      }
      
      subgraph cluster_RF {
      style = filled
      fillcolor = seashell1
      color = White
      penwidth = 2
      label = RFs 
     fontcolor = White
     fontsize = 0
      birth RF phy2 stress
      }
       
      
      
      subgraph cluster_outcomes {
     color = White
     penwidth = 2
     label = Outcomes 
     fontcolor = White
     fontsize = 0
     style = invis
     cvdrate cvdmort LE 
      }
      
      
     subgraph cluster_conf {
     style = filled
     fillcolor = Grey95
     color = '#99FFCC'
     label = 'Country level variables' 
     fontcolor = '#009966'
     penwidth = 2
     fontsize = 16
     confGDP confedu confhealthcare confMMR
     }
      
      
      
      
      }
      
      ")
```

<div id="htmlwidget-5e3d26d0afdb8f594fd9" style="width:672px;height:480px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-5e3d26d0afdb8f594fd9">{"x":{"diagram":" \n      \n      digraph 2.0 {\n  #appearance\n \n      graph[\n  label = \"Conceptual Model\n  \"\n  fontname = Helvetica\n  labelloc = t\n  fontsize = 30\n      nodesep= 0.3,\n      ranksep= 0.8,\n      ]\n      node[\n      shape = box,\n      fontcolor = Black\n      fontname = Helvetica\n      fontsize = 18\n      color = LightSlateGrey\n      ]\n      \n      edge[ \n      color = LightSlateGrey\n      ]\n      \n  #nodes\n      \n      node[style = filled, fillcolor = Bisque]\n      gsn [label = \"biased gender \n social norms\", fontsize = 20]\n      \n      node[penwidth = 1, fillcolor= Bisque]\n      pol [label = \"gender bias in \n political domain\"]\n      edu [label = \"gender bias in \n educational domain\"]\n      eco [label = \"gender bias in \n economic domain\"]\n      phy [label = \"gender bias in \n physical domain\"]\n      \n      node[penwidth = 1, color = LightSlateGrey, style = notfilled]\n      pol2[label = \"gendered differences \n in power\"]\n      edu2[label = \"gendered differences \n in education\"]\n      eco2[label = \"gendered differences in \n work\"]\n      phy2[label = \"intimate partner violence \n and lack of \n reproductive rights\", style = filled, fillcolor = White]\n      \n      men[label = \"pressure to confrom to \n societal expectations\"]\n      pov[label = \"gendered risk of poverty\"]\n     \n      acc[label = \"gendered differential \n accesss to healthcare\"]\n      birth[label = \"low birthweight \n and large for gestational age \n    babies\", style = filled, fillcolor = White]\n      cult[label= \"gender biased health systems, \n institutions and research\"]\n      \n      RF[label = \"risk factors for CVD (smoking, \n alcohol, obesity, physical activity, nutrition)\", style = filled, fillcolor = White]\n      stress[label = \"stress and mental \n health problems\", style = filled, fillcolor = White]\n      \n      \n#confounding\n      node[style = filled, fillcolor = HoneyDew]\n       \n      confhealthcare[label = \"population level \n healthcare resource\"]\n      confMMR[label = \"maternal mortality \n ratio\"]\n      confGDP[label = \"GDP \n per capita\"]\n      confedu[label = \"population level of \n education\"]\n      \n#outcomes\n      node[color = LightSlateGrey, style = filled, penwidth = 1, fillcolor = White]\n      cvdrate[label = \"population rates of CVD\"]\n      \n      cvdmort[label = \"population CVD mortaltiy \n and female CVD mortality\", fillcolor= MistyRose]\n      \n      LE[label = \"female life expectancy\", fillcolor= MistyRose\n      ]\n      \n\n    \n      \n#edges\n      \n      gsn -> {pol edu eco phy} [arrowhead = none]\n      \n      edu->edu2 [dir=both]\n      eco->eco2 [dir=both]\n      phy->phy2 [dir=both]\n      pol->pol2 [dir=both]\n      \n      {pol2 edu2 eco2} -> men\n      \n      {pol2 edu2 eco2} -> pov [dir=both]\n      \n      pol2 -> {cult edu2 eco2}[dir=both]\n      \n      phy2 -> pol2 [dir=both]\n      \n{edu2 eco2} -> cult\n      \n\n      \n      {edu2 eco2 phy2} -> acc\n      \n      acc -> birth\n      \n      cult -> acc\n      \n      acc -> RF\n      \n      {pol2 edu2 eco2 phy2} -> RF\n      \n      {eco2 phy2} -> stress\n      \n      pov -> RF [dir=both]\n      \n      pov -> stress [dir=both]\n      \n      men -> {RF stress}\n      \n      pol2 -> acc\n      \n      phy2 -> cult [dir=both]\n      \n      birth -> RF\n      \n      stress -> RF [dir=both]\n      \n      {RF stress acc birth cult} -> cvdrate\n      \n      cvdrate -> cvdmort\n      \n      cvdmort -> LE\n      \n      cult -> cvdmort\n      \n      acc -> cvdmort\n      \n      pov -> acc\n      \n      {confhealthcare confMMR} -> LE\n      {confhealthcare confGDP confedu} -> cvdmort\n      {confhealthcare confGDP confedu} -> cvdrate\n      \n      confGDP -> {confedu confhealthcare confMMR}\n      confhealthcare -> confMMR\n      confedu -> confhealthcare\n    \n      \n      subgraph cluster_gsn {\n     graph [color = Bisque\n     label = Outcomes \n     fontcolor = White\n     penwidth = 2\n     fontsize = 0\n     style=invis\n     ] \n     gsn pol eco edu phy\n      }\n      \n      subgraph cluster_RF {\n      style = filled\n      fillcolor = seashell1\n      color = White\n      penwidth = 2\n      label = RFs \n     fontcolor = White\n     fontsize = 0\n      birth RF phy2 stress\n      }\n       \n      \n      \n      subgraph cluster_outcomes {\n     color = White\n     penwidth = 2\n     label = Outcomes \n     fontcolor = White\n     fontsize = 0\n     style = invis\n     cvdrate cvdmort LE \n      }\n      \n      \n     subgraph cluster_conf {\n     style = filled\n     fillcolor = Grey95\n     color = \"#99FFCC\"\n     label = \"Country level variables\" \n     fontcolor = \"#009966\"\n     penwidth = 2\n     fontsize = 16\n     confGDP confedu confhealthcare confMMR\n     }\n      \n      \n      \n      \n      }\n      \n      ","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

``` r
grViz(" 
      
      digraph key {
  #appearance
     subgraph clusterkey {
     graph[
     font = Helvetica
      nodesep= 0.3,
      ranksep= 0.8,
      label = Key
      labelloc = t
  fontsize = 15
  fontname = Helvetica
  fontcolor = Black
      color = LightSlateGrey
      ]
      node[
      shape = box,
      fontcolor = Black
      fontname = Helvetica
      fontsize = 10
      color = LightSlateGrey
      ]
     
     
     #key
    gsnkey[label = 'gender social norms', style = filled, color = LightSlateGrey, fillcolor = Bisque]
    conf[label = 'country level variables', style = filled, fillcolor= HoneyDew]
    outcomes[label = 'outcomes', style = filled, fillcolor = MistyRose ]
    cvd[label='Acronyms: CVD - Cardiovascular Disease', color = none, fontsize = 8]
    downstream[label = 'downstream \n risk factors' , style = filled, fillcolor = White]
     
     
     gsnkey -> downstream -> conf -> outcomes -> cvd [style = invis]
     
     subgraph cluster_down{
     style = filled
      fillcolor = seashell1
      color = White
      penwidth = 2
      label = RFs 
     fontcolor = White
     fontsize = 0
      downstream
     }
      
         }}


")
```

<div id="htmlwidget-b592d626d97ed2339b24" style="width:672px;height:480px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-b592d626d97ed2339b24">{"x":{"diagram":" \n      \n      digraph key {\n  #appearance\n     subgraph clusterkey {\n     graph[\n     font = Helvetica\n      nodesep= 0.3,\n      ranksep= 0.8,\n      label = Key\n      labelloc = t\n  fontsize = 15\n  fontname = Helvetica\n  fontcolor = Black\n      color = LightSlateGrey\n      ]\n      node[\n      shape = box,\n      fontcolor = Black\n      fontname = Helvetica\n      fontsize = 10\n      color = LightSlateGrey\n      ]\n     \n     \n     #key\n    gsnkey[label = \"gender social norms\", style = filled, color = LightSlateGrey, fillcolor = Bisque]\n    conf[label = \"country level variables\", style = filled, fillcolor= HoneyDew]\n    outcomes[label = \"outcomes\", style = filled, fillcolor = MistyRose ]\n    cvd[label=\"Acronyms: CVD - Cardiovascular Disease\", color = none, fontsize = 8]\n    downstream[label = \"downstream \n risk factors\" , style = filled, fillcolor = White]\n     \n     \n     gsnkey -> downstream -> conf -> outcomes -> cvd [style = invis]\n     \n     subgraph cluster_down{\n     style = filled\n      fillcolor = seashell1\n      color = White\n      penwidth = 2\n      label = RFs \n     fontcolor = White\n     fontsize = 0\n      downstream\n     }\n      \n         }}\n\n\n","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
