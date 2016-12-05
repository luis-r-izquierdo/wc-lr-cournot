;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; wc-lr-cournot is a model designed to analyse
;; the “Win-Continue, Lose-Reverse” rule in Cournot oligopolies
;; Copyright (C) 2014 Segismundo S. Izquierdo & Luis R. Izquierdo
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Contact information:
;; Segismundo S. Izquierdo 
;;   University of Valladolid, Spain. 
;;   e-mail: segis@eii.uva.es


breed [firms firm]

;;;;;;;;;;;;;;;;;
;;; Variables ;;;
;;;;;;;;;;;;;;;;;

globals [
  price              ;; market price
  price-Cournot      ;; Cournot equilibrium price
  price-collusion    ;; collusion price
  price-factor
  
  q-total
  q-total-Cournot
  q-total-collusion
]

firms-own [ 
  q              ;; the chosen level of production or quantity
  last-q
  q-Cournot
  q-collusion
  
  profit
  last-profit
  
  increase?       ;; will take value true if the company is to increase output. false if to decrease
  
  cost-factor   ;; a number by which the cost of this firm will be multiplied
  my-price-factor
]



;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; the following procedure is called when the model is first loaded

to startup
  clear-all
  
  create-firms num-firms [
    set q-Cournot   (p0 - c1) / (a * num-firms + a + 2 * c2)
    set q-collusion (p0 - c1) / (2 * a * num-firms + 2 * c2)     
    set q (round (random-float (p0 / (a * num-firms)) / step)) * step
    set my-price-factor 1   
    set cost-factor 1      
    set hidden? true
  ]
  
  set price-factor 1
  
  compute-total-quantities-and-prices
  
  setup-graphs
  
  reset-ticks 
  no-display
end

to setup-graphs
  
  set-current-plot "Prices" 
  set-plot-y-range floor price-Cournot ceiling price-collusion
  
  set-current-plot "Quantity 1 vs. Quantity 2"
  set-plot-x-range floor [q-collusion] of firm 0 ceiling [q-Cournot] of firm 0
  set-plot-y-range floor [q-collusion] of firm 1 ceiling [q-Cournot] of firm 1
  
end

to-report price-for [x]
  let potential-price (p0 - a * x)
  report ifelse-value (potential-price > 0) [potential-price][0]
end

to-report cost-for [x]
    report (c0 + c1 * x + c2 * x * x)
end  

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run-time procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
   
  compute-total-quantities-and-prices
  
  ask firms [update-profit]
  
  tick
  
  set price-factor (1 + alpha * random-value * price-variability)
  ask firms [set cost-factor (1 + random-value * cost-variability)]
  
  ask firms [compute-quantities]
  
  if (count firms != num-firms) [adjust-num-firms]   
end

to compute-total-quantities-and-prices
  
  set q-total sum [q] of firms
  set q-total-Cournot sum [q-Cournot] of firms
  set q-total-collusion sum [q-collusion] of firms
  
  ask firms [ set my-price-factor (price-factor + (1 - alpha) * random-value * price-variability) ]
        
  set price            price-factor * price-for q-total
  set price-Cournot    price-factor * price-for q-total-Cournot
  set price-collusion  price-factor * price-for q-total-collusion 
  
end

to update-profit
  set profit my-price-factor * price * q - cost-factor * cost-for q
end

to compute-quantities
  
  let tmp (profit - last-profit) * (q - last-q)
  set increase? (tmp > 0)
  let random? ((random-float 1 < p-random) or ticks = 1 or tmp = 0)  
  
  set last-q q
  set last-profit profit
  
  set q ifelse-value random? 
   [ q + step * (random 3 - 1) ] 
   [ q + step * ifelse-value increase? [1] [-1] ]  
  
  if q < 0 [set q 0]
  
  set q-Cournot   (my-price-factor * p0 - cost-factor * c1) / (my-price-factor * a * num-firms + my-price-factor * a + 2 * cost-factor * c2)
  set q-collusion (my-price-factor * p0 - cost-factor * c1) / (2 * my-price-factor * a * num-firms + 2 * cost-factor * c2)
   
end

to adjust-num-firms
  
  let adjustment (num-firms - (count firms))
 
  ifelse adjustment > 0 
    [
      set q-total-collusion q-total-collusion * (p0 - c1) / (2 * c2 + 2 * a * num-firms) 
      create-firms adjustment [
        set q-Cournot   (p0 - c1) / (a * num-firms + a + 2 * c2)
        set q-collusion (p0 - c1) / (2 * a * num-firms + 2 * c2)     
        set q (round (random-float (p0 / (a * num-firms)) / step)) * step
        set my-price-factor 1   
        set cost-factor 1      
        set hidden? true  
      ]
    ]
    [ ask n-of (0 - adjustment) (firms with [who != 0 and who != 1]) [die] ]

end

to-report random-value
  report ifelse-value (random-distribution = "Uniform [-1, 1]")
  [(random-float 2) - 1]
  [random-normal 0 (1 / sqrt(3))]
end
@#$#@#$#@
GRAPHICS-WINDOW
555
241
800
443
16
16
5.2
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
9
44
121
77
p0
p0
100
1000
100
10
1
NIL
HORIZONTAL

SLIDER
124
44
228
77
a
a
0
10
1
0.1
1
NIL
HORIZONTAL

SLIDER
505
19
662
52
num-firms
num-firms
2
50
2
1
1
NIL
HORIZONTAL

BUTTON
930
25
1007
58
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
11
215
533
598
Prices
NIL
NIL
0.0
10.0
0.0
0.0
true
true
"" ""
PENS
"price" 1.0 0 -16777216 true "" "plot price"
"price collusion" 1.0 0 -2674135 true "" "plot price-collusion"
"price Cournot" 1.0 0 -13345367 true "" "plot price-Cournot"

SLIDER
130
141
243
174
c1
c1
0
10
10
1
1
NIL
HORIZONTAL

SLIDER
252
141
362
174
c2
c2
0
5
0.1
.1
1
NIL
HORIZONTAL

SLIDER
9
81
227
114
price-variability
price-variability
0
0.1
0.021
0.001
1
NIL
HORIZONTAL

TEXTBOX
15
10
482
46
Inverse Demand: \np = (p0 - a * Q) * {1 + price-variability * (alpha * RDcommon + (1 - alpha) * RDindep)}
11
0.0
1

TEXTBOX
15
121
372
149
Costs = (c0 + c1 * q + c2 * q^2) * (1 + cost-variability * RD)
11
0.0
1

SLIDER
504
94
661
127
p-random
p-random
0
1
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
505
56
661
89
step
step
0.001
2
0.1
0.001
1
NIL
HORIZONTAL

MONITOR
940
67
1008
112
NIL
ticks
17
1
11

BUTTON
849
25
926
58
Go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
769
24
841
57
Set up
startup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
538
215
1012
597
Quantity 1 vs. Quantity 2
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"actual q" 1.0 0 -16777216 true "" "plotxy [q] of firm 0 [q] of firm 1"
"q collusion" 1.0 0 -2674135 true "" "plotxy [q-collusion] of firm 0 [q-collusion] of firm 1"
"q Cournot" 1.0 0 -13345367 true "" "plotxy [q-Cournot] of firm 0 [q-Cournot] of firm 1"

BUTTON
830
173
1012
206
Reset plots
clear-all-plots\nsetup-graphs
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
918
350
1007
395
NIL
q-total-collusion
0
1
11

SLIDER
11
177
231
210
cost-variability
cost-variability
0
0.1
0
0.001
1
NIL
HORIZONTAL

MONITOR
918
399
1007
444
NIL
q-total-Cournot
2
1
11

MONITOR
449
320
527
365
NIL
price
2
1
11

MONITOR
450
367
527
412
NIL
price-collusion
2
1
11

MONITOR
449
414
527
459
NIL
price-Cournot
2
1
11

MONITOR
918
302
1007
347
NIL
q-total
2
1
11

SLIDER
231
81
362
114
alpha
alpha
0
1
0
0.01
1
NIL
HORIZONTAL

SLIDER
11
141
123
174
c0
c0
0
2000
0
100
1
NIL
HORIZONTAL

CHOOSER
505
132
661
177
random-distribution
random-distribution
"Uniform [-1, 1]" "Normal [0, 1/3]"
0

TEXTBOX
472
152
502
170
RD:
11
0.0
1

MONITOR
920
467
1002
512
profit 1
[profit] of firm 0
1
1
11

MONITOR
921
516
1002
561
profit 2
[profit] of firm 1
1
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
