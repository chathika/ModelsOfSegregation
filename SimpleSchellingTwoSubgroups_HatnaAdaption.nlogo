;; An implementation of the Schelling model of Segergation
;; By Erez Hatna (erezh51@gmail.com)
;; Updated with integration to clustering algorithm by Chathika Gunaratne <chathikagunaratne@gmail.com>
__includes ["util/clustering.nls"]
extensions [array]
globals [
  empty-patches-array;; an array of unoccupied patches
]

turtles-own [
  home-patch; a reference to the home patch
  home-utility; the utility of the home patch
  color-group; the group membership (1 or 2), represent the turtles color
  tolerance-group; the tolerance group (either 1 and 2)

  ;;;;variables important to clustering algorithm;;;;
  cluster ; The cluster id this turtle belongs to
  happy?
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
]

patches-own [
  resident;; a pointer to the resident turtle (if any)
  neighboring-patches; a list of all neighboring patches
  c-index-area ;; Used to calculate the c-index
]


to setup
  clear-all
  ask patches [
    ;; setting residet as null
    set resident nobody
    set neighboring-patches get-neighbors
  ]
  ;; creating the turtles
  let number-of-turtles density * count patches
  ask n-of number-of-turtles patches [
    sprout 1 [
      set color-group 2; 2 represents green turtles
      set tolerance-group 2 ;; setting the subgroup membership
      set home-patch myself ;; myself is the calling patch
      set resident self ;; the patch is now occupied by the turtle
    ]
  ]
  ;; setting the group code of the code group 1 ("blue") turtles
  let no-of-blue-turtles fraction-of-blue * count turtles
  ask n-of no-of-blue-turtles turtles [
    set color-group 1
  ]

  ;; setting the membership of subgroup 1 turtles
  let no-of-subgroup-1-turtles fraction-of-subgroup-1 *  count turtles

  ask n-of no-of-subgroup-1-turtles turtles [
   set tolerance-group 1
  ]

  set-turtles-color
  set empty-patches-array array:from-list sort patches with [resident = nobody]

  if update-turtles-appearance? [
    update-turtles-appearance
  ]

  reset-ticks
end


;; Patch procedure
;; Reports a list of neighboring patches of the calling patch.

to-report get-neighbors
  let neighbors-list [];; an enpty list to store the neighboring patches

  let x-counter pxcor - neighborhood-distance
  while [x-counter <= pxcor + neighborhood-distance] [
    let y-counter  pycor - neighborhood-distance
    while [y-counter <= pycor + neighborhood-distance] [
      let x x-counter
      let y y-counter

      ;; a patch is not a neighbor of itself
      if not (pxcor = x and pycor = y) [
        ;; adding the patch to the neighbors list
        set neighbors-list sentence neighbors-list patch x y
      ] ;; end if
      set y-counter y-counter + 1
    ] ;; end while
    set x-counter x-counter + 1
  ] ;; end while
  report  neighbors-list
end

;; Sets the colors of the turtles according to ther group and subgroup
to set-turtles-color
  ask turtles [
    set shape "square"
    ifelse color-group = 1 [
      set color blue
    ]
    [
      set color green
    ]

    if tolerance-group = 2 [
      set color color - 2
    ]
  ]
end

;; Changes the shapes: a square for happy turtles
;; and a circle for unhappy ones.
to update-turtles-appearance
  ask turtles [
    ifelse calc-utility home-patch = 1 [
      set shape "square 3"
    ]
    [
      set shape "circle"
    ]
  ]
end


to go
  ask turtles [
    if interested-to-relocate? [
      try-to-relocate
    ]
  ]

  if update-turtles-appearance? [
    update-turtles-appearance
  ]
  tick

  if ticks >= stopping-time [
    stop
  ]
end


;; turtle procedure
;; reports true if the turtle is intersted to relocate
to-report interested-to-relocate?
  ;; the utility of the home patch
  set home-utility calc-utility home-patch
  report
    home-utility < 1 or
      (home-utility = 1 and random-float 1 < prob-of-relocation-attempt-by-happy)
end


;; turtle procedure
to try-to-relocate

  ;; The turtle considers a given fraction of the empty patches
  let no-of-patches-to-evaluate empty-cells-to-evaluate-frac * array:length empty-patches-array

  ;; shuffeling the first "no-of-patches-to-evaluate" patches
  shuffle-empty-patches-array (no-of-patches-to-evaluate)

  let utility-of-best-patch -1
  let index-of-best-patch -1 ;; the index in the empty-patches-array
  let counter 0

  ;; Looking for the patch with the highest utility
  while [counter < no-of-patches-to-evaluate] [
    let patch-to-evaluate array:item empty-patches-array counter
    let utility calc-utility patch-to-evaluate
    if (utility >= utility-of-best-patch)
       [
         set utility-of-best-patch utility
         set index-of-best-patch counter
       ]
     set counter counter + 1

  ]

  ;; if the turte has found a patch
  if index-of-best-patch != -1 [
     ;; Unhappy turtles move if the new patch is better than their home patch
     if home-utility < 1 and utility-of-best-patch > home-utility [
       relocate-to index-of-best-patch
     ]
     ;; Happy turtles will only move to another good patch
     if home-utility = 1 and utility-of-best-patch = 1 [
       relocate-to index-of-best-patch
     ]
   ]
end


;; turtle procedure
;; relocates the turtle to the given patch and updates the empty patches array
to relocate-to [index-of-patch]
  let destination-patch array:item empty-patches-array index-of-patch
  ;; copy the home-patch to the array instead of destination-patch
  array:set empty-patches-array index-of-patch (home-patch)

  ;; the old home patch should no longer be occupied
  ask home-patch [
    set resident nobody
  ]

  ;; The new destination patch is now occupied
  ask destination-patch [
    set resident myself
  ]

  ;; the turtle should point to its new patch
  set home-patch destination-patch
  ;; The turtle loctates itself
  ;; in the center of the new patch
  move-to destination-patch
end

;; turtle procedure
;; the turtle evaluates the utility of a given patch
to-report calc-utility [patch-to-evaluate]
  let fraction calc-fraction-of-friends patch-to-evaluate
  let min-desired-fraction 0
  ifelse tolerance-group = 1 [
    set min-desired-fraction min-desired-fraction-of-subgroup-1
  ]
  [
    set min-desired-fraction min-desired-fraction-of-subgroup-2
  ]

  ifelse fraction < min-desired-fraction    [
    report fraction / min-desired-fraction
  ]
  [
    report 1; 1 represents an happy turtle
  ]
end

;; Turtle Procedure
;; The turtle calculates the fraction of friends in the neighborhood of a given patch
to-report calc-fraction-of-friends [patch-to-evaluate]
  let neighbor-patches [neighboring-patches] of patch-to-evaluate
  let no-of-friends 0
  let no-of-neighbors 0

  foreach neighbor-patches [
    [cur-patch] ->
    let neighbor-resident [resident] of cur-patch
    if neighbor-resident != nobody [
      set no-of-neighbors no-of-neighbors + 1
      if [color-group] of neighbor-resident = color-group [
        set no-of-friends no-of-friends + 1
      ]
    ]
  ]

  ifelse no-of-neighbors > 0 [
    report no-of-friends /  no-of-neighbors
  ]
  [
    report 0 ;; In case the neighborhood is empty, report 0.
  ]
end

;; randomizes the order of the empty patch array
;; only the first "no-of-element" are randomized
to shuffle-empty-patches-array [no-of-elements]
  let first-place 0
  let last-place array:length empty-patches-array - 1

  while [first-place < no-of-elements] [
    ;; pick a random index between first-place and last-place
    let rand-index first-place + random (last-place - first-place + 1)

    ;; swap the element at rand-index with the element at first place
    let temp array:item empty-patches-array first-place
    array:set empty-patches-array first-place (array:item empty-patches-array rand-index)
    array:set empty-patches-array rand-index (temp)
    set first-place first-place + 1
  ]
end


;; Calculates the Moran's I index (spatial autocorrelation of the color pattern)
;; calc-by-group?: if true, the index is caclulated by group, if false it is calculated by subgroup (toleance)
to-report moran-I [calc-by-group?]
  let n 0 ; number of turtles with at least one neighbor
  let sum-of-values 0
  let sum-of-squares 0


  ask turtles ; calculating the average value, variation and n:
  [
    foreach neighboring-patches [
      cur-patch ->
      let neighbor-resident [resident] of cur-patch
      if neighbor-resident != nobody [
        ask neighbor-resident [
          let value 0
          ifelse calc-by-group? [
            set value color-group
          ]
          [
            set value tolerance-group
          ]

          set n n + 1
          set sum-of-values sum-of-values + value
          set sum-of-squares sum-of-squares + value * value
        ]
      ]
    ]
  ]

  let average sum-of-values / n
  let variation sum-of-squares - sum-Of-Values * sum-of-values / n


  ; calculating the covariation
  let covariation 0
  let sum-of-weights  0

  ask turtles
  [

    let no-Of-neighbors length neighboring-patches ; no of turtles in the neighborhood
    let current-turtle-value 0

    ifelse calc-by-group? [
      set current-turtle-value color-group
    ]
    [
      set current-turtle-value tolerance-group
    ]


    foreach neighboring-patches [
      [cur-patch] ->
      let neighbor-resident [resident] of cur-patch
      if neighbor-resident != nobody [
        ask neighbor-resident [
          ;; "current-turtle-value" is the value of the turtle that is in the "center" while
          ;; "neighbor-value" is the value of one of its neighbors:

          let neighbor-value 0
          ifelse calc-by-group? [
            set neighbor-value color-group
          ]
          [
            set neighbor-value tolerance-group
          ]

          set covariation covariation + (current-turtle-value - average) * (neighbor-value - average) / no-of-neighbors
          set sum-of-weights sum-Of-weights + 1 / no-of-Neighbors
        ]
      ]
    ]
  ]

  ifelse variation * sum-of-weights > 0 [
    report n * covariation / (variation * sum-of-weights)
  ]
  [
    report -1
  ]
end

;; reports the c-index
to-report c-index
  mark-areas
  let size-of-blue-patch 0
  let size-of-green-patch 0
  let size-of-integrated-patch 0
  let total-size 0

  ask patches [

    set total-size total-size + 1
    if c-index-area = 1 [
      set size-of-blue-patch size-of-blue-patch + 1
    ]

    if c-index-area = 2 [
      set size-of-green-patch size-of-green-patch + 1
    ]

    if c-index-area = 3 [
      set size-of-integrated-patch size-of-integrated-patch + 1
    ]
  ]


  report (min (list (size-of-blue-patch) (size-of-green-patch) (size-of-integrated-patch))) / total-size

end


;; Marks the C index areas:
;; 1 represents homogeneous Blue area
;; 2 represents homogeneous Green area
;; 3 represents an integrated area
;; 4 represents the boundary
to mark-areas
  ask patches
  [
    set c-index-area 3 ; before marking the turtles, we set the values of all of them to 3 (which represents integrated area)
  ]

  mark-homogeneous-areas
  mark-homogeneous-boundary

end





;; marks pathces which are in homogeneous area for a given agent group
;; A patch is marked if the following conditions are met:
;;       1. It is occupied by a turtle
;;       2. The neighborhood is not empty and all the neighbors belong to the group of the turtle
;; The patches are marked by setting the c-index-area variable to the turtle's group code

to mark-homogeneous-areas
  ask patches [
    if resident != nobody [
      ask resident [
       if calc-fraction-of-friends myself = 1 [
         set c-index-area color-group
       ]
      ]
    ]
  ]
end


;; Marks by 4 any patch with c-index-area =3 which has at least one neighbor which is part of
;; the homogeneous area (marked by 1 or 2, i,e, the group code of the turtles)
to mark-homogeneous-boundary
  ask patches with [c-index-area = 3] [
   let no-of-homo-neighbors 0
   foreach neighboring-patches [
      [cur-patch] ->
     ask cur-patch [
       if c-index-area = 1 or c-index-area = 2  [
         set no-of-homo-neighbors no-of-homo-neighbors + 1
       ]
     ]
   ]
   if no-of-homo-neighbors > 0 [
     set c-index-area 4
   ]

  ]
end



to color-areas
  mark-areas
  ask patches [
    if c-index-area = 1 [
      set pcolor blue
    ]

    if c-index-area = 2 [
      set pcolor green
    ]

    if c-index-area = 3 [
      set pcolor red
    ]

    if c-index-area = 4 [
      set pcolor grey
    ]
]
end

;; saves the patches information as ESRI ascii grid
;; the group code of turtle is saved. If a patch is empty, a -1 (no data) is saved
;; Todo: enable saving the tolerance pattern
to save-as-ascii-grid [file-name]
  file-open file-name
  ;; saving the header
  file-print (word "ncols         " (max-pycor - min-pycor + 1)  "\r") ;; nunber of rows
  file-print (word "nrows         " (max-pxcor - min-pxcor + 1) "\r") ;; nunber of columns
  file-print "xllcorner     0\r"
  file-print "yllcorner     0\r"
  file-print "cellsize      1\r"
  file-print "NODATA_value  -1\r"


  let x-counter min-pxcor
  while [x-counter <= max-pxcor] [
    let y-counter min-pycor
    while [y-counter <= max-pycor] [
      let cur-value -1
      ;; if this patch is occupied by a turtle
      if [resident] of patch x-counter y-counter != nobody [
        set cur-value [color-group] of [resident] of patch x-counter y-counter
      ]
      file-print (word cur-value "\r")
      set y-counter y-counter + 1
    ]
    set x-counter x-counter + 1
  ]

  file-close
end

;; assign the tolerance of turtles according to a given distribution
;; set-of-turtles: an agentset of turtles
;; distrib-list: a list of pairs (a list of lists):
;; The first element of the list is the tolerance while the second is the frequency of that toleance in the agentset population
to set-tolerance-distribution [set-of-turtles distrib-list]

end
@#$#@#$#@
GRAPHICS-WINDOW
209
12
556
360
-1
-1
6.65
1
10
1
1
1
0
1
1
1
0
50
0
50
1
1
1
ticks
30.0

BUTTON
78
15
145
48
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
14
54
203
87
density
density
0
1
0.98
0.01
1
NIL
HORIZONTAL

SLIDER
13
94
203
127
fraction-of-blue
fraction-of-blue
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
10
392
343
425
min-desired-fraction-of-subgroup-1
min-desired-fraction-of-subgroup-1
0
1
0.185
0.005
1
NIL
HORIZONTAL

SLIDER
14
215
199
248
prob-of-relocation-attempt-by-happy
prob-of-relocation-attempt-by-happy
0
1
0.01
0.01
1
NIL
HORIZONTAL

BUTTON
115
327
178
360
NIL
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
566
12
837
150
plot 1
NIL
NIL
0.0
100.0
0.0
1.0
true
true
"" ""
PENS
"IC" 1.0 0 -5298144 true "" "if update-graph?  [\n  let m moran-I true\n  if m != -1 [\n    plotxy ticks m\n  ]\n]"
"IT" 1.0 0 -14730904 true "" "if update-graph? [\n  let m moran-I false\n  if m != -1 [\n    plotxy ticks m\n  ]\n]"
"C" 1.0 0 -15040220 true "" "if update-graph?  [\n  plotxy ticks c-index\n]"

SLIDER
14
173
201
206
neighborhood-distance
neighborhood-distance
1
8
2.0
1
1
NIL
HORIZONTAL

SLIDER
10
432
343
465
min-desired-fraction-of-subgroup-2
min-desired-fraction-of-subgroup-2
0
1
0.86
0.005
1
NIL
HORIZONTAL

SLIDER
12
133
205
166
fraction-of-subgroup-1
fraction-of-subgroup-1
0
1
0.5
0.01
1
NIL
HORIZONTAL

BUTTON
60
470
194
503
Copy Tolerance
set min-desired-fraction-of-subgroup-2 min-desired-fraction-of-subgroup-1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
350
434
589
467
update-turtles-appearance?
update-turtles-appearance?
0
1
-1000

BUTTON
839
50
996
83
test neighborhoods
ask patches [\n  set pcolor black\n]\n\nask patch 3 23 [\n  set pcolor yellow\n  foreach neighboring-patches [\n    [cur-patch] ->\n    ask cur-patch [\n      set pcolor blue\n    ]\n  ]\n]
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
838
10
942
43
test shuffle
shuffle-empty-patches-array 10\nprint empty-patches-array
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
373
392
530
425
show-hide-c-areas
ask turtles [set hidden? not hidden?]\nifelse [hidden?] of turtle 0 [ \n  color-areas\n]\n[\n  ask patches [\n    set pcolor black\n  ]\n  \n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
636
320
788
353
update-graph?
update-graph?
0
1
-1000

INPUTBOX
9
316
99
376
stopping-time
50000.0
1
0
Number

PLOT
564
160
839
310
Distribution of Number of Friends
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 0.1 1 -16777216 true "" "if update-graph? [ \n  histogram [calc-fraction-of-friends patch-here] of turtles\n]"

SLIDER
0
263
209
296
empty-cells-to-evaluate-frac
empty-cells-to-evaluate-frac
0
1
0.2
0.05
1
NIL
HORIZONTAL

PLOT
844
109
1119
264
Cluster Count
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -7500403 true "" "plot length remove-duplicates [cluster] of turtles with [ cluster > 0]"

BUTTON
842
268
927
301
Cluster
ask turtles[set happy? ifelse-value (home-utility = 1)[true][false]]\nclustering
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
17
545
534
605
tolerance-dist
let x  list (list 0.18 0.5) (list 0.8 0.5)
1
0
String

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

square 3
false
0
Rectangle -7500403 true true 0 0 300 300

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
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Common F" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
set min-desired-fraction-of-subgroup-2 min-desired-fraction-of-subgroup-1</setup>
    <go>go</go>
    <timeLimit steps="50000"/>
    <metric>moran-I 1 true</metric>
    <metric>moran-I 1 false</metric>
    <metric>c-index 1</metric>
    <enumeratedValueSet variable="fraction-of-blue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-of-relocation-attempt-by-happy">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-time">
      <value value="50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-desired-fraction-of-subgroup-2">
      <value value="0.275"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-of-subgroup-1">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-distance">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density">
      <value value="0.98"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-turtles-appearance?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-regions?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="min-desired-fraction-of-subgroup-1" first="-0.001" step="0.041666667" last="1"/>
    <enumeratedValueSet variable="update-graph?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="F1=3/24" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="50000"/>
    <metric>moran-I 1 true</metric>
    <metric>moran-I 1 false</metric>
    <metric>c-index 1</metric>
    <enumeratedValueSet variable="fraction-of-blue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-of-relocation-attempt-by-happy">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopping-time">
      <value value="50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-desired-fraction-of-subgroup-1">
      <value value="0.124"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fraction-of-subgroup-1">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-distance">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density">
      <value value="0.98"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-turtles-appearance?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="two-regions?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="min-desired-fraction-of-subgroup-2" first="-0.001" step="0.041666667" last="1"/>
    <enumeratedValueSet variable="update-graph?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
