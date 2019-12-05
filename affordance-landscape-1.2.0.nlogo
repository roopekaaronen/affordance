;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:::::::::::;;;;;;;;;;;;;;;;:
;;;;;;; AFFORDANCE LANDSCAPE MODEL:                        ;;;
;;;;;;; CULTURAL EVOLUTION OF PRO-ENVIRONMENTAL BEHAVIORS  ;;;
;;;;;;; VERSION 1.2.0 on Netlogo 6.1.0                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; VARIABLES       ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals [
pro-behavior ;; Counts the total number of pro-environmental behaviors within one tick.
non-behavior] ;; Counts the total number of non-environmental behaviors within one tick.

turtles-own [
pro-env ;; The pro-environmental personal state (habits, attitudes, etc.) of the agent.
non-env ;; The non-environmental (environmentally harmful) personal state of the agent.
behaved? ;; A binary value (true or false) which indicates whether or not an agent has behaved somehow during the current tick.
lower-bound ;; lower bound for pro-env and non-env
upper-bound;; upper bound for pro-env and non-env
]

patches-own [
  affordance ] ;; Patches own an affordance variable. This variable has binary values (0 and 1). 1 is a pro-environmental affordance, 0 is a non-environmental affordance.

extensions [
  nw ] ;; The network extension is used in this model.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; SETUP PROCEDURE ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  if random-seed? = true [random-seed rseed] ;; If random-seed is true, the random seed is set to the value of rseed.
  clear-all ;; All patches and turtles are cleared before setup.
  set-default-shape turtles "circle" ;; Set the shape of turtles (purely cosmetic).
  ifelse networks = TRUE [ create-network ] ;; Creates a social network if networks is true. Otherwise just creates a set of unconnected turtles.
  [create-agents]
  create-traits ;; Sets the personal states for each turtle.
  create-aff ;; Creates patches.
  reset-ticks ;; Reset ticks before running model.
end

to create-agents ;; Creates number-of-agents turtles if networks is false (i.e., in case the model has no social networks).
  crt number-of-agents [
    setxy random-xcor random-ycor]
end

to create-traits ;; Procedure for giving each agent a pro-env and non-env value.
  ask turtles [
    set size 3
    set color white ;; Set initial turtle color to white (they are recoloured later in the Go procedure).
  ]

  foreach (list turtles) [[x] -> ask x [ ;; For each turtle in the list of all turtles...
    set pro-env random-normal initial-pro 0.15 ;; ...sample a random pro-env value from a normal distribution with mean of initial-pro and a standard deviation of 0.15.
    set non-env random-normal initial-non 0.15 ;; ...sample a random non-env value from a normal distribution with mean of initial-non and a standard deviation of 0.15.
    set lower-bound random-normal 0.2 0.05 ;; set a lower-bound and upper bound for pro-env and non-env.
    set upper-bound random-normal 0.8 0.05 ] ]
end

to create-aff ;; Creates patches, or affordances (opportunities for action).
let total-patches count patches ;; Count total patches.
  ask patches [
    set pcolor (sky)
    set affordance 0] ;; Ask all patches to first set affordance to 0 (and colour to the associated sky-blue).
  ask n-of (total-patches * pro-amount) patches [ ;; Then ask a random set of patches to set affordance to 1 (and color to violet). The proportion of pro-environmental affordances is designated by the parameter pro-amount.
    set pcolor (violet - 1)
    set affordance 1]
end

to create-network ;; Includes procedures for four kinds of networks.

if network-type = "random" [ ;; Creates one random network (Erdös-Renyi random network).
    crt number-of-agents ;; Create number-of-agents turtles.
    repeat (network-param * count turtles) / 2 [ ;; Divide by two (because a link connects two turtles).
      ask one-of turtles [
        create-link-with one-of other turtles with [ not link-neighbor? myself ] ;; Ask a random turtle to create link with another random turtle.
      ]
      layout-spring turtles links 0.2 4.0 500 ;; Lay the turtles out (mainly cosmetic).
    ]
  ]

if network-type = "small-world" [ ;; Creates a Watts-Strogatz small-world network (high clustering coefficient). Uses the algorithm from NetLogo network extension (https://ccl.northwestern.edu/netlogo/docs/nw.html).
nw:generate-watts-strogatz turtles links number-of-agents network-param 0.1 [ fd 10 ] ;; Structure: turtle-breed link-breed num-nodes neighborhood-size rewire-probability optional-command-block
    ask turtles [
    set size 3
    layout-spring turtles links 0.2 4.0 500 ;; reorganise the layout to highlight new network structure
    set color white ;; Set agent colour white to begin with. They are coloured black or red later.
    ]
  ]

if network-type = "preferential" [ ;; Creates a scale-free network with hubs (preferential attachment). This is the Barabási–Albert network model. Uses the algorithm from NetLogo network extension (https://ccl.northwestern.edu/netlogo/docs/nw.html).
nw:generate-preferential-attachment turtles links number-of-agents network-param [ ;; Structure: turtle-breed link-breed num-nodes min-degree optional-command-block
        repeat 3 [
   layout-spring turtles links 0.2 4.0 500 ;; Layout procedure (mainly cosmetic).
    display  ;; For smooth animation.
      ]
   ]
]


if network-type = "KE" [ ;; Creates a scale-free network with high clustering, the Klemm-Eguíluz model.
;; The following algorithm is adapted with permission from Fernando Sancho Caparrini's "Complex Networks Toolbox", see http://www.cs.us.es/~fsancho/?e=162#KE for details and a conceptual model.
  clear-all
  create-turtles network-param [ ;; The algorithm begins with an initial set of turtles. The number of initial turtles is defined by network-param. (This is m0 in the original KE algorithm.)
    set color red
  ]
  ask turtles [
    create-links-with other turtles
  ]
  let active turtles with [self = self]
  let no-active no-turtles
  repeat (number-of-agents - network-param) [
    create-turtles 1 [
      set color white
      foreach shuffle (sort active) [ [ac] ->
        ifelse (random-float 1 < mu or count no-active = 0)
        [
          create-link-with ac
        ]
        [
          let cut? false
          while [not cut?] [
            let nodej one-of no-active
            let kj [count my-links] of nodej
            let S sum [count my-links] of no-active
            if (kj / S) > random-float 1 [
              create-link-with nodej
              set cut? true
            ]
          ]
        ]
      ]
      set active (turtle-set active self)
      let cut? false
      while [not cut?] [
        let nodej one-of active
        let kj [count my-links] of nodej
        let S sum [1 / (count my-links)] of active
        let P (1 / (kj * S))
        if P > random-float 1 [
          set no-active (turtle-set no-active nodej)
          set active active with [self != nodej]
          set cut? true
        ]
      ]
    ]
  ]]
  ask turtles [setxy random-xcor random-ycor]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; GO PROCEDURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to go
  set non-behavior 0 ;; The global variables non-behavior and pro-behavior reset each tick.
  set pro-behavior 0
  ask turtles [
    set behaved? false ;; Every turtle must behave each turn. behaved? is a turtles-own variable (true/false) that ensures this.
    behave ;; Turtles interact with affordances (environment) in order to behave.
    if pro-env > non-env [
      set color black ] ;; The colour of agents is set to either black or red (depending on which is bigger, pro-env or non-env). This merely cosmetic.
    if non-env > pro-env [
      set color red ]
  ]
  if mutate-on? = true [ mutate] ;; The pro-env and non-env of agents is suspect to mutations. This can be considered a product of, e.g., external influence.
  tick
  if ticks > 20440 [stop] ;; Stopping condition.
end

to mutate ;; At each tick, there is a chance for the pro-env and non-env values of agents to mutate. This represents any external influence to the model.
let mutate-prob 0.005 ;; Arbitrary values for mutation probability (chance of mutation per tick)...
let mutate-rate 0.05  ;; ...and the rate of mutation (how large the mutation is).
  if random-float 1 < mutate-prob [ ;; If a random number between 0 and 1 is smaller than mutate-prob...
    ask turtles [ set pro-env pro-env + mutate-rate ]] ;; ...increase pro-env by mutate-rate.
  if random-float 1 < mutate-prob [
    ask turtles [ set non-env non-env - mutate-rate ]] ;; ...and so on for all four possible configurations. (Mutation is not biased to any direction.)
  if random-float 1 < mutate-prob [
    ask turtles [ set non-env non-env + mutate-rate ]]
  if random-float 1 < mutate-prob [
    ask turtles [ set pro-env pro-env - mutate-rate ]]
end

to behave ;; The heart of this model. This procedure defines how agents interact with the environment, or how they behave.

  while [behaved? = false] [ ;; This while-loop ensures that a turtle behaves somehow during its turn.
  if affordance = 1 ;; If the patch the agent is on is a pro-environmental affordance (affordance = 1)...
  and random-float 1 < pro-env [ ;; ...and a random real number between 0 and 1 is smaller than the pro-env of the agent...
    set pro-env pro-env + asocial-learning ;; ...interact with the patch and learn from the process (gain pro-env + asocial-learning)
    set non-env non-env - asocial-learning ;; ...and decrease non-env (non-env - asocial-learning).
    set pro-behavior pro-behavior + 1 ;; Increase the global variable pro-behavior (number of pro-environmental behaviors per tick).
    set behaved? true ;; The turtle has behaved pro-environmentally, so it has behaved for this turn and will not behave again until the next tick (it will still complete the commands in this behave procedure).
    if niche-construction = TRUE [ ;; If niche-construction is turned on...
      if random-float 1 < (construct-pro / number-of-agents) [ ;; if a random real number between 0 and 1 is smaller than the probability for niche construction (niche construction rate)...
        ask one-of neighbors [ ;; ask a random neighbouring patch (in Moore neighbourhood)...
            set affordance 1 ;; ...to construct the patch into a pro-environmental affordance...
            set pcolor (violet - 1)] ;; ...and set its colour to violet.
     ]
    ]

    if networks = TRUE [ ;; If networks are turned on, interact with network neighbours...
      ask link-neighbors [
        set pro-env pro-env + (social-learning) ;; ...and engage in social learning (set pro-env and non-env values of neighbors to +/- rate of social-learning).
        set non-env non-env - (social-learning)
  ]
 ]
]

  if affordance = 0 ;; This procedure follows the same logic as above, except it defines the procedure for interacting with non-environmental affordances.
  and random-float 1 < non-env [
    set non-env non-env + asocial-learning
    set pro-env pro-env - asocial-learning
    set non-behavior non-behavior + 1
    set behaved? true
    if niche-construction = TRUE [
      if random-float 1 < (construct-non / number-of-agents) [
        ask one-of neighbors [
            set affordance 0
            set pcolor (sky)]
 ]
]
    if networks = TRUE [
      ask link-neighbors [
        set non-env non-env + (social-learning)
        set pro-env pro-env - (social-learning)
    ]
  ]
]

 if pro-env > upper-bound [set pro-env upper-bound] ;; Here I establish maximum boundaries for pro-env and non-env.
 if non-env < lower-bound [set non-env lower-bound] ;; These are arbitrary boundaries, but presumably it is unrealistic to have probability of 1...
 if non-env > upper-bound  [set non-env upper-bound] ;; ...since humans never behave that predictably.
 if pro-env < lower-bound [set pro-env lower-bound]

move ;; Move to random direction.
  ]
end


to move ;; Random walk procedure.
  rt random 45 ;; "Wiggle" procedure, i.e. face up to 45 degrees right and then up to 45 degrees left...
  lt random 45
  fd 1 ;; ...and move one step forward.
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; OTHER        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to-report global-clustering-coefficient ;; Calculate global clustering coefficient. From NetLogo's network extension: https://ccl.northwestern.edu/netlogo/docs/nw.html
  let closed-triplets sum [ nw:clustering-coefficient * count my-links * (count my-links - 1) ] of turtles
  let triplets sum [ count my-links * (count my-links - 1) ] of turtles
  report closed-triplets / triplets
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copyright 2019 Roope O. Kaaronen.
; See Info tab for full copyright and license.
; Contact:
; Social media: @roopekaaronen
; Webpage: roopekaaronen.com
; Email: roope.kaaronen@helsinki.fi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@#$#@#$#@
GRAPHICS-WINDOW
210
10
620
421
-1
-1
2.0
1
10
1
1
1
0
1
1
1
-100
100
-100
100
0
0
1
ticks
30.0

SLIDER
14
10
186
43
number-of-agents
number-of-agents
50
1000
100.0
1
1
NIL
HORIZONTAL

SLIDER
14
55
186
88
pro-amount
pro-amount
0
1
0.5
0.01
1
NIL
HORIZONTAL

BUTTON
36
282
99
315
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

BUTTON
110
326
173
359
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

SLIDER
7
465
179
498
asocial-learning
asocial-learning
0
0.0002
5.0E-5
0.00001
1
NIL
HORIZONTAL

SWITCH
46
99
152
132
networks
networks
0
1
-1000

PLOT
630
378
1014
582
Network degree distribution (# of connections per turtle)
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
"default" 1.0 1 -16777216 true "" "let max-degree max [count link-neighbors] of turtles\nplot-pen-reset  ;; erase what we plotted before\nset-plot-x-range 1 (max-degree + 1)  ;; + 1 to make room for the width of the last bar\nhistogram [count link-neighbors] of turtles"

SLIDER
6
231
178
264
initial-pro
initial-pro
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
7
282
179
315
initial-non
initial-non
0
1
0.5
0.01
1
NIL
HORIZONTAL

CHOOSER
28
141
166
186
network-type
network-type
"random" "small-world" "preferential" "KE"
3

SWITCH
23
193
180
226
niche-construction
niche-construction
0
1
-1000

SLIDER
1031
431
1203
464
rseed
rseed
1
100
74.0
1
1
NIL
HORIZONTAL

SLIDER
7
505
179
538
social-learning
social-learning
0
0.0002
7.0E-5
0.00001
1
NIL
HORIZONTAL

SWITCH
1031
392
1165
425
random-seed?
random-seed?
1
1
-1000

BUTTON
32
326
95
359
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

TEXTBOX
1175
395
1325
423
If On, seed is set to value of 'rseed'.
11
0.0
1

SLIDER
8
595
180
628
network-param
network-param
1
20
5.0
0.1
1
NIL
HORIZONTAL

PLOT
630
194
1014
375
Turtles: Mean pro-env and non-env 
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"pro" 1.0 0 -16777216 true "" "plot mean [pro-env] of turtles"
"non" 1.0 0 -2674135 true "" "plot mean [non-env] of turtles"

PLOT
630
11
1013
192
Patch colour
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
"pro" 1.0 0 -8630108 true "" "plot count patches with [pcolor = (violet - 1)]"
"non" 1.0 0 -13791810 true "" "plot count patches with [pcolor = sky]"

PLOT
1019
194
1368
374
Number of red & black turtles
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
"white" 1.0 0 -16777216 true "" "plot count turtles with [color = black]"
"red" 1.0 0 -2674135 true "" "plot count turtles with [color = red]"

SLIDER
7
378
179
411
construct-non
construct-non
0
number-of-agents
5.0
0.1
1
NIL
HORIZONTAL

TEXTBOX
11
573
161
591
Network parameters:
11
0.0
1

SLIDER
7
635
179
668
mu
mu
0
1
0.9
0.01
1
NIL
HORIZONTAL

SLIDER
8
419
180
452
construct-pro
construct-pro
0
number-of-agents
5.0
0.1
1
NIL
HORIZONTAL

SWITCH
1031
471
1150
504
mutate-on?
mutate-on?
0
1
-1000

PLOT
1019
10
1393
192
Pro-behavior and non-behavior / number of agents
NIL
NIL
0.0
10.0
0.0
0.5
true
true
"" ""
PENS
"pro" 1.0 2 -16777216 true "" "plot pro-behavior / number-of-agents"
"non" 1.0 2 -2674135 true "" "plot non-behavior / number-of-agents"

MONITOR
630
585
794
630
Global clustering coefficient
global-clustering-coefficient
2
1
11

MONITOR
190
429
318
474
Construct probability
construct-pro / number-of-agents
2
1
11

@#$#@#$#@
# Cultural Evolution of Sustainable Behaviours: Landscape of Affordances Model (version 1.2.0)

## ODD Protocol 
Please refer to CoMSES (https://www.comses.net/codebases/c2feceb8-d9c4-4637-8f27-fda49c7dc4f3/releases/1.2.0/) for an ODD (Overview, Design concepts, Details) protocol which provides a full description of this model.

## Publication
For an associated publication (research article), see PsyArXiv: https://psyarxiv.com/w6dpa/

## Summary
This NetLogo model illustrates the cultural evolution of pro-environmental behaviour patterns. It illustrates how collective behaviour patterns evolve from interactions between agents and agents (in a social network) as well as agents and the affordances within a niche. More specifically, the cultural evolution of behaviour patterns is understood in this model as a product of:

1.	The landscape of affordances (action opportunities) provided by the material environment,
2.	Individual learning and habituation,
3.	Social learning and network structure,
4.	Personal states (such as habits and attitudes), and
5.	Cultural niche construction, or the modulation of affordances within a niche.

More particularly, the model illustrates how changes in the landscape of affordances (Rietveld and Kiverstein, 2014) can trigger nonlinear changes in collective behaviour patterns. The model also shows how several behavioural cultures can emerge from the same environment and even within the same network.

## CREDITS AND REFERENCES

ROOPE O. KAARONEN (https://roopekaaronen.com)

Copyright 2019 Roope Kaaronen.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Roope Kaaronen at roope.kaaronen@helsinki.fi.

This model was created as part of the projects: Cultural Evolution of Sustainable Behaviours: Pro-Environmental Tipping Points in an Agent-Based Model at the YSSP Programme at IIASA, Vienna. The project gratefully acknowledges the support of the Academy of Finland and IIASA.

Cite as: Kaaronen, R.O. 2019. Cultural Evolution of Sustainable Behaviours: Landscape of Affordances Model. 
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
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Copenhagen timeseries" repetitions="300" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="21535"/>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>count patches with [pcolor = violet]</metric>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro-amount">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Copenhagen timeseries batch" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="17885"/>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <steppedValueSet variable="construct-pro" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="pro-amount">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Stylized batch" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>standard-deviation [pro-env] of turtles</metric>
    <metric>standard-deviation [non-env] of turtles</metric>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pro-amount" first="0" step="0.01" last="1"/>
  </experiment>
  <experiment name="Stylized timeseries" repetitions="300" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro-amount">
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity batch" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>standard-deviation [pro-env] of turtles</metric>
    <metric>standard-deviation [non-env] of turtles</metric>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="100"/>
      <value value="300"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pro-amount" first="0" step="0.01" last="1"/>
  </experiment>
  <experiment name="network" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>[count link-neighbors] of turtles</metric>
    <metric>global-clustering-coefficient</metric>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro-amount">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity batch" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>standard-deviation [pro-env] of turtles</metric>
    <metric>standard-deviation [non-env] of turtles</metric>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="100"/>
      <value value="300"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pro-amount" first="0" step="0.01" last="1"/>
  </experiment>
  <experiment name="Sensitivity-pop" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>standard-deviation [pro-env] of turtles</metric>
    <metric>standard-deviation [non-env] of turtles</metric>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="10"/>
      <value value="25"/>
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
      <value value="300"/>
      <value value="400"/>
      <value value="500"/>
      <value value="750"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro-amount">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity-net" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>standard-deviation [pro-env] of turtles</metric>
    <metric>standard-deviation [non-env] of turtles</metric>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="1"/>
      <value value="5"/>
      <value value="7"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro-amount">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity-soc" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>standard-deviation [pro-env] of turtles</metric>
    <metric>standard-deviation [non-env] of turtles</metric>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="0"/>
      <value value="3.0E-5"/>
      <value value="4.0E-5"/>
      <value value="6.0E-5"/>
      <value value="7.0E-5"/>
      <value value="7.0E-4"/>
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro-amount">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity-env" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>standard-deviation [pro-env] of turtles</metric>
    <metric>standard-deviation [non-env] of turtles</metric>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pro-amount" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="Sensitivity-nc" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>standard-deviation [pro-env] of turtles</metric>
    <metric>standard-deviation [non-env] of turtles</metric>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
      <value value="25"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro-amount">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity-asoc" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>standard-deviation [pro-env] of turtles</metric>
    <metric>standard-deviation [non-env] of turtles</metric>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="0"/>
      <value value="3.0E-5"/>
      <value value="5.0E-5"/>
      <value value="6.0E-5"/>
      <value value="7.0E-5"/>
      <value value="5.0E-4"/>
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro-amount">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity-pro" repetitions="300" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>mean [pro-env] of turtles</metric>
    <metric>mean [non-env] of turtles</metric>
    <metric>standard-deviation [pro-env] of turtles</metric>
    <metric>standard-deviation [non-env] of turtles</metric>
    <metric>pro-behavior</metric>
    <metric>non-behavior</metric>
    <enumeratedValueSet variable="initial-pro">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="networks">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-non">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rebound">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutate-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-param">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-non">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;KE&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="niche-construction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-learning">
      <value value="7.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asocial-learning">
      <value value="5.0E-5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rseed">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-agents">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="construct-pro">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro-amount">
      <value value="0.5"/>
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
