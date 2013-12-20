# bartok

A music clojure library.

## Litterals
#### Alteration 
```
  :bb :b :# :x
```
#### NaturalPitchClass 
```
  :a :b :c :d :e :f :g
```
#### Directions 
```
  :+ :-
```
#### PitchClass
  - upcased NaturalPitchClass
  - Alteration
```  
  //example 
  :C# 
```
#### Pitch
  - PitchClass
  - octave
```  
  example: :C#2 / :Eb-2
```
#### DegreeClass
```
  :root :second :thrid :fourth :fifth :sixth :seventh
```
#### Degree
```
  :R
  :m2 :M2 :#2
  :o3 :m3 :M3 :#3
  :b4 :P4 :+4
  :b5 :P5 :+5
  :m6 :M6 :#6
  :o7 :m7 :M7   
```
#### GenericIntervalClass  
```
  :1st :2nd :3rd :4th :5th :6th :7th 
```
#### GenericInterval 
  - GenericIntervalClass
  - separator -
  - Direction
  - *optional* octave-offset #"[0–9]"
```
  example: :2nd-u2
```
#### Interval
  - Degree
  - separator -
  - direction
  - *optional* octave-offset #"[0–9]"
```
  example: +4-u2 / P4-d
```
#### ModeClass
```
  :Lyd :Mix :Eol :Loc :Ion :Dor :Phry
  :Lyd+ :Lydb7 :Mixb6 :Loc2 :Alt :Melm :Phry6
  :Lyd#2 :AltDim :Harmm :Loc6 :Ion+ :Dor+4 :PhryM
```
#### Mode
  - PitchClass
  - separator
  - ModeClass
```
  ;example
  :Eb-Dor+4
```    

