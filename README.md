# BARTÒK

A clojure library for music composition.

## Litterals

### Alteration 

```
  :bb :o :b :m :M :N :P :# :+ :x
```
### NaturalPitchClass
 
```
  'A 'B 'C 'D 'E 'F 'G
```
### Directions 

```
  :u :d
```
### PitchClass
- NaturalPitchClass
- Alteration

*usage*

```  
  :C# :C :Dx :Fb
```
### Pitch
- PitchClass
- octave

*usage*

```  
  :C#2 :Eb-2 :B#0
```
### DegreeClass

```
  :root :second :third :fourth :fifth :sixth :seventh
```
### Degree

```
  :R
  :m2 :M2 :#2
  :o3 :m3 :M3 :#3
  :b4 :P4 :+4
  :b5 :P5 :+5
  :m6 :M6 :#6
  :o7 :m7 :M7   
```
### GenericIntervalClass 
 
```
  :1st :2nd :3rd :4th :5th :6th :7th 
```
### GenericInterval 

- GenericIntervalClass
- separator -
- Direction
- *optional* octave-offset #"[0–9]"

*usage*

```
:2nd-u2 :4th-d1 :7th-u :3rd-d
```

### Interval
- Degree
- separator -
- direction
- *optional* octave-offset #"[0–9]"

*usage*

```
  +4-u2 / P4-d
```
### ModeClass
```
  :Lyd :Mix :Eol :Loc :Ion :Dor :Phry
  :Lyd+ :Lydb7 :Mixb6 :Loc2 :Alt :Melm :Phry6
  :Lyd#2 :AltDim :Harmm :Loc6 :Ion+ :Dor+4 :PhryM
```
### Mode

- PitchClass
- separator
- ModeClass

*usage*

```
  :Eb-Dor+4
```    

