# motc

Compiler & Decompiler for Crackle Cradle ver21 .mot (Bind & Motion) file. 

Documentation in progress. May be written roughly after the project is done.

## Requirement

[Racket](https://racket-lang.org/) is required.

## Sample
Motion:
```bash
racket motc.rkt -o ./Wait.mot /www/CrackleCradle170416/Chara/Motion/Bomb/Wait.txt
```
or
```bash
racket motc.rkt -o ./Wait.mot -m motion /www/CrackleCradle170416/Chara/Motion/Bomb/Wait.txt
```
Bind:
```bash
racket motc.rkt -o ./Drill_Right_Finish.mot -m bind /www/CrackleCradle170416/Chara/Bind/BossMachine/Drill_Right_Finish.txt
```