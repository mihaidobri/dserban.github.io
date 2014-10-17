### Bridge Traversal

Se da un pod suspendat format din scanduri legate prin liane.  
In timp, unele scanduri s-au deteriorat, altele au disparut.  
Pentru traversare se pot face pasi de lungime 1, 2 sau 3.  
Scandurile deteriorate permit doar pasi de lungime 1 (si dinspre si catre).  
De asemenea se subintelege ca nu se poate pasi pe scanduri lipsa.  
Sa se determine toate modurile in care poate fi traversat podul.  
Conventia va fi sa notam cu "SB" o scandura buna, cu "SD" una deteriorata, si cu "SL" una lipsa.

Pentru rezolvarea problemei, se vor crea de la zero fisierele `testsuite.rb` si `bridgetraversal.rb`. Apoi se va scrie primul test (it must fail), dupa care se va implementa o bucatica de algoritm care face testul sa treaca. Apoi se va relua ciclul, trecand la a doua metoda (scrierea testului, verificare ca pica, implementare de algoritm).  
Problema se considera rezolvata atunci cand toate exemplele de teste listate mai jos au trecut prin cate o faza RED si cate una GREEN.  
Aceasta abordare poarta numele de metodologia TDD (Test-Driven Development), si este folosita pe larg in industria software.
```
Traversari posibile pentru [SB,SB]:
[SB1SB]

Traversari posibile pentru [SB,SB,SB]:
[SB1SB,SB1SB]
[SB2SB]

Traversari posibile pentru [SB,SD,SB,SB]:
[SB1SD,SD1SB,SB1SB]
[SB2SB,SB1SB]
[SB3SB]

Traversari posibile pentru [SB,SD,SB,SL,SB]:
[SB1SD,SD1SB,SB2SB]
[SB2SB,SB2SB]

Traversari posibile pentru [SB,SL,SB,SB,SL,SB,SD,SB]:
[SB2SB,SB1SB,SB2SB,SB1SD,SD1SB]
[SB2SB,SB1SB,SB2SB,SB2SB]
[SB2SB,SB3SB,SB1SD,SD1SB]
[SB2SB,SB3SB,SB2SB]
[SB3SB,SB2SB,SB1SD,SD1SB]
[SB3SB,SB2SB,SB2SB]
```

