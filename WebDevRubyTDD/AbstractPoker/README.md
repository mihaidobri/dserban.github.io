### Poker Abstract

**In jocul real de poker**, jucatorii construiesc maini de cate cinci carti în conformitate cu regulile prestabilite ale jocului.

Aceste maini sunt comparate pentru a se stabili castigatorul.

Dar inainte de a fi comparate, ele trebuie mai intai categorisite folosind un algoritm de clasificare, care este standard la toate variantele de poker.

**Poker-ul abstract** este o varianta imaginara a jocului de poker, in care un pachet de carti nu are 52 de carti (cate 13 pentru fiecare culoare), ci 4000 de carti (numerotate de la 1 la 1000 pentru fiecare culoare).

Esenta poker-ului abstract consta tocmai in proiectarea algoritmului de clasificare mentionat mai sus. In poker-ul abstract, o mana consta dintr-un set de 5 numere intregi cuprinse intre 1 si 1000, iar culoarea unei carti nu este luata aproape deloc in considerare la clasificarea unei maini (singura situatie in care se tine - in mod indirect - cont de culoare este restrictia ca nu pot exista 5 carti cu acelasi numar, evident din cauza ca exista doar 4 culori).

Iata lista tuturor mainilor posibile in poker-ul abstract:

**High Card** (atunci cand mana contine 3 sau 4 numere consecutive)  
Exemple:  
Mana `[7,2,6,9,5]` contine numerele consecutive 5,6,7 deci este high card  
Mana `[7,4,6,9,5]` contine numerele consecutive 4,5,6,7 deci este high card

**One Pair**  
Exemplu:  
Mana `[4,3,5,8,3]` contine perechea 3,3 si nicio alta pereche, deci este one pair

**Two Pairs**  
Exemplu:  
Mana `[8,3,5,8,3]` contine perechile 3,3 si 8,8 deci este two pairs

**Three Of A Kind**  
Exemplu:  
Mana `[5,9,5,7,5]` contine tripleta 5,5,5 deci este three of a kind

**Four Of A Kind**  
Exemplu:  
Mana `[5,5,5,2,5]` contine 4 carti de acelasi fel, deci este four of a kind

**Straight**  
Exemplu:  
Mana `[7,5,6,8,9]` contine 5 numere consecutive, deci este straight

**Full House**  
Exemplu:  
Mana `[8,3,3,8,3]` contine 2 carti de-un fel si 3 de-un fel, deci este full house

Pentru rezolvarea problemei, se va de-comenta prima metoda de test din fisierul `testsuite.rb`, se va rula suita de teste cu comanda `ruby testsuite.rb`, verificand ca pica testul, apoi se va implementa in `abstractpoker.rb` o bucatica de algoritm care face testul sa treaca. Apoi se va relua ciclul, trecand la a doua metoda (de-comentare, verificare ca pica, implementare de algoritm).  
Problema se considera rezolvata atunci cand toate testele sunt de-comentate si "pe verde".  
Abordarea de mai sus are avantajul ca simuleaza foarte indeaproape metodologia TDD (Test-Driven Development) folosita pe larg in industria software.
