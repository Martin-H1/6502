10 REM Toggle VIA2.PB0 LOW and HIGH
20 REM Set up common VIA2 constants
30 Via2PRB=$7f60
40 Via2DDRB=$7f62
50 FOR X = 0 TO 16
60 REM Set VIA2.PB0 as output
70 A = PEEK Via2DDRB
80 A = A AND $FE
90 POKE Via2DDRB, A
100 A = PEEK Via2PRB
110 A = A EOR $01
120 POKE Via2PRB, A
130 FOR Y = 1 TO 100
140 NEXT Y
150 NEXT X
