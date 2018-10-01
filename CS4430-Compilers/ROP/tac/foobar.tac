
0:	write #0;
1:	mov R0 #3;
2:	mov Rx R0;
3:	mov R1 #0;
4:	sub R2 Rx R1;
5:	brnz R2 #10;

6:	write #1;
7:	mov R2 #0;
8:	jmp #11;

9:	write #2;
10:	mov R2 #1;

11:	write #3;
12:	brz R2 #18;

13:	write #4;
14:	mov R3 #1;
15:	sub R4 Rx R3;
16:	mov Rx R4;
17:	jmp #3;

18:	write #5;
19:	exit;

