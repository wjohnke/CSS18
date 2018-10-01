
0:	pushl 11;
1:	pushl 9;
2:	pushl 4;
3:	ret;
4:	mov R0 #0;
5:	mov Rx R0;
6:	write #98;
7:	sub SP SP #1;
8:	ret;
9:	write #99;
10:	ret;
11:	write #100;
12:	exit;
