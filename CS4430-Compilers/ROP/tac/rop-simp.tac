0:	pushl 14;
1:	pushl 9;
2:	pushl 4;
3:	ret;

4:	mov R0 #0;
5:	mov Rx R0;
6:	mov R1 #9;
7:	write Rx;
8:	ret;

9:	mov Rx R1;
10:	mov R2 #8;
11:	mov Rx R2;
12:	write Rx;
13:	ret;

14:	exit;