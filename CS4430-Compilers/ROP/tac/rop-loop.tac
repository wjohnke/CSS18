
0:	pushl 11;
1:	pushl 7;
2:	pushl 4;
3:	ret;

4:	mov R0 #0;
5:	mov Rx R0;
6:	ret;

7:	write Rx;
8:	add Rx Rx #1;
9:	add SP SP #1;
10:	ret;

11:	exit;