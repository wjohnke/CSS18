

0:	pushl 14;
1:	pushl 12;
2:	pushl 10;
3:	pushl 5;
4:	ret;

5:	mov Rx #2;
6:	equal Rtst Rx #0;
7:	mul Rdsp Rtst #1;
8:	sub SP SP Rdsp; // replace BRNZ Rx 12 //
9:	ret;

10:	write #1;
11:	ret;

12:	write #2;
13:	ret;

14:	exit;