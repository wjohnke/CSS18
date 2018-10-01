
0:	pushl 49;
1:	pushl 47;
2:	pushl 42;
3:  pushl 40;
4:	pushl 36;
5:	pushl 34;
6:	pushl 32;
7:	pushl 30;
8:	pushl 27;
9:	pushl 25;
10: pushl 18;
11:	pushl 16;
12:	pushl 14;
13:	ret;

14:	write #0;
15:	ret;

16:	mov R0 #3;
17:	mov Rx R0;
18:	mov R1 #0;
19:	sub R2 Rx R1;
20:	equal Rtst R2 #0;
21:	not Rtst Rtst;
22:	mul	Rdsp Rtst #4;
23: sub SP SP Rdsp;
24: ret;

25: write #1;
26: ret;

27: mov R2 #0;
28: sub SP SP #2;
29: ret;

30: write #2;
31: ret;

32: mov R2 #1;
33:	ret;

34: write #3;
35: ret;

36: equal Rtst R2 #0;
37: mul Rdsp Rtst #2;
38: sub SP SP Rdsp;
39:	ret;

40: write #4;
41: ret;

42: mov R3 #1;
43: sub R4 Rx R3;
44:	mov	Rx R4;
45:	add SP SP #9;
46: ret;

47: write #5;
48: ret;

49: exit;
