0:	mov R0 #0;
	mov Rx R0;
1:	write Rx;
        add Rx Rx #1;
	jmp #1;
2:	exit;
