TITLE Insertion Sort (insertionSort.asm)
;
;
;

INCLUDE Irvine32.inc

.data
X BYTE 1,3,5,2,4

.code
MAIN PROC

	call PrintX

	mov esi, OFFSET X
	mov ecx, LENGTHOF X    ;Size array
	add ecx, esi
	inc esi
	
	xor eax,eax
	xor ebx,ebx
	xor edx,edx
	
;for(si = 1; si < arrLength; si++)
L1:
	cmp esi, ecx
	je endfor

	mov al,[esi]	  ;item = X[si]
	mov ebx, esi   ; ebx = si
	
	
;while (ebx > 0) && (X[ebx - 1] > item)
L2:
	cmp ebx, OFFSET X
	jle endwhile
	cmp [ebx - 1], al
	jle endwhile

	;Shift element one slot to the right
	mov dl,[ebx-1]
	mov [ebx], dl
	dec ebx

	jmp L2
	
endwhile:
	mov [ebx], al
	inc esi
	
	jmp L1
	
endfor:
	call PrintX
	exit	
MAIN ENDP

PrintX PROC
	pushad
		mov esi, OFFSET X
		mov ecx, LENGTHOF X
		mov ebx, 1
		call DumpMem
	popad
	ret
PrintX ENDP


END MAIN
