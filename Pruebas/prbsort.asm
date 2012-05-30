TITLE Prueba de ordenamiento

INCLUDE Irvine32.inc

;definimos el tipo de dato de puntero a byte
PBYTE TYPEDEF PTR BYTE
PWORD TYPEDEF PTR WORD
PDWORD TYPEDEF PTR DWORD

.data
arreglo DWORD 6, 5, 4, 3, 5, 2, 9, 11

.code
;Ordenamiento por seleccion
;Ordena el arreglo de DWORD's (elem. de 32bit) dado por esi
;ordena ecx elementos
SelectionSort PROC
.data
	act PDWORD ?
	idx PDWORD ? ;para moverse en el arreglo
	imin PDWORD ? ;apunta al valor minimo
	tamArr DWORD ? ;contador para indicar fin
	cont DWORD 0 ;contados 
	aux DWORD ?
	parr PDWORD ?
.code
	pushad
	;calculamos edi = limite superior del arreglo
	mov tamArr, ecx
	mov act, esi
	mov parr, esi
INI:
	mov esi, act
	mov idx, esi
	mov imin, esi
	mov ecx, tamArr
L1:
	;nos preparamos para comparar
	mov esi, idx
	mov eax, [esi]
	mov esi, imin
	mov ebx, [esi]
	cmp eax, ebx
	jge NEXT
	;si [idx] < [imin] updates imin  = idx
	mov esi, idx
	mov imin, esi
NEXT:
	;incrementamos idx
	add idx, TYPE DWORD
	loop L1
	
	;mov esi, act
	;mov eax, [esi]
	;mov aux, eax
	;mov [esi], [imin]
	;mov [imin], aux
	
	mov esi, act
	mov eax, [esi]
	mov esi, imin
	mov ebx, [esi]
	;call DumpRegs
	;call imprArr
	;xchg eax, imin
	mov [esi], eax
	mov esi, act
	mov [esi], ebx
	;call imprArr
	;mov act, eax
	;call imprArr
	;call ReadChar

	add act, TYPE DWORD
	dec tamArr
	cmp tamArr, 0
	jnz INI
FIN:
	popad
	ret
SelectionSort ENDP

imprArr PROC USES esi ecx ebx
	mov esi, OFFSET arreglo
	mov ecx, LENGTHOF arreglo
	MOV ebx, TYPE arreglo
	call DumpMem
	ret
imprArr ENDP

;main()
main PROC
	mov esi, OFFSET arreglo
	mov ecx, LENGTHOF arreglo
	call SelectionSort
	
	call imprArr
	
	exit
main ENDP
END MAIN