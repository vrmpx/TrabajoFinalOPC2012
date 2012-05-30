TITLE Prueba de ordenamiento

INCLUDE Irvine32.inc

;Se necesitan punteros a DWORD para que el procedimiento
;de ordenamiento (SelectionSort) funcione
PDWORD TYPEDEF PTR DWORD

.data
arreglo DWORD 6, 5, 4, 3, 5, 2, 9, 11
arreglo2 DWORD 6, 5, 4, 80, 5, 2, 9, 100, 12

.code

;--------------------------------------------------------
; Ordenamiento por seleccion
; Ordena el arreglo de DWORD's (elem. de 32bit) 
; dado por esi. Ordena ecx elementos
; Recibe:
; - Registro ESI: OFFSET del arreglo a ordenar
; - Registro ECX: Numero de elementos a ordenar
;--------------------------------------------------------
SelectionSort PROC
.data
	act PDWORD ? ;posicion actual que queremos ordenar
	idx PDWORD ? ;indice para moverse en el arreglo
	imin PDWORD ? ;apunta al valor minimo encontrado
	tamArr DWORD ? ;tamano del arreglo "decreciente"
.code
	pushad
	;sacamos los valores iniciales
	mov tamArr, ecx
	mov act, esi
INI:
	;preparamos el loop q busca el valor minimo
	;entre [esi, esi + tamArr]
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
	;incrementamos idx pa q avancemos en el arreglo
	add idx, TYPE DWORD
	loop L1
	;preparamos el intercambio
	mov esi, act
	mov eax, [esi]
	mov esi, imin
	mov ebx, [esi]
	;hacemos el intercambio entre imin y act
	mov [esi], eax
	mov esi, act
	mov [esi], ebx
	;avanzamos en la siguiente posicion a ordenar
	add act, TYPE DWORD
	dec tamArr
	cmp tamArr, 0
	jnz INI
FIN:
	popad
	ret
SelectionSort ENDP
;--------------------------------------------------------

;--------------------------------------------------------
; Imprime el arreglo "arreglo". Usaar este proc. para
; depurar.
; Recibe: Nada
; Devuelve: Nada
;--------------------------------------------------------
imprArr PROC USES esi ecx ebx
	mov esi, OFFSET arreglo
	mov ecx, LENGTHOF arreglo
	MOV ebx, TYPE arreglo
	call DumpMem
	ret
imprArr ENDP
;--------------------------------------------------------

;--------------------------------------------------------
; Procedimiento main(). Punto de entrada del programa.
; Recibe: Nada
; Devuelve: Nada
;--------------------------------------------------------
main PROC
	;PRUEBA 1: Un arreglo
	mov esi, OFFSET arreglo
	mov ecx, LENGTHOF arreglo
	call SelectionSort
	mov ebx, TYPE arreglo
	call DumpMem
	;PRUEBA 2: Otro arreglo con otro tamanio 
	mov esi, OFFSET arreglo2
	mov ecx, 3
	call SelectionSort
	mov ebx, TYPE arreglo2
	call DumpMem
	;PRUEBA 3: Otro arreglo 
	mov esi, OFFSET arreglo2
	mov ecx, LENGTHOF arreglo2
	call SelectionSort
	mov ebx, TYPE arreglo2
	call DumpMem
	
	exit
main ENDP
;--------------------------------------------------------

END MAIN
