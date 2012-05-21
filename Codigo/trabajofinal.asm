TITLE TrabajoFinal (trabajofinal.asm)
;
;
;

INCLUDE Irvine32.inc

.data
x REAL8 1.0
y REAL8 2.0

msg1 BYTE "Y es mayor que X",13,10,0
msg2 BYTE "X es mayor que Y",13,10,0
msg3 BYTE "Son iguales",13,10,0



.code
MAIN PROC
	call DumpRegs
	finit ;Initialize FPU
	fld x
	fld y
	call compara
	call DumpRegs
	ja st0_greater
	jb st0_lower
	jz both_equal
	
st0_greater:
	mov edx, OFFSET msg1
	call WriteString
	jmp salida
	
st0_lower:
	mov edx, OFFSET msg2
	call WriteString
	jmp salida
	
both_equal:
	mov edx, OFFSET msg3
	call WriteString
	jmp salida
	
salida:	
	exit
MAIN ENDP




;========================================
; Compara st(0) y st(1) 
; Establece las banderas del CPU de acuerdo
; 
; Recibe: st(0) y st(1) dos numeros de punto flotante
;         en el stack
; Devuelve: CF,SF,ZF de acuerdo al resultado
;========================================
compara PROC
	pushad
	fcom
	fstsw ax   ;Copia status word a AX
	fwait      ;Aseguramos el resultado anterior
	sahf       ;Copiamos el resultado a las banderas del CPU
	popad
	ret
compara ENDP
;========================================

END MAIN