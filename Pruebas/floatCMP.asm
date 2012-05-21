TITLE Utilizando FCMP (floatCMP.asm)
;
; Este codigo crea dos variables FLOAT X y Y
; Y las compara utilizando FCMP

INCLUDE Irvine32.inc
.data
x real8 5.5
y real8 2.5

msg1 BYTE "Y es mayor que X",13,10,0
msg2 BYTE "X es mayor que Y",13,10,0
msg3 BYTE "Son iguales",13,10,0

.code
MAIN PROC
	finit ;initialize FPU
	call DumpRegs
	fld x ;push 1.5 onto the stack
	fld y ;push 1.0e-25 onto the stack
	fcom
	fstsw ax ;Copy the status word containing the result
	fwait ;Ensure previous instruction completed
	sahf ;transfer the condition codes to CPUs flag register	
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
END MAIN