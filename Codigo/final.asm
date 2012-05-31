;================================================================
; Proyecto final de OPC 2012
; Ordena numeros en coma flotante
; Autores:
; - Victor Raul Martinez Palacios
;   victor.martinez@itam.mx
; - Fernando Aguilar Reyes
;   fernando.aguilar@itam.mx
;================================================================
TITLE TrabajoFinal (final.asm)

INCLUDE Irvine16.inc
INCLUDE macros.inc

;Constantes globales
ARRAYSIZE = 11
CR = 13
LF = 10
DOS_OPEN_FILE = 03DH
DOS_CLOSE_FILE = 03EH
DOS_READ_FILE = 03FH
DOS_INT = 021H

;Se necesitan punteros a DWORD para que el procedimiento
;de ordenamiento (SelectionSort) funcione
PREAL10 TYPEDEF PTR REAL10
PBYTE TYPEDEF PTR BYTE

.data
X REAL10 102.03,65.0,1.002,33.29,33.44,55.18,22.1,0.0,1.0,99.0,1000.01
;X REAL10 5.4E10, 4.5E9, 3.4E8, 2.3E11, 1.2E5, 0.1E1
Num DWORD ?

;Variables used by Irvine Kip's Procedures
pwr10  DWORD  1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000
ErrMsg BYTE 0dh,0ah,"Floating point error",0dh,0ah,0


.code
MAIN PROC
	;Nos ubicamos en el data segment
	mov ax, @data
	mov ds, ax
	;Limpiamos la pantalla
	call ClrScr
	;Leemos datos de entrada
	call LeeDatos
	;Inicalizamos la FPU
	finit
	;Ordenamos
	mov esi, OFFSET X
	mov ecx, LENGTHOF X
	call SelectionSort
	;imprimimos en pantalla el resultado
	;call printX

	exit
MAIN ENDP

;-------------------------------------------------------
;
;-------------------------------------------------------
Kill PROC uses edx
.data
	msg BYTE CR, LF, "*=MATANDO EL PROGRAMA=*", CR, LF, 0
.code
	mov edx, OFFSET msg
	call WriteString
	call DumpRegs
	exit
Kill ENDP
;-------------------------------------------------------

;-------------------------------------------------------
; Lee los datos a utilizar en el programa.
; NOTA: 
; El archivo de entrada debe estar en el mismo
; directiorio que en el .EXE
; Recibe:
; NADA
; Regresa:
; Arreglo Global X con los numeros leidos del archivo
;--------------------------------------------------------
LeeDatos PROC uses eax ecx edx
.data
	LeeDatos_msgin BYTE "Archivo de entrada?", CR, LF, 0
	LeeDatos_msgok BYTE "Archivo leido exitorsamente: ", 0
	LeeDatos_msgerr BYTE "PROC LeeDatos: ERROR al leer el archivo: ", 0
	nomArchivo BYTE "float.in", 0
	linea BYTE 20 DUP (0)
.code
	;leemos el nombre del archivo
	;mov edx, OFFSET LeeDatos_msgin
	;call WriteString
	;mov edx, OFFSET nomArchivo
	;mov ecx, SIZEOF nomArchivo
	;call ReadString
	;call CrLf
	;abrimos el archivo
	mov dx, OFFSET nomArchivo
	call AbreArchivoLect
	mov bx, ax
	jc ERR1
	;imprimimos mensaje exitosos
	mov edx, OFFSET LeeDatos_msgok
	call WriteString
	mov edx, OFFSET nomArchivo
	call WriteString
	call Crlf
	;prueba de ReadFloat Modificado
	mov bx, ax
	call ReadFloat
	;cerramos el archivo
	mov bx, ax
	call CierraArchivo
	ret
ERR1:
	mov edx, OFFSET LeeDatos_msgerr
	call WriteString
	mov edx, OFFSET nomArchivo
	call WriteString
	call Crlf
	call Kill
	ret
LeeDatos ENDP
;--------------------------------------------------------


;--------------------------------------------------------
; Lee una linea del archivo abierto hasta encontrar CR
; Recibe:
; - Registro BX: Handle del archivo
; - Registro DX: OFFSET del buffer poner los datos
; Regresa:
; - Lo leido en un arreglo apuntado por DX.
;--------------------------------------------------------
LeeLinea PROC uses bx cx di dx
.data
	msgerr BYTE "PROC LeeLinea: Error al leer los bytes del archivo", CR, LF, 0
	caract BYTE 0
	pbuff PBYTE ?
	cont WORD 0
.code
	mov pbuff, dx
	mov cx, 1
	mov dx, OFFSET caract
L1:
	call LeeBytes
	jc ERR
	mov al, caract
	mov di, pbuff
	mov [di], al
	add pbuff, TYPE BYTE
	cmp caract, CR
	jne L1
	ret
ERR:
	mov edx, OFFSET msgerr
	call WriteString
	call Kill
	ret
LeeLinea ENDP
;--------------------------------------------------------

;--------------------------------------------------------
; Lee un byte (caracter) de un archivo
; Recibe:
; - Registro BX: Handle del archivo
; Regresa:
; - Registro AL: Caracter leido
;--------------------------------------------------------
LeeByte PROC uses cx dx
.data
	LeeByte_car BYTE 0
.code
	mov ah, DOS_READ_FILE
	;bx nos lo dan como parametro
	mov cx, 1
	mov dx, OFFSET LeeByte_car
	int DOS_INT
	mov al, LeeByte_car
	ret
LeeByte ENDP
;--------------------------------------------------------

;--------------------------------------------------------
; Lee un bloque de bytes de un archivo abierto
; Recibe:
; - Registro BX: Handle del archivo
; - Registro CX: Numero maximo de bytes a leer
; - Registro DX: OFFSET del buffer poner los datos
; Regresa:
; - Registro AX: Si no hay error, regresa el numero de 
;                bytes leidos. Si hay error, regresa el 
;                codigo del error
;--------------------------------------------------------
LeeBytes PROC uses bx cx dx
	mov ah, DOS_READ_FILE
	int DOS_INT
	ret
LeeBytes ENDP
;--------------------------------------------------------

;--------------------------------------------------------
; Abre un archivo en modo de escritura
; Recibe:
; - Registro DX: OFFSET del String con el path del
;                archivo.
; Regresa:
; - Flag CF: 0 si no hubo error, 1 si hay
; - Registro AX: Si no hay error, regresa el handle del
;                del archivo. Si hay error, regresa el 
;                codigo del error
;--------------------------------------------------------
AbreArchivoEsc PROC uses cx
	mov cx, 1
	call AbreArchivo
	ret
AbreArchivoEsc ENDP
;--------------------------------------------------------

;--------------------------------------------------------
; Abre un archivo en modo de lectura
; Recibe:
; - Registro DX: OFFSET del String con el path del
;                archivo.
; Regresa:
; - Flag CF: 0 si no hubo error, 1 si hay
; - Registro AX: Si no hay error, regresa el handle del
;                del archivo. Si hay error, regresa el 
;                codigo del error
;--------------------------------------------------------
AbreArchivoLect PROC uses cx
	mov cx, 0
	call AbreArchivo
	ret
AbreArchivoLect ENDP
;--------------------------------------------------------

;--------------------------------------------------------
; Abre un archivo con las interrupciones de MS-DOS
; Recibe:
; - Registro CX: Modo de abir (lect, Esc)
; - Registro DX: OFFSET del String con el path del
;                archivo.
; Regresa:
; - Flag CF: 0 si no hubo error, 1 si hay
; - Registro AX: Si no hay error, regresa el handle del
;                del archivo. Si hay error, regresa el 
;                codigo del error
;--------------------------------------------------------
AbreArchivo PROC uses cx dx
	mov ah, DOS_OPEN_FILE
	int DOS_INT
	ret
AbreArchivo ENDP
;--------------------------------------------------------

;--------------------------------------------------------
; Cierra un archivo con las interrupciones de MS-DOS
; Recibe:
; - Registro BX: Handle del archivo
; Regresa:
; NADA
;--------------------------------------------------------
CierraArchivo PROC uses ax bx
	mov ah, DOS_CLOSE_FILE
	int DOS_INT
	ret
CierraArchivo ENDP
;--------------------------------------------------------

;--------------------------------------------------------
; Ordenamiento por seleccion
; Ordena el arreglo de REAL10's (elem. de 80bits) 
; dado por ESI. Ordena ECX elementos
; Recibe:
; - Registro ESI: OFFSET del arreglo a ordenar
; - Registro ECX: Numero de elementos a ordenar
;--------------------------------------------------------
SelectionSort PROC
.data
	act PREAL10 ? ;posicion actual que queremos ordenar
	idx PREAL10 ? ;indice para moverse en el arreglo
	imin PREAL10 ? ;apunta al valor minimo encontrado
	tamArr DWORD ? ;tamano del arreglo "decreciente"
	valfa REAL10 ? ;auxiliar para el stack FPU
	valfb REAL10 ? ;auxiliar para el stack FPU
.code
	pushad
	;sacamos los valores iniciales
	mov tamArr, ecx
	mov act, si
INI:
	;preparamos el loop q busca el valor minimo
	;entre [esi, esi + tamArr]
	mov si, act
	mov idx, si
	mov imin, si
	mov ecx, tamArr
	mov si, imin
	fld real10 ptr [esi]
L1:
	;nos preparamos para comparar
	mov si, idx
	fld real10 ptr [esi]
	call compara
	jae NEXT
	;si [idx] < [imin] updates imin  = idx	
	fstp valfa
	fstp valfa
	mov si, idx
	mov imin, si
	mov si, imin
	fld real10 ptr [esi]
	fld valfa
NEXT:
	;incrementamos idx pa q avancemos en el arreglo
	fstp valfa
	add idx, TYPE REAL10
	loop L1
	;hacemos el intercambio entre imin y act
	mov si, act
	fld real10 ptr [esi]
	mov si, imin
	fstp real10 ptr [esi]
	mov si, act
	fstp real10 ptr [esi]
	;avanzamos en la siguiente posicion a ordenar
	add act, TYPE REAL10
	dec tamArr
	cmp tamArr, 0
	jnz INI
FIN:
	popad
	ret
SelectionSort ENDP
;--------------------------------------------------------


;--------------------------------------------------------
; InsertionSort
; Ordena el arreglo X utilizando el algoritmo de insercion
; Recibe:
;		Un arreglo de punto flotante REAL10 en X
; Devuelve:
;		El mismo arreglo ordenado en X
;--------------------------------------------------------
InsertionSort PROC USES si cx bx 
.data
	tmp REAL10 0.0
.code
	mov si, 0		
	add si, TYPE REAL10	;ESI apunta a un elemento en X
	mov cx, ARRAYSIZE - 1
	
;for (ESI = 1; ESI < ECX; ESI++)
L1:
	fld X[si]    ;ST(0) = X[ESI] guardamos el dato temporal
	mov bx, si	;EBX = ESI
	
;while (EBX > 0) && (X[EBX - 1] > X[ESI]) 
L2:
	cmp bx, 0	
	jle endwhile	;JMP si EBX <= 0
	
	fld X[bx-TYPE REAL10]	;ST(0) = X[EBX - 1], ST(1) = X[ESI]
	call compara
	jbe below	;JMP si ST(0) >= ST(1)
	
	;Shift element one slot to the right
	fstp X[bx]	;X[EBX] = ST(0),	ST(0) = X[ESI]
	sub bx, TYPE REAL10	;EBX = EBX - 1
	jmp L2
	
below:
	fstp tmp	;Llegando aqui nos sobra un elemento en ST(0)
	
endwhile:
	fstp X[bx]	;X[EBX] = ST(0) = X[ESI]
	add si, TYPE REAL10	;ESI = ESI + 1

	;loop L1
	dec cx
	jnz L1
	
endfor:
	ret
	
InsertionSort ENDP
;--------------------------------------------------------



;--------------------------------------------------------
; Compara st(0) y st(1) 
; Establece las banderas del CPU de acuerdo
; 
; Recibe: st(0) y st(1) dos numeros de punto flotante
;         en el stack
; Devuelve: CF,SF,ZF de acuerdo al resultado
; Modo de uso:
;			JBE si ST(1) <= ST(0)
;			JAE si ST(1) >= ST(0)
;--------------------------------------------------------
compara PROC USES ax
	fcom
	fstsw ax   ;Copia status word a AX
	fwait      ;Aseguramos el resultado anterior
	sahf       ;Copiamos el resultado a las banderas del CPU
	ret
compara ENDP
;--------------------------------------------------------

;--------------------------------------------------------
; Imprime los contenidos de X 
; Recibe: Nada
; Devuelve: Nada
;--------------------------------------------------------
PrintX PROC USES dx si cx
.data
	msg1 BYTE "=================================",13,10,0
	msg2 BYTE "Begin Array: ",13,10,0
	tmp0 REAL10 0.0
.code
	mov dx, OFFSET msg1
	call WriteString
	mov dx, OFFSET msg2
	call WriteString
	mov dx, OFFSET msg1
	call WriteString
	
	mov si, 0
	mov cx, ARRAYSIZE
L1:
	fld X[si]
	call WriteFloat
	fstp tmp0
	call CrLf
	add si, TYPE REAL10
	loop L1
	
	call CrLf
	ret
PrintX ENDP
;--------------------------------------------------------

;--------------------------------------------------------
;--------------------------------------------------------
DEBUG PROC USES dx
.data
	msg_debug BYTE "DEBUG>",13,10,0
.code
	mov dx, OFFSET msg_debug
	call WriteString
	call DumpRegs
	call ShowFPUStack
DEBUG ENDP
;--------------------------------------------------------



;********************************************************
;			PRIVATE PROCEDURES
;			By Irvine Kip
;********************************************************

;---------------------------------------------------------
ReadFloat PROC USES eax ebx ecx
   LOCAL expsign:SDWORD, sign:byte
;
; Reads a decimal floating-point number from the keyboard
; and translates to binary floating point. The value is
; placed in SP(0) of the floating-point stack.
; Thanks to Mike Byrne (May 09) for suggesting a way to remove
; redundant lines.
; Updated: 11/20/09, by Irvine
;----------------------------------------------------------
.data
expint SDWORD  0
itmp       SDWORD  ?
power      REAL8   ?

.code
	  call fpuSet
    mov  sign,0

    ; look for an optional + or - first
    call GetChar
    cmp  al,'+'
    jne  R1
    ; is a '+' -- ignore it; sign= 0
    call GetChar
    jmp  R2
R1:
    cmp  al,'-'
    jne  R2
    ; is a '-' -- sign= 1
    call GetChar
    inc  sign

    ; here we are done with the optional sign flag
R2:
    ; look for a digit in the mantissa part
    .IF (al >= '0' && al <= '9') || (al == '.')	
      fldz     ; push a 0.  ONE thing in FPU stack
      .WHILE (al >= '0' && al <= '9')
        sub    al,'0'
        and    eax,0Fh
        mov    itmp,eax
        fmul   ten
        fild   itmp
        fadd
        call   GetChar
      .ENDW

      ; decimal point in the mantissa?
      .IF (al == '.')
        call GetChar
        fldz     ; start the fractional part
        fld   ten  ; get the power part started
        fstp  power  ; will be 10, 100, 1000, etc.
        .WHILE (al >= '0' && al <= '9')
          sub  al,'0'
          and  eax,0Fh
          mov  itmp,eax
          fild itmp
          fdiv power
          fadd
          fld  power
          fmul ten
          fstp power
          call GetChar
        .ENDW
        fadd       ; add the front end to the back end
      .ENDIF
    .ELSE
	  ; neither ddd.ddd nor .ddd
	  ; so it's a syntax error
      mov  edx,OFFSET ErrMsg
      call WriteString
	    fldz      ; return a 0.0 in any case
	    call fpuReset
      ret
    .ENDIF
      
    ; OK -- now we have the ddd.ddd part in ST(0)
    ; Now look for an exponent
	; We still have the mantissa in the stack:  ONE thing

    .IF (al=='E' || al=='e')
      mov  expsign,1
      call GetChar
      .IF (al=='+')
        call GetChar
      .ELSEIF (al=='-')
        mov  expsign,-1
        call GetChar
      .ENDIF
      mov  expint,0
      .WHILE (al>='0' && al<= '9')
        sub  al,'0'
        and  eax,0FFh
        mov  ebx,eax
        mov  eax,10
        mul  expint
        add  eax,ebx
        mov  expint,eax
		call GetChar
      .ENDW

      ; power10 gets expsign*10^expint, stuffs it in exponent.
      ; Result returned in FPU.
      
      mov  eax,expint
      imul expsign
      call power10    	; TWO things in the FPU stack
      fmul     	; mantissa is sitting underneath; ONE thing left over
    .ENDIF
    .IF (sign==1)
      fchs
    .ENDIF
	  call fpuReset    	; shouldn't affect stack position
	  
    ret    	; result should be in FPU top
ReadFloat  ENDP

;--------------------------------------------------------------
WriteFloat PROC USES eax ecx edx
;
; Writes the floating point value in the top of the FPU stack 
; to the console window. Displays in exponential format. The 
; value remains on the stack.
;--------------------------------------------------------------
.data
temp_01   REAL8 ?	; KRI
iten    SDWORD  10
mantissa   REAL8 ?
zeroes  BYTE "+0.", 7 DUP('0'), "E+000",0
NaNStr  BYTE "NaN", 0                                     ;******
InfinityStr BYTE "infinity", 0                            ;******
.code
	fst	temp_01	; KRI: save a copy

    call  fpuSet
    ftst                                                  ;******
    call  fChkNaN       ; check for NaN                    ******
    jnz   W0            ; jump if not NaN                  ******
    mov   edx,offset NaNStr  ; print NaN                   ******
    jmp   W0a           ; otherwise this is like a zero    ******
W0:                                                       ;******
    ftst
    call  fcompare   	; look at the sign bit
    jnz   W1
    ; here the thing is all zeroes
    mov   edx,offset zeroes
W0a:
    call  writeString
W0b:
    ;fstp  mantissa
	fst	mantissa	; KRI 7/20/05: changed fstp to fst

    call  fpuReset
    ret
W1:
    mov   al,'+'
    jge   W2
    mov   al,'-'
    fchs    	; now have value >= 0
W2:
    call  WriteChar    ; the sign
    call  fChkInfinity ; Check for infinity                ******
    jne   w2a          ; if not continue normally          ******
    mov   al, 0ECh     ; Print "infinity sign"             ******
    call  writeChar                                       ;******
    jmp   W0b          ; finish like for zeros             ******
W2a:    
    ; Suppose the number's value is V.  We first find an exponent E
    ;  and mantissa M such that 10^8 <= M < 10^9 and M*10^-8*10^E = V.
    ; (E will be in 'exponent', M will be in ST(0))
    call splitup
    
    fistp  itmp    	; save as an integer & POP
    
    mov  eax,itmp
    cmp  eax,pwr10+9*4    ; 10^9
    jl   W4
    xor  edx,edx    	; it's > 10^9
    add  eax,5      	; for rounding
    div  iten       	; divide by 10
    inc  exponent
W4:
    ; start with the MSD
    mov  edx,pwr10+8*4
    xor  edx,edx
    div  pwr10+8*4
    and  al,0Fh
    add  al,'0'
    call WriteChar
    mov  al,'.'
    call WriteChar
    mov  eax,edx
    mov  ecx,7
    call wrdigits

    ; that takes care of the decimals after the decimal point
    ; now work on the exponent part
    mov   al,'E'
    call  WriteChar
    .IF (exponent < 0)
      mov  al,'-'
      neg  exponent
    .ELSE
      mov  al,'+'
    .ENDIF
    call  WriteChar

    movzx eax,exponent
    mul   iten
    mov   ecx,3
    call  wrdigits
    
    call  fpuReset
	
	fld	temp_01	; KRI: restore saved value
    ret
WriteFloat  endp

;------------------------------------------------------------
ShowFPUStack PROC USES  eax
   ; LOCAL  temp:REAL8
;
; Prints the FPU stack in decimal exponential format.
; Written by James Brink, Pacific Lutheran University.
; Used by permission.
;
; Adapted by Kip Irvine, 7/18/05.  
; Revised 7/20/05.
;
; Receives:  Nothing
; Returns:  Nothing
;
; Technique:
;     Uses FINCSTP move the stack top, effectly popping 
;     the stack without actually removing values.
; Note:
;     This procedure clears the exception bits in the FPU status register 
;     before it terminates.  This includes B, ES, SF, PE, UE, OE, ZE, DE, 
;     and IE bits.  
; Uses:  
;	  WriteFloat, mWrite, Crlf, WriteDec
;----------------------------------------------------------------
ControlWordMask = 0000000000111111b  ; Used to mask exception bits
.data
SavedCWord WORD ?   ; Control word when the procedure is started
UsedCWord  WORD ?   ; Control word used by procedure

.code
; Write a header message.
	mWrite  <0Dh,0Ah,"------ FPU Stack ------", 0Dh, 0Ah>

; Set the control word to mask the exception bits
        fclex               ; Clear pending exceptions
        fstcw   SavedCWord  ; Get copy of CW used to restore original
        fstcw   UsedCWord
        or      UsedCWord, ControlWordMask
        fldcw   usedCWord   ; Mask exception bits

; Set up counter n for SP(n)
	mov	eax,0
       
; Display the stack (loop)

LDisplay:
	; added 6/13/06 by KRI
	ftst
	call	fChkNaN
	jz	next_stack_value
	;------ end of 6/13/06
	
	mWrite  "ST("	; Display stack index
	call    WriteDec
	mWrite  "): "

; Write the value of ST(n) and go to new line.
; WriteFloat pops the value from the stack, so we save
; and restore it to compensate.

	call	WriteFloat	; write ST(0) & pop
	call	Crlf

; Move the top of stack pointer.
next_stack_value:
	fincstp             

; Increment count and repeat for 8 values.
	inc	eax 
	cmp	eax,8
	jl	LDisplay

LReturn:
; clear any exceptions and restore original control word before returning
        fclex           ; clear exceptions
        fstcw  SavedCWord
	ret  
ShowFPUStack ENDP

;------------------------------------------------------
GetChar  PROC 
;
; Reads a single character from input,
; echoes end of line character to console window.
;
; Modified by Irvine (7/18/05): removed check for Ctl-C.
;------------------------------------------------------

    ;call ReadChar   	; get a character from keyboard
	call LeeByte ;lee el byte del archivo
    .IF (al == 0dh)	; Enter key?
       call Crlf
    .ELSE
       call WriteChar  	; and echo it back
    .ENDIF
    ret
GetChar  ENDP

MAXEXPONENT=99
; truncate, double precision, all exceptions masked out
stdMask   = 1111000011000000b
stdRMask  = 0000111100111111b
stdControl= 0000111000111111b

.data
stmp word  ?
sw   word  ?
.code
;-------------------------------------------------------
fpuSet PROC uses ax
;
;-------------------------------------------------------
      fstcw sw	; save current control word
      mov   ax,sw
      and   ax,stdMask
      or    ax,stdControl
      mov   stmp,ax
      fldcw stmp	; load masked control word
      ret
fpuSet ENDP

;--------------------------------------------------------
fpuReset  PROC  uses ax bx
;
; This resets the control word
; bits defined by the stdMask
;--------------------------------------------------------
      fstcw stmp	; get current control word
      mov   ax,stmp	; save it in AX
      and   ax,stdMask	; clear bits 6-7, 11-14
      mov   bx,sw	; get saved control word
      and   bx,stdRMask
      or    ax,bx	; set bits 0-5, 8-11
      mov   stmp,ax
      fldcw stmp
	  ret
fpuReset  ENDP
;-------------------------------------------------------- ******
fChkNaN PROC  uses ax                                    ;******
;                                                         ******
; Check the results of the last FTST instruction to see   ******
; the Z flag is set if indeed the value was NaN           ******
;        5432109876543210                                 ******
.data
C3C2C0 = 0100010100000000b                               ;******
.code
    fnstsw ax         ; mov the status word to AX         ******
    and   ax, C3C2C0  ; get the C3, C2, C0 bits from the status word
    cmp   ax, C3C2C0  ; are all the bits 0                ******
    ret                                                  ;******
fChkNaN ENDP    				         ;******

;-------------------------------------------------------- ******
fChkInfinity PROC                                        ;******
.data                                                    ;******
temp REAL10 ?                                            ;******
.code                                                    ;******
    fstp  temp ; store value, fst can't store REAL10     ;******
    fld   temp ; restore the stack                       ;******
    mov   ax, WORD PTR temp
    cmp   WORD PTR temp, 0000h; is Exponent all 1 bits    ;******
    jne   CF1                                            ;******
    cmp   DWORD PTR (temp+2), 00000000h                  ;******
    jne   CF1  ; check first 4 bytes of the mantissa     ;******
    cmp   DWORD PTR (temp+6), 7FFF8000h                  ;****** 
           ; this checks last four bytes of the mantissa ;******
CF1:                                                     ;******
    ret                                                  ;******
fChkInfinity ENDP                                        ;******
;--------------------------------------------------------
fcompare PROC uses ax
;
; Compares two floating-point values.
; Transfers ZF & SF registers from the FPU status word
;  to the CPU, so we can do branches on them
;
;--------------------------------------------------------
    fstsw  status
    mov    ah,byte ptr status+1
    mov    al,ah
    and    ah,040h
    and    al,1
    ror    al,1
    or     ah,al
    sahf
    ret
fcompare ENDP

;--------------------------------------------------------
normalize PROC
;
; shifts ST(0) into range 10^8 <= V < 10^9
; and adjusts the exponent in the process
;
;--------------------------------------------------------
.data
tenp8      REAL8  10.0E8
onep8      REAL8  1.0E8
exponent   SWORD   0

.code
N1:
    fcom tenp8   ; compare to 10^9
    call fcompare
    jl   N2
    fdiv ten
    inc  exponent
    jmp  N1

N2:
    fcom onep8   ; compare to 10^8
    call fcompare
    jge  N3
    fmul ten
    dec  exponent
    jmp  N2
N3:
    ret
normalize ENDP

;---------------------------------------------------------
splitup  PROC USES ecx esi edi
;
; Receives a non-negative number in ST(0).
; Suppose the number's value is V.  The goal is to find an exponent E
;  and integer mantissa M such that 10^8 <= M < 10^9 and
;   V= M*10^-8 * 10^E
; (E will be in 'exponent', M will be in ST(0) on return)
; This uses the pwr10 table in an attempt to narrow down the
; appropriate power using a kind of binary search and reduction
;
;---------------------------------------------------------
.data
onehalf    REAL8  0.5
one       REAL8   1.0
ten       REAL8   10.0

bpwr10    WORD  64,32,16,8,4,2,1
binpwrM10 REAL8  1.0E-64, 1.0E-32, 1.0E-16, 1.0E-8, 1.0E-4, 1.0E-2, 1.0E-1

.code
    mov  exponent,0 

    ; see if == 0.0
    ftst
    call fcompare
    jne  S1
    ret

S1:
    ; start by seeing if it's greater than 10
    fcom  ten
    call  fcompare
    jge   S2    ; >= 10
    ; see if it's < 10
    fcom one
    call fcompare
    jge  S4   ; it's >= 10
    jmp  S3   ; it's < 10

S2: ; here, it's > 10
    ; so we'll reduce it using the binpwr10 table
    mov  ecx,TOPPWR
    mov  esi,0    	; index to binpwr10
    mov  edi,0    	; index to bpwr10
S2a:
    fcom binpwr10[esi]
    call fcompare
    jl   S2c
    fdiv binpwr10[esi]
		mov  ax,bpwr10[edi]
    add  exponent,ax
S2c:
    add  esi,type binpwr10
    add  edi,type bpwr10
    loop S2a
    jmp  S4

S3: ; here, it's < 1.0
    mov  ecx,TOPPWR
    mov  esi,0    	; index to binpwrM10
    mov  edi,0    	; index to mpwr10
S3a:
    fcom binpwrM10[esi]
    call fcompare
    jge  S3c
    fdiv binpwrM10[esi]
	mov  ax,bpwr10[edi]
    sub  exponent,ax
S3c:
    add  esi,type binpwr10
    add  edi,type bpwr10
    loop S3a

S4:
    fmul onep8	  ; multiply by 10^8
    ; adjust to range 10^8 <= V < 10^9
    call normalize

; Round the mantissa to 8 decimal places
    fadd onehalf     ; add one half
    frndint          ; should truncate fractional part

    ; readjust to 10^8 <= V < 10^9
    call normalize
    
    ret
splitup  ENDP


;---------------------------------------------------------
wrdigits PROC PRIVATE
;
; (Helper procedure) Writes 'ecx' digits of register eax
;  as decimal digits, with leading zeros.
; 
;---------------------------------------------------------
WR1:
    mov  edx,pwr10[ecx*4]
	xor  edx,edx
    div  pwr10[ecx*4]
    and  al,0Fh
    add  al,'0'
    call WriteChar
    mov  eax,edx
    loop WR1
    
    ret
wrdigits ENDP


.data
status  dw 0
showMsg byte "stack top= "
stbyte  byte 0, 0dh, 0ah, 0
.code
;--------------------------------------------------------
power10  PROC uses ebx ecx
;
; power10 expects:  EAX (signed exponent)
; This returns 10.0^(sign*EAX) in the FPU
;--------------------------------------------------------
.data
binpwr10   REAL8  1.0E64, 1.0E32, 1.0E16, 1.0E8, 1.0E4, 1.0E2, 1.0E1
TOPPWR= ($-binpwr10)/type binpwr10
.code
      .IF (eax == 0)
        fld1   ; load a 1
        ret
      .ENDIF

	   ; get the sign of eax
	   mov  bl,0
	   .IF (sdword ptr eax < 0)
	     neg  eax
	   	 inc  bl
      .ENDIF

      ; check for too-large exponent
      .IF (sdword ptr eax > MAXEXPONENT)
        ; complain
        mov  edx,OFFSET ErrMsg
        call WriteString
        ; ...but return 1.0
        fld1
        ret
      .ENDIF

      ; now for the computation
      ; The general idea is that if eax= 11101b, then the value wanted
      ;  is 10^(11101b)= 10^(2^4) * 10^(2^3) * 10^(2^2) * 10^(2^0)
      ; So we use a table of these powers, binpwr10.
      ; we start with 10^1
      fld1
      mov  ecx,TOPPWR
	  mov  esi,(type binpwr10)*(TOPPWR-1)
    P1:
      test  eax,1
      jz    P2
      fmul  binpwr10[esi]
    P2:
	  sub   esi,type binpwr10
      shr   eax,1
      loopnz  P1
      .IF (bl != 0)
        ; take the reciprocal
        fld1
        fdivr   ; reverse division
      .ENDIF
      ret
power10  ENDP
END MAIN