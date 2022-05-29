%macro SEGCS 0
   db  02Eh
%endmacro

%macro MovSEG 2
  push %2
  pop  %1
%endmacro

%macro XchgSEG 2
  push %2
  push %1
  pop  %2
  pop  %1
%endmacro

SuccessExitCode EQU 0
ErrExec    EQU 1
ErrWF      EQU 2
ErrOF      EQU 3
ErrOR      EQU 4
ErrMZ      EQU 5
ErrMaskNF  EQU 6

ETX equ 0

PM_STACKSIZE equ 200h

segment code16 public align=16 use16
segment stackseg stack align=16


segment code16
..start:
Fix:
	cld
	push   ds
	MovSEG es, cs
	mov    ds, word [2Ch]
	xor    si, si
.nech:
	lodsb
	cmp    al, 1
	jne    .nech
	lodsb
	mov    di, vxdname
.nech2:
	lodsb
	stosb
	cmp    al, 0
	jne   .nech2

.Tunedargv0:
	pop    ds
	mov    si, 81h
	mov    di, ff
	lea    bx, [di-1]
	mov    cl, byte [80h]
	cmp    cl, 0
	jz     .emptycline
.NCH:
	lodsb
	cmp    al, 20h
	je     .SkipChar
	cmp    al, 9
	jne    .SaveChar
.SkipChar:
	mov    byte [es:bx], 0
	lea    di, [bx+1]
	jmp    short .DecChar
.SaveChar:
	stosb
	mov    bx, di
.DecChar:
	dec    cl
	jne    .NCH

.emptycline:
	MovSEG  es, ds
	MovSEG  ds, cs
	mov    SI, IntroMessage
	CALL   WriteIt
	cmp    byte [ff], 0
	jnz    .win9x
	mov    SI, UsageMessage
	CALL   WriteIt
	MOV    AL, SuccessExitCode
	JMP    short .QQ
.OFE:
	MOV    AL, ErrOF
	JMP    short .QQ
.win9x:
        mov   ax, 3306h
	int   21h      
	cmp   bx, 3205h
	je    .GiveWindows
        mov   ax, 1600h
        int   2fh
        cmp   al, 4
        jae   .checkwinice
.GiveWindows:
        mov   si, Win9xNotP
        jmp   WriteError
.checkwinice:
        xor   ax, ax
        mov   ebp, 4243484Bh
        int   3h
        cmp   ah, 0
        je    .unistxfse
        mov   si, Winice
        jmp   WriteError
.RFE:
	MOV    AL, ErrOR
	JMP    short .QQ
.MZE:
	MOV    AL, ErrMZ
.QQ:
	mov    [cs:xFSECode], AL
	JMP    QuitIT
.unistxfse:
        push  es
	mov   ax, 1684h
	mov   bx, 27h
	int   2Fh
	push  es
	push  di
	pop   dword [vxdldrentry]

	mov   ax, 1684h
	mov   bx, 0E911h
	int   2Fh
	push  es
	push  di
	pop   dword [xfseentry]
	cmp   dword [xfseentry], BYTE 0
	je    .cfile
        call  UninstallxFSE              ; try to uninstall failed
                                           ; xfse
.cfile:
        pop   es
	mov   dx, ff
	mov   ax, 3D00h
	int   21h
	jc    .OFE
	mov   bx, ax
	mov   cx, 32
	sub   sp, cx
	mov   dx, sp
	push  ss
	pop   ds
	mov   ah, 3fh
	int   21h
	jc    .RFE
	cmp   ax, cx
	jne   .RFE
	mov   si, dx
	cmp   word [si], 5A4Dh
	jne   .MZE

	mov   eax, dword [si+0Ah]
	mov   dword [cs:ms], eax

	add   sp, cx
 
	MovSEG ds, cs

	mov   word [mystack], sp
	mov   word [mystack+2], ss

	mov   bx, ss
	mov   ax, sp
	add   ax, BYTE 0Fh
     	shr   ax, 4
     	add   bx, ax
     	mov   ax, es
        sub   bx, ax

        mov   ah, 4Ah
        int   21h
        jnc   .NE1
        mov   si, MemoErr
        jmp   WriteError
.VXDE:
        mov   si, VXDErr
        jmp   WriteError
.NE1:
	mov   ax, 3521h              ; Get original 21h
	int   21h
	mov   word [cs:orig21h], bx
	mov   word [cs:orig21h+2], es

	mov   ax, 1
	mov   dx, vxdname
	call  far [vxdldrentry]
	jc    .VXDE
	mov   ax, 1684h
	mov   bx, 0E911h
	int   2Fh
	jc    .VXDE
	push  es
	push  di
	pop   dword [xfseentry]

	mov   ax, 0
	mov   bx, cs
	shl   ebx, 16
	mov   bx, dump
	call  far [xfseentry]
	jnc   short loadfse
	jmp   short .VXDE
@exec_error:
	mov   byte [cs:xFSECode], ErrExec
	jmp   QuitIT

loadfse:
	mov   si, MessageSuccess
	call  WriteIt
	push  cs
	MovSEG ds, word 0
	MovSEG es, cs
	mov   cx, 100h
	cld
	xor   si, si
	mov   di, vecttable
	rep   movsd
	pop   ds

	xor   eax, eax
	xor   ebx, ebx
	xor   ecx, ecx
	xor   edx, edx
    xor     esi, esi
    xor     edi, edi
    xor     ebp, ebp
    push    ds
    pop     es
    mov     [comlin+2], cs

    mov     ax, 4b01h
    mov     dx, ff
    mov     bx, exest
    int     21h
    jc      @exec_error

    mov     ah, 62h
    int     21h
    mov     [fsepsp], bx
    mov     dx, bx
    mov     es, bx
    mov     ax, word [es:16h]            ;  Parent PSP should be the same
    mov     word [es:0Ch], ax            ;  as segment of exit. As if it was
    mov     bx, cs                       ;  created by command.com
    sub     bx, ax
    shl     bx, 4
    add     bx, fse_exit
    mov     word [es:0Ah], bx
    push    ds
    push    dx
    mov     ds, ax
    mov     dx, bx
    mov     ax, 2522h             ; Set proper int 22h = exit (0ah) in PSP
    int     21h
    pop     dx
    pop     ds
    mov     di, word [sssp]
    inc     di
    inc     di
    cli
    mov     ax, word [sssp+2]
    mov     ss, ax
    mov     sp, di
    sti
    lds     si, [csip]
    push    ds
    push    si
    mov     ds, dx
    mov     es, dx
    xor     ax, ax
    mov     bx, ax
    mov     cx, 0FFh
    mov     bp, 91ch
    retf

@masknf:
    mov   byte [cs:xFSECode], ErrMaskNF
    jmp   Quit

dump:
       push eax
       push es
       pushad
       mov   di, ax
       shr   eax, 16
       mov   es, ax
       mov   ax, _fse01
       mov   bx, _fse02 - _fse01
       lea   dx, [di+200h]
       call  SearchStringByte
       jnc   @masknf
       mov   byte [es:di - (_fse02 - _fse01)], 0CCh
       mov   ax, _fse02
       mov   bx, _fse03 - _fse02
       call  SearchStringByte
       jnc   @masknf

;      install int 3
       mov    byte [es:di+2], 0CCh

       MovSEG es, word 0
       mov    ax, cs
       shl    eax, 16
       mov    ax, i3h1
       xchg   [es:3*4], eax

       mov    [cs:orgi], eax
       popad
       xor   eax, eax
       pop   es
       retf

WriteToFile:
  mov   ah, 40h
  int   21h
  jc    @writefile_error
  cmp   ax, cx
  jne   @writefile_error
retn

@writefile_error:
    mov   byte [cs:xFSECode], ErrWF
    jmp   Quit

SearchStringByte:
; cs:ax = searched string
; from es:di to es:dx = searched place
; bx = string length

        mov   si, ax     ; first string
        mov   cx, bx     ; length in bytes
        SEGCS
        rep   cmpsb      ;
        stc              ; Found !!!
  jz    .exit
        cmp   di, dx     ; End of Second String
  jb    SearchStringByte
.exit:
   ret

i3h1:
    push bp
    mov  bp, sp
    and  byte [bp+7], 0FBh        ; emulate CLD
    mov  word [cs:mzbeg], 0
    mov  word [cs:mzbeg+2], dx
    push ds
    MovSEG ds, word 0
    mov  word [03*4], i3h2
    pop  ds
    pop  bp
    iret
i3h2:
       MovSEG  ds, cs
       xor   eax, eax
       mov   ax, es
       shl   eax, 4
       lea   edi, [di]
       add   eax, edi
       movzx edx, word [mzbeg+2]
       shl   edx, 4
       movzx ecx, word [mzbeg]
       add   edx, ecx
       sub   eax, edx                      ; eax = 'mz' size

       mov   edi, eax

       mov    ah, 3Ch
       mov    dx, ofname
       xor    cx, cx
       int    21h
       jc     @writefile_error
       mov    bx, ax

       mov    ax, word [mzbeg]
       mov    si, word [mzbeg+2]
       mov    dx, ax
       shr    ax, 4
       add    si, ax
       and    dx, 0Fh

       mov   ds, si
       mov   ecx, edi
       mov   ax, di
       shr   ecx, 9
       and   ax, 1FFh
       jz    @@notincpage
       inc   cx
@@notincpage:
       push  bx
       mov   bx, dx
       mov   word [bx], 5A4Dh
       mov   [bx+2], ax
       mov   [bx+4], cx
       mov   eax,  dword [cs:ms]
       mov   dword [bx+0Ah], eax

       pop   bx

       mov   ecx, 8000h
@nq:
       mov   ds, si
       cmp   edi, ecx
       ja    @00q
       mov   ecx, edi
@00q:
       call  WriteToFile
       add   si, 800h

       sub   edi, ecx
       jne   @nq

	MovSEG ds, word 0
	mov   eax, [cs:orgi]
	mov   [3*4], eax
	cli
	lss   sp, [cs:mystack]
	sti
	mov   byte [cs:xFSECode], 0
Quit:
	MovSEG ds, cs
	MovSEG es, word 0
	mov   cx, 100h
	cld
	mov   si, vecttable
	xor   di, di
	rep   movsd

	push  cs
	MovSEG ds, word 0
	push  dword [cs:orig21h]
	pop   dword [ds:21h*4]
	push  word [cs:fsepsp]
	pop   es
	mov   word [es:0ah], QuitIT
	mov   word [es:0ch], cs
	mov   word [22h*4], QuitIT
	mov   word [22h*4+2], cs
	pop   ds

	mov   ah, 4Ch
	int   21h

fse_exit:
    db 0EAh
    dw QuitIT
    dw code16

QuitIT:
        MovSEG ds, cs
        movzx  bx, [xFSECode]
        cmp    bl, 0
        je     .GoExit
        dec    bx
        shl    bx, 1
        mov    si, [bx+ ExFSE]
        CALL   WriteIt
.GoExit:
        cmp  dword [xfseentry], BYTE 0
        je   .RM
        call  UninstallxFSE
.RM:
        mov  ah, 4Ch
        int  21h

UninstallxFSE:
	mov     ax, 1
	call    far [xfseentry]
	mov     ax, 2
	mov     bx, 0E911h
	call    far [vxdldrentry]
	ret

WriteError:
     PUSH  CS
     POP   DS
     PUSH  AX
     PUSH  SI
     mov   SI,PreError
     CALL  WriteIt
     POP   SI
     CALL  WriteIt
     POP   AX
     jmp   short QuitIT

WriteIt:
     CLD
     MOV    AH,02
 WriteItNext:
     LODSB
     CMP    AL,ETX
     JE     WriteItQuit
     MOV    DL,AL
     INT    21H
     JMP    short WriteItNext
 WriteItQuit:
     RET

orig21h  dd 0
vxdldrentry dd 0
xfseentry dd 0
vxdname resb 100h

ExFSE   dw MessageExec, MessageWF, MessageOF
        dw MessageRF, MessageMZ, MessageMaskNF

        ofname    db  'out.exe', 0
        xFSECode  db  0

exest:
evir    dw      0
comlin  dw      cline
        dw      0
fcb1    dd      0
fcb2    dd      0
sssp    dd      0
csip    dd      0
mystack dd      0
fsepsp   dw 0
cline db  00h, 0Dh
      resb 80h
    db 0
ff  resb 100h

vecttable resd   100h
orgi    dd 0
mzbeg   dd 0
mzsize  dd 0
ms      dd 0

_fse01   dd 00100B9FCh,0DA8EC28Eh,0F78BFF33h,0520100B9h
 db 050h
_fse02    dd 0C2835A58h
 db 010h,03Bh,0D3h
_fse03:

PreError        DB 13,10,"Install: ",ETX

MemoErr         DB "DOS memory service failed!",ETX
Win9xNotP       DB "Win9x required!",ETX
Winice          DB "Uninstall WinIce!",ETX
VXDErr          DB "VXD load failed!",ETX

IntroMessage:   DB "ÄÄ[Final Fantasy Security Envelope remover]ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
                DB 13,10,"xFSE 0.01b by Oleg Prokhorov  þ Win'9x þ [10-Mar-2K] þ Mail to: olegpro@mail.ru"
                DB 13,10,"ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ[ USE AT YOUR OWN RISK ]ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ",ETX

UsageMessage    DB 13,10,"Usage: xfse filename",ETX

MessageSuccess  DB 13,10,"xFSE: Listen and obey.",ETX
MessageSignature  DB 13,10,"xFSE: FSE signature found.",ETX
MessageExec     DB 13,10,"xFSE: File execution's failed!",ETX
MessageWF       DB 13,10,"xFSE: File writing's failed!",ETX
MessageOF       DB 13,10,"xFSE: File opening's failed!",ETX
MessageRF       DB 13,10,"xFSE: File reading's failed!",ETX
MessageMZ       DB 13,10,"xFSE: File's not DOS EXE!",ETX
MessageMaskNF   DB 13,10,"xFSE: Mask not found (not FSE)!",ETX
MessageError    DB 13,10,"Can't install xFSE!",ETX

segment stackseg
          resb    PM_STACKSIZE    ; real mode stack