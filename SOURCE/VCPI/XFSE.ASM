;%define SWAT            1

%macro SEGCS 0
   db  02Eh
%endmacro

%macro SEGES 0
   db  026h
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

PM_STACKSIZE equ 200h
HIGHEST_INT  equ 20h
UNINSTALLCODE equ  0EDA0DEDAh

       SuccessExitCode EQU 0
       ErrExec    EQU 1
       ErrWF      EQU 2
       ErrOF      EQU 3
       ErrOR      EQU 4
       ErrMZ      EQU 5
       ErrMaskNF  EQU 6

_EDI    equ     -32                  ; dword
_ESI    equ     -28                  ; dword
_EBX    equ     -16                  ; dword
_EDX    equ     -12                  ; dword
_ECX    equ     -08                  ; dword
_EAX    equ     -04                  ; dword
_AL     equ     _EAX                 ; byte
_AH     equ     (_EAX+1)             ; byte
_EBP    equ     0                   ; dword
_ERCODE equ     04                  ; dword
_EIP    equ     08                  ; dword
_CS     equ     12                  ; dword
_EFLAGS equ     16                  ; dword
_ESP    equ     20                  ; dword
_SS     equ     24                  ; dword
_ES     equ     28                  ; dword
_DS     equ     32                  ; dword
_FS     equ     36                  ; dword
_GS     equ     40                  ; dword

REPE_CODE  equ 0F3h
REPNE_CODE equ 0F2h
LOCK_CODE  equ 0F0h
SEG_CS     equ 2Eh
SEG_SS     equ 36h
SEG_DS     equ 3Eh
SEG_ES     equ 26h
SEG_FS     equ 64h
SEG_GS     equ 65h

INSBCode       equ 6Ch
INSWCode       equ 6Dh
OUTSBCode      equ 6Eh
OUTSWCode      equ 6Fh
InDxBytePortCode  equ 0ECh
InDxWordPortCode  equ 0EDh
InBytePortCode    equ 0E4h
InWordPortCode    equ 0E5h
OutDxBytePortCode equ 0EEh
OutDxWordPortCode equ 0EFh
OutBytePortCode   equ 0E6h
OutWordPortCode   equ 0E7h

%include "raw32.inc"

segment code16

_NewExc13:
        push  ebp
        mov   bp, sp
        pushad
        pushf
        cld
        test    byte [bp + _EFLAGS+2],2    ; Test VM flag
        jnz     V86Fault

_EMMExc13:
        popf
        popad
        pop  ebp
_OldExc13:
        db  66h, 0EAH
        dd  0
        dd  0

V86Fault:
        mov     di, 0
segreloc equ $-2
        MovSEG  ds, word [cs:vcpidata16sel+di]
        MovSEG  es, word [cs:vcpizerosel+di]  ; Load 4Gb selector

        movzx   esi,word [bp + _CS]     ; Get CS of V86
        shl     esi,4
        movzx   ebx,word [bp + _EIP]    ; Get IP
        add     esi,ebx                 ;

        mov     dword [GPFRegs+di], 0
        mov     byte [Zip+di], 0
        cld
.GetFaultyCommand:
        db 067h
        SEGES
        lodsb                ; Fetch instruction
        inc   byte [Zip+di]
        cmp   al, 66h
  jne   .TryAddrModeChange

        mov   byte [GPFRegs+di], 1
  jmp   short .GetFaultyCommand

.TryAddrModeChange:
        cmp   al, 67h
  jne   .TryReps

        mov   byte [GPFAddr+di], 1
  jmp   short .GetFaultyCommand

.TryReps:
        cmp   al, REPE_CODE
  je    .FoundReps

        cmp   al, REPNE_CODE
  jne   .TryLock

.FoundReps:
        mov   byte [GPFRep+di], al
  jmp   short .GetFaultyCommand

.TryLock:
        cmp   al, LOCK_CODE
  jne   .TrySegment

        mov   byte [GPFLock+di], 1
  jmp   short .GetFaultyCommand

.TrySegment:
        cmp   al, SEG_CS
  je    .FoundSegment
        cmp   al, SEG_SS
  je    .FoundSegment
        cmp   al, SEG_DS
  je    .FoundSegment
        cmp   al, SEG_ES
  je    .FoundSegment
        cmp   al, SEG_FS
  je    .FoundSegment
        cmp   al, SEG_FS
  jne   .Other

.FoundSegment:
        mov   byte [GPFSegVal+di], al
  jmp   short .GetFaultyCommand
.Other:

        cmp     al,0F4h                 ; Is it HLT?
        je      near .HLT               ;

        cmp     al, 0Fh
        jne     near _EMMExc13
        db 067h
        SEGES
        lodsb                ; Fetch instruction
        cmp     al, 23h
        jne     near _EMMExc13
        cmp     dword [bp + _EAX], BYTE 0
        jne     near _EMMExc13

        lea     edi, [di]
        dec     esi
        dec     esi
        push    esi
        push    di

        push    ds
        push    es
        XchgSEG ds, es
        add     edi, _stamp1
        mov     ecx, _stamp2 - _stamp1

        db      067h
        rep     cmpsb
        pop     es
        pop     ds
        pop     di
        pop     esi
        jz      .Gotcha
        push    esi
        push    di
        push    ds
        push    es
        XchgSEG ds, es
        sub     esi, BYTE 3
        add     edi, _stamp2
        mov     ecx, _stamp3 - _stamp2
        db      067h
        rep     cmpsb
        pop     es
        pop     ds
        pop     di
        pop     esi
        jnz     .EMMExc13_1
.Gotcha:
        mov     ax, code16
        shl     eax, 16
        mov     ax, dump
        xchg    word [bp + _EIP], ax    ; Return CS:IP in EAX.
        mov     word [bp + _EAX], ax
;       add     word [bp + _EAX], 12h
        shr     eax, 16
        xchg    word [bp + _CS], ax
        mov     word [bp + 2 + _EAX], ax
        mov     byte [Zip+di], 0
        jmp     short Quit

.EMMExc13_1:
        jmp     _EMMExc13
.HLT:   cmp     dword [bp + _EBP], UNINSTALLCODE
        jnz     .EMMExc13_1

Quit:
        mov     byte [di+PMI], 0
        lea     si, [di+xidt]
        sidt    [si]
        mov     esi, dword [xidt+2+di]
        add     esi, BYTE 13*8
        mov     eax, dword [_OldExc13+di+2]
        mov     [es:esi], ax
        shr     eax, 16
        mov     [es:esi+6], ax
        mov     ax, word [_OldExc13+di+6]
        mov     [es:esi+2], ax
        movzx   ax, byte [Zip+di]
        add     [bp + _EIP], ax

        popf
        popad
        pop  ebp
        add  sp, BYTE 4
        iretd
_stamp1:
 dd 00FC0230Fh,0230FC823h,0D8230FD0h,00FF0230Fh,0E08EF823h
_stamp2:
 dd 00FF8230Fh,0230FC023h,0D0230FC8h,00FD8230Fh,0E08EF023h
_stamp3:
xidt            dw       0
                dd       0
vcpizerosel     dw      -1
vcpicode16sel   dw      -1
vcpidata16sel   dw      -1
GPFRegs         db      0
GPFAddr         db      0
GPFRep          db      0
GPFSegVal       db      0
GPFLock         db      0
Zip             db      0
PMI             db      0               ; PM part installed
..start:
	cld
	MovSEG es, cs
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
	jnz    .cfile
	mov    SI, UsageMessage
	CALL   WriteIt
	MOV    AL, SuccessExitCode
	JMP    short .QQ
.OFE:
	MOV    AL, ErrOF
	JMP    short .QQ
.RFE:
	MOV    AL, ErrOR
	JMP    short .QQ
.MZE:
	MOV    AL, ErrMZ
.QQ:
	mov    [cs:xFSECode], AL
	JMP    QuitIT

.cfile:
     mov    dx, ff
     mov    ax, 3D00h
     int    21h
     jc     .OFE
     mov     bx, ax
     mov     cx, 32
     sub     sp, cx
     mov     dx, sp
     push    ss
     pop     ds
     mov     ah, 3fh
     int     21h
     jc      .RFE
     cmp     ax, cx
     jne     .RFE
     mov     si, dx
     cmp     word [si], 5A4Dh
     jne     .MZE

     mov     eax, dword [si+0Ah]
     mov     dword [cs:ms], eax

     add     sp, cx
 
     MovSEG  ds, cs

     mov    word [mystack], sp
     mov    word [mystack+2], ss
     push  es
     mov   ax, 3521h              ; Get original 21h
     int   21h
     mov   word [orig21h], bx
     mov   word [orig21h+2], es
     pop   es


        mov ax, cs
        and eax, 0FFFFh
        shl eax,4                       ; eax=linear address of code16
        mov [code16a wrt code16],eax

        mov  bx, ss
        mov  ax, sp
        add  ax, BYTE 0Fh
        shr  ax, 4
        add  bx, ax
        add  bx, 0FFh
        and  bx, 0FF00h
        mov  [pagedirseg], bx
        add  bx, 2*4096 / 16
        mov  ax, es
        sub  bx,  ax

        mov  ah, 4Ah
        int  21h
        jnc   .NE1
        mov   si, MemoErr
        jmp   WriteError
.NE1:
        call  check_VCPI
        jnc   .NE2
        mov   si, VCPINotP
        jmp   WriteError

.NE2:
        sgdt  [vcpigdt]
        inc   word [vcpigdt]
        sidt  [vcpiidt]
        inc   word [vcpiidt]
        mov   bx, [vcpigdt]
        mov   cx, bx
        call  GetMEM
        mov   [gdtSEG], ax
        mov   es, ax
        mov   eax, [vcpigdt+2]
        call  CopyHMEM

        mov   bx, [vcpiidt]
        mov   cx, bx
        call  GetMEM
        mov   [idtSEG], ax
        mov   es, ax
        mov   eax, [vcpiidt+2]
        call  CopyHMEM

setup_dsc:
        mov eax,[code16a wrt code16]
        add [VCPIidtr wrt code16],eax
        add [VCPIgdtr wrt code16],eax
        add [gdtr+2 wrt code16],eax
        mov [idtr+2 wrt code16],ebx     ; Set IDT base

        mov [code16dsc.base0_15 wrt code16],ax
        mov [data16dsc.base0_15 wrt code16],ax

        add [mainTSSdsc.base0_15 wrt code16],ax
        shr eax,8
        mov [code16dsc.base16_23 wrt code16],ah
        mov [data16dsc.base16_23 wrt code16],ah
        mov [mainTSSdsc.base16_23 wrt code16],ah

        xor eax, eax
        mov ax, ss
        shl eax, 4
        mov [stack16dsc.base0_15 wrt code16],ax
        shr eax,8
        mov [stack16dsc.base0_15 wrt code16],ah

        mov cx, [pagedirseg]
        mov es, cx
        shr cx, 12 - 4
        mov ax, 0de06h
        int 67h
        mov [VCPIcr3], edx

        inc cx
        mov ax, 0de06h
        int 67h

        xor di, di
        mov cx,2*4096/4
        xor eax,eax
        rep stosd
        or  edx, BYTE 67h
        mov [es:0], edx
                                        ; Get current 1st page table (0-4MB)
        mov di, 4096                    ; es:di=ptr to 4KB page table buffer
        mov si,VCPIdsc0 wrt code16      ; ds:si is filled in by server (3 dsc)
        mov ax,0de01h                   ; al=1 -> Get PM interface
        int 67h
        cmp ah,0                        ; No error?
        je  .NE3
        mov  si, VCPIErr
        jmp  WriteError
.NE3:
        mov [VCPIcall wrt code16],ebx   ; ebx=offset server's PM entry point
                                        ;  (in VCPIdsc0)
        mov eax, [vcpigdt+2]
        movzx ecx, word [vcpigdt]
        call MapPages
        mov  eax, [vcpigdt+2]
        and  eax, 0FFFh
        or   edx, eax
        mov  [vcpigdt+2], edx

        mov eax, [vcpiidt+2]
        movzx ecx, word [vcpiidt]
        call MapPages
        mov  eax, [vcpiidt+2]
        and  eax, 0FFFh
        or   edx, eax
        mov  [vcpiidt+2], edx

        mov ax,[VCPIsel0 wrt code16]
        mov [VCPIcall+4 wrt code16],ax
        mov eax,[VCPIcr3 wrt code16]
        mov [mainTSS.@cr3 wrt code16],eax

        mov ax,[mainTSSsel wrt code16]
        mov [VCPItask wrt code16],ax

        in  al, 21h
        mov bl, al
        mov al, 0FFh
        out  21h, al
        in  al, 0A1h
        mov  bh, al
        mov al, 0FFh
        out  0A1h, al
        mov  [irqmask], bx

%ifdef  SWAT
        call SWATinit
%endif
        cli
        mov ax,[code16sel wrt code16]
        mov [VCPItargetPM+4 wrt code16],ax
        mov dword [VCPItargetPM wrt code16],.inPM
        mov esi,VCPIcr3                 ; Switch to protected mode
        add esi,[code16a wrt code16]    ; esi=system register table address
        mov ax,0de0ch                   ; al=ch -> Switch CPU mode
        int 67h
        mov  si, VCPIErr
        jmp  WriteError
.inPM:

        cld
        mov  ds, [cs:zerosel wrt code16]
        mov  es, [cs:data16sel wrt code16]
        movzx edi, word [cs:gdtSEG]
        shl  edi, 4
        mov  esi, [cs:vcpigdt+2]
        xor  ebx, ebx

.nc:
        cmp  word [esi+ebx], 0FFFFh       ; size 15..0
        jnz  near .wrongsel
        test byte [esi+ebx+4], 0F0h       ; base 23..16
        jnz  near .wrongsel
        cmp  word [esi+ebx+6], BYTE 0     ; base 31..24 & size 19..16
        jnz  .maybe4G
        cmp  byte [esi+ebx+5], 9Bh        ; code
        jnz  near .wrongsel

        cmp   word [es:vcpicode16sel], -1
        jnz  near .wrongsel
        xor  eax, eax
        mov  al, byte [esi+ebx+4]
        shl  eax, 16
        mov  ax, word [esi+ebx+2]
        mov  edx, [es:code16a]
        mov  ecx, edx
        sub  ecx, eax
        cmp  eax, edx
        ja   .wrongsel
        add  eax, 0FFFFh
        add  edx, ..start
        cmp  eax, edx
        jb   .wrongsel
        call checkSEL
        jc   .wrongsel
        mov  [es:vcpicode16sel], bx
        mov  [es:segreloc], cx
        jmp  short .wrongsel

.maybe4G:
        cmp  dword [esi+ebx+2], 93000000h 
        jnz  .wrongsel
        cmp  word [esi+ebx+6], 0CFh 
        jnz  .wrongsel
        cmp  word [es:vcpizerosel], -1
        jnz  .wrongsel
        call checkSEL
        jc   .wrongsel
        mov  [es:vcpizerosel], bx

.wrongsel:
        add  bx, BYTE 8
        cmp  bx, word [cs:vcpigdt]
        jb    near .nc

        xor  edx, edx
        xor  bx, bx
        mov  dx, [es:vcpicode16sel]
        cmp  dx, -1
        je   near .rtovm

.nd
        mov  eax, dword [esi+edx]
        cmp  dword [esi+ebx], eax
        jne  .wrongdata
        mov  eax, dword [esi+edx+4]
        mov  ah, 93h
        cmp  dword [esi+ebx+4], eax
        jne  .wrongdata
        call checkSEL
        jc   .wrongsel
        mov  [es:vcpidata16sel], bx
        jmp  short .df
.wrongdata:
        add  bx, BYTE 8
        cmp  bx, word [es:vcpigdt]
        jb   .nd
.df:

        MovSEG es, ds
        movzx  ecx, word [cs:vcpiidt]
        movzx edi, word [cs:idtSEG]
        shl  edi, 4
        mov  esi, [cs:vcpiidt+2]
        push esi
        db  067h
        rep  cmpsb
        pop  esi
        jnz  .rtovm

; Install new Exc13
        xor  eax, eax
        mov  es, [cs:data16sel wrt code16]
        mov  ax, word [13*8+6+esi]
        mov  word [es:_OldExc13+4], ax

        mov  eax, dword [13*8+esi]

        mov  word [es:_OldExc13+2], ax
        shr  eax, 16
        mov  word [es:_OldExc13+6], ax
        mov  ax, [es:vcpicode16sel]
        shl  eax, 16
        mov  ax, _NewExc13
        add  ax, [es:segreloc]
        mov  dword [13*8+esi], eax
        mov  word [13*8+6+esi], 0
        mov  byte [es:PMI], 1

.rtovm:
        push dword 0                    ; V86 gs
        push dword 0                    ; V86 fs
        push dword code16               ; V86 ds
        push dword code16               ; V86 es
        push dword stackseg             ; V86 ss
        push dword PM_STACKSIZE         ; V86 esp
        push dword 3002h                ; eflags (dummy, _not_ value in V86)
        push dword code16               ; V86 cs
        push dword .inRM                ; V86 eip
        mov ds,[cs:VCPIsel1 wrt code16]
        mov ax,0de0ch                   ; al=ch -> Switch CPU mode
        call dword far [cs:VCPIcall wrt code16] ; Switch to V86 mode
.inRM:

        mov  bx, [irqmask]
        mov  al, bl
        out  21h, al
        mov  al, bh
        out  0A1h, al
        cmp  byte [PMI], 0
        jne  loadfse
        mov  si, VCPIErr
        jmp  WriteError
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

    xor     eax, eax
    xor     ebx, ebx
    xor     ecx, ecx
    xor     edx, edx
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
    jmp   QuitIT

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
    jmp   QuitIT

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
       and    dx, BYTE 0Fh

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
     mov   byte [xFSECode], 0
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
        cmp  byte [PMI], 0
        je   .RM
        mov  ebp, UNINSTALLCODE
        hlt
.RM:
        mov  ah, 4Ch
        int  21h


CopyHMEM:
        mov     si, struct

        mov     [si+12h], ax
        shr     eax, 10h
        mov     [si+14h], al
        mov     [si+17h], ah
        mov     eax, es
        shl     eax, 4
        mov     [si+1Ah], ax
        shr     eax, 10h
        mov     [si+1Ch], al
        mov     [si+1Fh], ah

        mov     ah, 87h
        shr     cx, 1
        MovSEG  es, cs
        int     15h
        ret
GetMEM:
        add   bx, BYTE 0Fh
        shr   bx, 4
        mov   ah, 48h
        int   21h
        ret

MapPages:
        mov ebx, eax
        add ebx, ecx
        and eax, 0FFFFF000h
        add ebx, 0FFFh
        and ebx, 0FFFFF000h
        or  eax, BYTE 67h
        movzx edx, di
        sub   dx, 1000h
        shl   edx, 12 - 2
.np
        cmp di, 2000h
        ja  .eexit
        mov [es:di], eax
        add di, BYTE 4
        add eax, 1000h
        cmp eax, ebx
        jb  .np
        clc
        ret
.eexit
        stc
        ret

check_VCPI:
        push word 0
        pop es
        cmp dword [es:4*67h], BYTE 0    ; Check if int 67h handler exists
        je short .no_VCPI
        mov ax,0de00h                   ; al=0 -> VCPI installation check
        int 67h
        cmp ah,0                        ; No error?
        je short .VCPI
.no_VCPI:
        stc
        ret
.VCPI:
;       clc                             ; Cleared by CMP
        ret

checkSEL:
        mov eax, [esi+ebx]
        cmp [edi+ebx], eax
        jne .eexit
        mov eax, [esi+ebx+4]
        cmp [edi+ebx+4], eax
        jne .eexit
        clc
        ret
.eexit:
        stc
        ret

ETX equ 0

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
     jmp   QuitIT

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

%ifdef  SWAT
SWATinit:
        push word 0
        pop es
        cmp dword [es:4*67h], BYTE 0    ; Check if int 67h handler exists
        je short .exit

        mov ax,0def0h                   ; al=f0h -> Determine dbgr presence
        int 67h
        cmp ah,0                        ; No error?
        jne short .exit

        mov ax,0def2h                   ; al=f2h -> Init debugger interface
        push cs
        pop es
        mov di,SWATdsc                  ; es:di=addr of GDT entries for dbgr
        mov bx,SWATdsc-gdt              ; bx=first selector for debugger
        int 67h
        cmp ah,0                        ; No error?

        mov eax,[idtr+2 wrt code16]
        shr eax,4
        mov es,ax
        xor di,di                       ; es:di=addr IDT entry for exc # bx
        xor bx,bx                       ; Start with exception 0
        mov cx,32                       ; Try to set all exception handlers
.nextexc:
        mov ax,0def3h                   ; al=f3h -> Initialize dbgr IDT entry
        int 67h                         ; no err check; init what can be init
        inc bx                          ; next exception
        add di, BYTE 8                  ; nxt IDT entry 8 bytes higher in mem
        loop .nextexc

.exit:
        ret
%endif
align 4
orig21h  dd 0
struct          resb 16     ; zeros (used by BIOS)
        dw      0ffffh      ; :10h source segment length in bytes (2*CX-1 or greater)
                resb 3      ; :12h 24-bit linear source address, low byte first
        db      93h         ; :15h source segment access rights (93h)
        dw      0           ; :16h (386+) extended access rights and high byte of source address
        dw      0ffffh      ; :18h destination segment length in bytes (2*CX-1 or greater)
                resb 3      ; :1Bh 24-bit linear destination address, low byte first
        db      93h         ; :1Ch destination segment access rights (93h)
        dw      0h          ; :1Dh (386+) extended access rights and high byte of destin. address
                resb 16     ; :20h zeros (used by BIOS to build CS and SS descriptors)

vcpigdt         dw      0
                dd      0
vcpiidt         dw      0
                dd      0
pagedirseg      dw      0
irqmask         dw      0
gdtSEG          dw      0
idtSEG          dw      0
VCPIcall        dd      0               ; 32-bit offset
                dw      0               ; selector
code16a         dd      0               ; linear address of code16

VCPIcr3         dd      0               ; physical address of page directory
VCPIgdtr        dd      gdtr
VCPIidtr        dd      idtr
VCPIldt         dw      0
VCPItask        dw      0
VCPItargetPM    dd      0               ; 32-bit offset
                dw      0               ; selector

align 4
gdt             seg_descriptor
zerodsc         seg_descriptor  0ffffh, 0, 0, 10010010b, 11001111b, 0
code16dsc       seg_descriptor  0ffffh, 0, 0, 10011010b, 0,         0
data16dsc       seg_descriptor  0ffffh, 0, 0, 10010010b, 0,         0
stack16dsc      seg_descriptor  0ffffh, 0, 0, 10010010b, 0,         0
mainTSSdsc      seg_descriptor  67h, mainTSS, 0, 10001001b, 0, 0
VCPIdsc0        seg_descriptor          ; 68h bytes for task state. Default
VCPIdsc1        seg_descriptor          ;  I/O map base=68h(104d). That's >=
VCPIdsc2        seg_descriptor          ;  TSS limit -> no I/O permission map
%ifdef  SWAT
SWATdsc         times 30 dd 0,0         ; 386SWAT needs 30 dscs (set to 0)
%endif

gdtr            dw      $-gdt-1, gdt, 0

align 16
idt             resb    (HIGHEST_INT+1)*8
idtr            dw      (HIGHEST_INT+1)*8-1,0,0
mainTSS         TSS     0, PM_STACKSIZE, stack16dsc-gdt

zerosel         dw      zerodsc    -gdt
code16sel       dw      code16dsc  -gdt
data16sel       dw      data16dsc  -gdt
stack16sel      dw      stack16dsc-gdt
mainTSSsel      dw      mainTSSdsc -gdt
VCPIsel0        dw      VCPIdsc0   -gdt
VCPIsel1        dw      VCPIdsc1   -gdt
VCPIsel2        dw      VCPIdsc2   -gdt

_fse01   dd 00100B9FCh,0DA8EC28Eh,0F78BFF33h,0520100B9h
 db 050h
_fse02    dd 0C2835A58h
 db 010h,03Bh,0D3h
_fse03:


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
PreError        DB 13,10,"Install: ",ETX

MemoErr         DB "DOS memory service failed!",ETX
VCPINotP        DB "VCPI required!",ETX
VCPIErr         DB "VCPI service failed!",ETX

IntroMessage:   DB "ÄÄ[Final Fantasy Security Envelope remover]ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
                DB 13,10,"xFSE 0.01b by Oleg Prokhorov  ş  VCPI  ş [10-Mar-2K] ş Mail to: olegpro@mail.ru"
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

