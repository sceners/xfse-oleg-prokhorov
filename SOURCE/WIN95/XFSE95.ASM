segment code32 public align=16 use32
segment code32
DkEmu_Maj_Ver           EQU     01h
DkEmu_Min_Ver           EQU     00h
DkEmu_Req_Num           EQU     0E911h

%include "VCALL.INC"
%include "xfse95.inc"

                        global  XFSE_DDB
XFSE_DDB                DD      0
DDK_Version             DW      400h
Req_Device_Number       DW      DkEmu_Req_Num
Dev_Maj_Ver             DB      DkEmu_Maj_Ver
Dev_Min_Ver             DB      DkEmu_Min_Ver
Flags                   DW      0
VxD_Name                DB      'XFSEWN95'
Init_Order              DD      80000000h
Control_Proc            DD      Control
V86_API_Proc            DD      V86
PM_API_Proc             DD      0
V86_API_CSIP            DD      0
PM_API_CSIP             DD      0
Reference_Data          DD      0
Service_Table_Ptr       DD      0
Service_Size            DD      0
Win32_Service_Table     DD      0
DDB_Prev                DB      'verP'
DDB_Size                DD      50h
DDB_Res1                DB      '1vsR'
DDB_Res2                DB      '2vsR'
DDB_Res3                DB      '3vsR'

Control:
                        cmp     eax, 0000001Bh
                        jz      SysDynamicDeviceInit
                        cmp     eax, 0000001Ch
                        jz      SysDynamicDeviceExit
                        xor     eax, eax
                        clc
                        ret

SysDynamicDeviceInit:
        ;returns CF according to result so there's no need to set/clear CF
        ;explicitly
        xor     eax, eax
        clc
        ret

SysDynamicDeviceExit:
        xor     eax, eax
        clc
        ret

V86:
                xor     eax, eax
		mov	al, [EBP + Client_EAX]
		cmp	al, 0
		jb	.eexit
		cmp	al, 1
		ja	.eexit
		cld	
		call	[v86service + eax*4]
                clc
		jmp	short .exit
.eexit:
		stc	

.exit:
		setb	al
		or	[ebp+Client_EFlags], al 
		retn	
v86service:
           dd xfse_init
           dd xfse_uninit

xfse_init:
        mov     ebx, dword [EBP + Client_EBX]
        mov     [DumpExit], ebx
        mov     ESI, MainInt13
	MOV     EAX,13
	VMMCall Hook_V86_Fault
        jc      .exit
        mov     byte [hook13], 1
.exit:
	ret

xfse_uninit:
        cmp   byte [hook13], 1
        jne   .exit
	mov   ESI,MainInt13
	MOV   EAX,13
	VMMCall Unhook_V86_Fault              ;try to unhook
.exit:
	ret

        ;Following jumps are required  ("handler header") by Install_???_Hook
        ;(trace Install_???_Hook in WinICE) to get PrevHook13 address

	JMP	short MainInt13       ;EB 06  (can be EB 0A too)

      Chain13:
	JMP	[PrevHook13]          ;FF 25 AdrOfPrev
                                      ;Hook will fill [AdrOfPrev] 

MainInt13:                      ;handler begins here!!!

      ;in EBX is (as always) handle of Virtual Machine which caused exception 13
      ;in EBP is (as always) pointer to Client_Reg_Struc

	PUSHFD
	PUSHAD

	CLD
        MOV    AX, Client_CS * 256 + Client_EIP
        VMMCall Map_Flat
        CMP   EAX, -1
        JE    near Quit
        XCHG    EAX,ESI
        MOV     EBX,ESI               

        ;-------skip prefixes
       NextPref:
	LODSB
	mov	EDI,PrefixTable       ;contains prefixes(without LOCK) + 0FH
	MOV	ECX,PrefixTableL
	REPNE	SCASB
	JE	NextPref

       CheckXRX:
        CMP     AL,  23H
        JNE     NotMe13

        sub     esi, BYTE 2
        cmp     dword [EBP + Client_EAX], BYTE 0
        jne     NotMe13

        push    esi
        mov     edi, _stamp1
        mov     ecx, _stamp2 - _stamp1
        rep     cmpsb
        pop     esi
        jz      Gotcha
        mov     edi, _stamp2
        mov     ecx, _stamp3 - _stamp2
        rep     cmpsb
        jnz     NotMe13
Gotcha:
        mov     eax, dword [DumpExit]
        xchg    word [EBP + Client_EIP], ax    ; Return CS:IP in EAX.
        mov     word [EBP + Client_EAX], ax
        shr     eax, 16
        xchg    word [EBP+ Client_CS], ax
        mov     word [EBP+2+Client_EAX], ax
        add     word [EBP + Client_EAX], BYTE 12h    ; Skip stamp opcode.
        xor     eax, eax
        mov     dr7, eax
        mov     dr0, eax
        mov     dr1, eax
        mov     dr2, eax
        mov     dr3, eax
        jmp     short Quit

     NotMe13:                         ;it was not MOV from/to CR/DR or "my" HLT
	CMP	dword [PrevHook13], BYTE 0  ;no previous hook?
	JE	Quit             ;yes -> quit RET
	POPAD
	POPFD
	JMP	Chain13               ;no -> chain

      Quit:
	POPAD
	POPFD
      UnInstFailed:
	RET

PrefixTable	DB 26H,2EH,36H,3EH,64H,65H,66H,67H,0F2H,0F3H,  0FH
PrefixTableL	EQU $-PrefixTable

_stamp1:
 dd 00FC0230Fh,0230FC823h,0D8230FD0h,00FF0230Fh,0E08EF823h
_stamp2:
 dd 00FF8230Fh,0230FC023h,0D0230FC8h,00FD8230Fh,0E08EF023h
_stamp3:

        hook13      db 0
        PrevHook13  dd 0
        DumpExit    dd 0
