struc Client_Reg_Struc
Client_EDI:      resd    1       ; client's EDI
Client_ESI:      resd    1       ; client's ESI
Client_EBP:      resd    1       ; client's EBP
                 resd    1       ; ESP when pusha instruction is executed
Client_EBX:      resd    1       ; client's EBX
Client_EDX:      resd    1       ; client's EDX
Client_ECX:      resd    1       ; client's ECX
Client_EAX:      resd    1       ; client's EAX
Client_Error:    resd    1       ; doubleword error code
Client_EIP:      resd    1       ; EIP
Client_CS:       resw    1       ; CS
                resw    1       ;   (padding)
Client_EFlags:   resd    1       ; EFLAGS
Client_ESP:      resd    1       ; ESP
Client_SS:       resw    1       ; SS
                resw    1       ;   (padding)
Client_ES:       resw    1       ; ES
                resw    1       ;   (padding)
Client_DS:       resw    1       ; DS
                resw    1       ;   (padding)
Client_FS:       resw    1       ; FS
                resw    1       ;   (padding)
Client_GS:       resw    1       ; GS
                resw    1       ;   (padding)
Client_Alt_EIP:  resd    1
Client_Alt_CS:   resw    1
                resw    1
Client_Alt_EFlags:  resd    1
Client_Alt_ESP:  resd    1
Client_Alt_SS:   resw    1
                resw    1
Client_Alt_ES:   resw    1
                resw    1
Client_Alt_DS:   resw    1
                resw    1
Client_Alt_FS:   resw    1
                resw    1
Client_Alt_GS:  resw    1
                resw    1
endstruc

%MACRo VMMCall 1
        DB 0CDh, 20H
        DD %1
;        %IF     (%1 SHR 16) = VMM
;	INT    20H
;	DD     %1
;        %ELSE
;        ERR    "This is not VMM service"
;        %ENDIF
%ENDMacro
