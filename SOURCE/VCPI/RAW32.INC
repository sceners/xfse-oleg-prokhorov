%define RMCALL_VECT     34h     ; vector of RM int caller routine ( >=32h )
%define N_DSC           8       ; number of free descriptors
%define N_INT           8       ; number of free interrupt gates

%macro  seg_descriptor 0-6 0,0,0,0,0,0
  .limit0_15    dw      %1      ; g=granularity bit; x=default bit for a code
  .base0_15     dw      %2      ;  seg, x=big bit for a data seg: if data seg
  .base16_23    db      %3      ;  expands down: b=0 -> upper bound is 64K-1
  .access       db      %4      ;  b=1 -> upper bound is 4G-1, x=big bit for
  .gx00limit16_19 db    %5      ;  a stack seg too -> determines stack
  .base24_31    db      %6      ;  address-size attribute (sp <-> esp)
%endmacro

%macro  TSS 0-26 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104
.backlink       dw      %1, 0           ; selector of the outgoing task's TSS
.esp0           dd      %2              ; privilege level 0 stack pointers
.ss0            dd      %3
.esp1           dd      %4              ; privilege level 1 stack pointers
.ss1            dd      %5
.esp2           dd      %6              ; privilege level 2 stack pointers
.ss2            dd      %7
.@cr3           dd      %8
.eip            dd      %9
.eflags         dd      %10
.@eax           dd      %11
.@ecx           dd      %12
.@edx           dd      %13
.@ebx           dd      %14
.@esp           dd      %15
.@ebp           dd      %16
.@esi           dd      %17
.@edi           dd      %18
.@es            dd      %19
.@cs            dd      %20
.@ss            dd      %21
.@ds            dd      %22
.@fs            dd      %23
.@gs            dd      %24
.ldtr           dw      %25, 0,0
.io_map_base    dw      %26             ; offset I/O map from the start of
%endmacro                               ;  the Task State Segment

segment code16 public align=16 use16
segment stackseg stack align=16

; Move relatively adjusted pointer to register
%macro  @rlp 2
        mov %1,%2
        sub %1,[code32a]
%endmacro
