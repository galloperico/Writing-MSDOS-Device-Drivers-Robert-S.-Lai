;Listing 3-12: A simple Device Driver.
;*******************************************************************************
;* This is a simple Device Driver
;*******************************************************************************

;*******************************************************************************
;* INSTRUCTING THE ASSEMBLER
;*******************************************************************************

cseg         segment para public 'code'
simple       proc    far 
             assume  cs:cseg,es:cseg,ds:cseg

;*******************************************************************************
;* MAIN PROCEDURE
;*******************************************************************************

begin:

;*******************************************************************************
;* DEVICE HEADER REQUIRED BY DOS
;*******************************************************************************
next_dev    dd     -1            ;no other device drivers
attribute   dw     8000h         ;character device
strategy    dw     dev_strategy  ;address of 1st dos call
interrupt   dw     dev_int       ;address of 2nd dos call
dev_name    db     'SIMPLE$ '    ;name of the driver

;*******************************************************************************
;* WORK SPACE FOR THE DEVICE DRIVER
;*******************************************************************************

rh_ofs     dw       ?      ;request header offset
rh_seg     dw       ?      ;request header segment 
msg1       db       07h
           db       'The Waite Group Simple Device Driver! '
           db       0dh,0ah,07h,'$'

;*******************************************************************************
;* THE STRATEGY PROCEDURE
;*******************************************************************************

dev_strategy:                ;First call from DOS
        mov cs:rh_seg,es     ;save request header ptr segment
        mov cs:rh_ofs,bx     ;save request header ptr offset 
        ret

;*******************************************************************************
;* THE INTERRUPT PROCEDURE
;*******************************************************************************

dev_int:                    ;Second call from DOS
        cid                 ;save machine state on entry
        push     ds 
        push     es 
        push     ax 
        push     bx 
        push     cx 
        push     dx 
        push     di 
        push     si 

;perform branch based on the command passed in the req header

        mov      al,es:[bx]+2     ;get command code
        cmp      al,0             ;check for 0
        jnz      exit3            ;no - exit go to error exit
        rol      al,1             ;get offset into table
        lea      di,cmdtab        ;get address of command table
        mov      ah,0             ;clear hi order
        add      di,ax            ;add offset
        jmp      word ptr[di]     ;jump indirect

;command table
;       the command code field of the static request
;       field contains the function to be performed

cmdtab label byte                ;
       dw    init                ;initialization

;*******************************************************************************
;* YOUR LOCAL PROCEDURES
;*******************************************************************************

initial proc near
        lea  dx,msg1             ;initialization
        mov  ah,9                ; message 
        int  21h                 ;doscall
        ret                      ;return
initial endp

;*******************************************************************************
;* DOS COMMAND PROCESSING
;*******************************************************************************

;command 0 initialization

init:   call   initial             ;display a message
        lea    ax,exit             ;get end address (offset)
        mov    es:[bx]+0eh,ax      ;store offset address
        push   cs                  ;get end 
        pop    ax                  ; address (segment)
        mov    es:[bx]+10h,ax      ;store in Break Address

;*******************************************************************************
;* ERROR EXIT
;*******************************************************************************

;Set the Done flag, error flag, and unknown command error code                

exit3: mov     es:word ptr 3[bx],8103h
       jmp     exit1              ;restore environment

;*******************************************************************************
;* COMMON EXIT
;*******************************************************************************

;common exits fall thru code
;       2 sets status to done and no error
;       1 restore callers es:bx
;       0 restore machine state and exit
;

exit2:                          ; Set done flag and no error
        mov    es:word ptr 3[bx],0100h
exit1:  mov    bx,cs:rh_ofs     ;restore req hdr to bx and es 
        mov    es,cs:rh_seg     ;as sabed by dev_strategy
exit0:  pop    si               ;restore all registers
        pop    di
        pop    dx
        pop    cx
        pop    bx
        pop    ax
        pop    ea
        pop    ds
        ret
exit:

;*******************************************************************************
;* END OF PROGRAM
;*******************************************************************************

simple endp
cseg   ends
       end   begin

;that's all folks
