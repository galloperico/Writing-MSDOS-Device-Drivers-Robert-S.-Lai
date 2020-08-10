;Listing 6-12: The entire listing of a Clock Device Driver
;*******************************************************************************
;* This is a Clock Device Driver
;* Author:  Robert S. Lai
;* Date:    27 November 1991
;* Purpose: A Clock Driver based on the MM58167A clock chip 
;*******************************************************************************

;*******************************************************************************
;* INSTRUCTING THE ASSEMBLER
;*******************************************************************************

timer      segment at       0h     ;int 1c segment
           org     1ch*4
timer_ofs  label   word
timer_seg  label   word
timer      ends

cseg       segment  para public   'code'
clock      proc     far
           assume   cs:cseg, es:cseg, ds:cseg

;structures for the Device Driver
dosdate struc           ;DOS DATE structure
dos_day dw      ?       ;days sin 1/1/80
dos_min db      ?       ;minutes
dos_hr  db      ?       ;hours
dos_hun db      ?       ;hundredths of a second
dos_sec db      ?       ;seconds
dosdate ends            ;end of struc

;structures

rh              struc          ;request header
rh_len          db      ?      ;len of packet
rh_unit         db      ?      ;unit code
                               ;(block devices only)
rh_cmd          db      ?      ;device driver command
rh_status       dw      ?      ;returned by device driver
rh_res1         dd      ?      ;reserved
rh_res2         dd      ?      ;reserved
rh              ends           ;

rh0             struc          ;Initialization (command 0)
rh0_rh          db      size rh dup (?) ;fixed portion
rh0_nunits      db      ?      ;number of units
                               ;(block devices only)
rh0_brk_ofs     dw      ?      ;offset address for break
rh0_brk_seg     dw      ?      ;segment address for break 
rh0_bpb_tbo     dw      ?      ;offset address for pointer 
                               ;to BPB array
rh0_bpb_tbs     dw      ?      ;segment address for pointer 
                               ;to BPB array
rh0_drv_lts     db      ?      ;first available drive 
                               ;(DOS 3+) (Block only)
rh0             ends           ;

rh4             struc          ;INPUT (command 4) 
rh4_rh          db      size rh dup(?)  ;fixed portion
rh4_media       db      ?      ;media descriptor from DPB
rh4_buf_ofs     dw      ?      ;offset address of 
                               ;data transfer area
rh4_buf_seg     dw      ?      ;segment address of
                               ;data transfer area
rh4_count       dw      ?      ;transfer count
                               ;(sectors for block)
                               ;(bytes for character)
rh4_start       dw      ?      ;start sector number
                               ;(block only)
rh4             ends

rh8             struc          ;INPUT (command 8) 
rh8_rh          db      size rh dup(?)  ;fixed portion
rh8_media       db      ?      ;media descriptor from DPB
rh8_buf_ofs     dw      ?      ;offset address of 
                               ;data transfer area
rh8_buf_seg     dw      ?      ;segment address of
                               ;data transfer area
rh8_count       dw      ?      ;transfer count
                               ;(sectors for block)
                               ;(bytes for character)
rh8_start       dw      ?      ;start sector number
                               ;(block only)
rh8             ends

rh9             struc          ;INPUT (command 9) 
rh9_rh          db      size rh dup(?)  ;fixed portion
rh9_media       db      ?      ;media descriptor from DPB
rh9_buf_ofs     dw      ?      ;offset address of 
                               ;data transfer area
rh9_buf_seg     dw      ?      ;segment address of
                               ;data transfer area
rh9_count       dw      ?      ;transfer count
                               ;(sectors for block)
                               ;(bytes for character)
rh9_start       dw      ?      ;start sector number
                               ;(block only)
rh9             ends

;commands that do not have unique portions to the request header
;       INPUT_STATUS   (command 6)
;       INPUT_FLUSH    (command 7)
;       OUTPUT_STATUS  (command 10)
;       OUTPUT_FLUSH   (command 11)
;       OPEN           (command 13)
;       CLOSE          (command 14)
;       REMOVABLE      (command 15)

;*******************************************************************************
;* MAIN PROCEDURE
;*******************************************************************************

begin:

;*******************************************************************************
;* DEVICE HEADER REQUIRED BY DOS 
;*******************************************************************************

next_dev    dd   -1             ;no other drivers following
attribute   dw   8008h          ;char, clock device
strategy    dw   dev_strategy   ;Strategy routine address
interrupt   dw   dev_interrupt  ;Interrupt routine address
dev_name    db   'CLOCK$'       ;name of our Clock driver
 
;*******************************************************************************
;* WORK SPACE FOR THE DEVICE DRIVER
;*******************************************************************************

rh_ofs  dw      ?       ;offset address of the request header
rh_seg  dw      ?       ;segment address of the request header

table1  label   byte
jan     db      31
feb     db      28
mar     db      31
apr     db      30
may     db      31
jun     db      30
jul     db      31
ago     db      31
sep     db      30
oct     db      31
nov     db      30
dec     db      31

dosdays         dw       0        ;DOS date (days since 1/1/80)
clock_port      dw       0        ;clock chip base address
old1c           label    dword    ;old timer interrupt 1C
old1c_ofs       dw       ?        ; offset
old1c_seg       dw       ?        ; segment
refresh         dw       0        ;screen update indicator
mode            db       0        ;color = 0, mono = 1
scn_pos         dw       144      ;column 72 (includes attribute)
scn_port        dw       03dah    ;video status port for color
                                  ;03bah for mono
scn_reg         dw       0b800h   ;video memory address for color
                                  ;0b000h for mono
time            dw       8 dup (003ah)   ;time display

;*******************************************************************************
;* THE STRATEGY PROCEDURE
;*******************************************************************************

dev_strategy: mov  cs:rh_seg,es  ;save the segment address
              mov  cs:rh_ofs,bx  ;save the offset address
              ret                ;return to DOS

;*******************************************************************************
;* THE INTERRUPT PROCEDURE
;*******************************************************************************

;device interrup handler - 2nd call from DOS
dev_interrupt:
         cld                   ;save machine state on entry
         push   ds
         push   es
         push   ax 
         push   bx 
         push   cx 
         push   dx 
         push   di 
         push   si 
           
         mov    ax,cs:rh_seg   ;restore ES as saved by STRETEGY call  
         mov    es,ax          ;  
         mov    bx,cs:rh_ofs   ;restore BX as saved by STRATEGY call  

;jump to appropriate routine to process command
         mov
         rol
         lea
         mov
         add
         jmp

;CMDTAB is the command table that contains the word address
;for each command. The request header will contain the
;commnad desired. The INTERRUPT routine will jump through an
;address corresponding to the requested command to get to
;the appropriate command processing routine.

CMDTAB  label    byte             ;* = char devices only
        dw       INITIALIZATION   ; initialization
        dw       MEDIA_CHECK      ; media check (block only)
        dw       GET_BPB          ; build bpb
        dw       IOCTL_INPUT      ; ioctl in
        dw       INPUT            ; input (read)
        dw       ND_INPUT         ; *nondestructive input no wait
        dw       INPUT_STATUS     ; *input status
        dw       INPUT_FLUSH      ; *input flush
        dw       OUTPUT           ; output (write)
        dw       OUTPUT_VERIFY    ; output (write) with verify
        dw       OUTPUT_STATUS    ; *output status
        dw       OUTPUT_FLUSH     ; *output flush
        dw       IOCTL_OUT        ; ioctl output
        dw       OPEN             ; device open
        dw       CLOSE            ; device close
        dw       REMOVABLE        ; removable media
        dw       OUTPUT_BUSY      ; output til busy

;*******************************************************************************
;* YOUR LOCAL PROCEDURES
;*******************************************************************************

hex2bcd proc   near    ;convert AL from Hex to BCD
;uses   ax,cx
        push   cx      
        mov    cl,10   ;divide by 10     
        mov    ah,0    ;setup for divide
        div    cl      ;get 10's digits
        mov    cl,4    ;shift count
        shl    al,cl   ;place 10's in left half 
        or     al,ah   ;add back 1's
        pop    cx
        ret            ;return to caller
hex2bcd endp

bcd2hex proc   near    
;uses ax,cx
        push   cs
        mov    ah,0    ;setup for divide
        push   ax      ;save for 1's processing
        mov    cl,16   ;divide for left half of byte
        div    cl      ; to get 10's digits
        mov    ah,0    ;have 10's digits
        mov    cl,10   ;convert to base 10
        mul    cl      ; by multiplying by 10
        pop    cx      ;process 1's digits
        and    cl,0fh  ;keep 1's only
        add    al,cl   ;add 1's to 10's
        pop    cx
        ret            ;return to caller
bcd2hex endp

cvt2asc proc   near    ;get chip data & converts to ASCII
        in     al,dx           ;get (BCD) chip data
        mov    cl,10h          ;separate 10's digits
        div    cl              ;al=10's, ah=1's
        or     ax,3030h        ;convert to ascii
        ret 
cvt2asc endp

display proc   near            ;calculates time for display
        push   ax              ;save registers used
        push   bx
        push   cx
        push   dx
        mov    dx,cs:clock_port;get chip's base address
        add    dx,4            ;base+4 = hours
        call   cvt2asc         ;get hours and convert
        lea    bx,cs:time      ;move to Time string
        mov    cs:[bx],al      ;tens of hrs
        mov    cs:[bx+2],ah    ;hrs
        dec    dx              ;base+3 = minutes
        call   cvt2asc         ;get minutes and convert
        mov    cs:[bx+6],al    ;tens of minutes
        mov    cs:[bx+8],ah    ;ones
        dec    dx              ;base+2 = seconds
        call   cvt2asc         ;get seconds and convert
        mov    cs:[bx+12],al   ;tens of seconds
        mov    cs:[bx+14],ah   ;ones
        pop    dx
        pop    cx
        pop    bx
        pop    ax
        ret                    ;return to caller
display endp

;clock Driver's replacement code for interrupting 1Ch

clkint  proc   near     ;new timer interrupt code
        push   ax              ;save registers used
        push   cx
        push   di
        push   si
        push   es
        pushf                  ;must push flags
        call   cs:old1c        ;call old timer int
        mov    cx,cs:refresh   ;get refresh counter
        inc    cx              ;increment
        cmp    cx,18           ;18th time?
        jb     notime          ;no need to recalc time
        call   display         ;yes we do
        mov    cx,0            ;reset counter
notime: mov    cs:refresh,cx   ;store it
        mov    dx,cs:scn_port  ;screen status port
        mov    di,cs:scn+pos   ;screen display position
        lea    si,cs:time      ;time string source
        mov    cx,cs:scn_seg   ;screen segment
        mov    es,cx           ; in es
        mov    cx,10           ; mov 10 bytes
        cli                    ; clear interrupts
hlow:                          ;wait for horizontal scan
        in     al,dx           ;get video port status
        test   al,1            ;wait for low = 1
        jnz    hlow            ;back
        mov    ah,cs:[si]      ;get byte to be displayed
hhigh:  
        in     al,dx           ;status must go hi after lo
        test   al,1            ; before a screen write
        jz     hhigh           ;wait til high = 0
        mov    es:[di],ah      ;1 byte at any one time
        inc    di              ;increment screen position
        inc    si              ;increment source position
        loop   hlow            ;loop thru all bytes
        sti                    ;restore interrupts
        pop    es              ;restore all saved registers
        pop    si
        pop    di
        pop    cx
        pop    ax
        iret                   ;interrupt return
clkint  endp

;*******************************************************************************
;* DOS COMMAND PROCESSING
;*******************************************************************************
;command 0      Initialization
Initialization:

        call    initial                   ;display message
;determine wheter we found a clock chip
        cmp     cs:clock_port,0           ;is chip base = 0?
        jne     init1                     ;no - there is a chip
;no chip found - we must abort loading this driver 
        mov     ax,0                      ;set address to beginning
        jmp     init2                     ;store break offset
init1:  lea     ax,initial                ;set Break Addr. at initial
init2:  mov     es:[bx].rh0_brk_ofs,ax    ;store offset address
        mov     es:[bx].rh0_brk_seg,cs    ;store segment address
        jmp     done                      ;set done status and exit

;command 1      Media_Check
Media_Check:

        jmp     done                      ;set done bit and exit

;command 2      Get_BPB
Get_BPB:

        jmp     done                      ;set done bit and exit

;command 3      IOCTL_Input
IOCTL_Input:

        jmp     unknown                   ;set error bit/code and exit

;command 4      Input    Read clock chip and return to DOS
Input:

;Read and convert clock chip date and time to DOS date format
        mov     dx,es:[bx].rh4_buf_ofs    ;get dos date data area
        mov     ax,es:[bx].rh4_buf_seg    ;
        mov     es,ax                     ;set up es
        mov     bx,dx                     ;set up bx
;ES:BX points to the DOS date buffer
        push    es                        ;save segment for later
        push    bx                        ;save offset for later
;first read the clock chip for time
        mov     dx,cs:clock_port          ;get the clock base address
        inc     dx                        ;base+1
        in      al,dx                     ;get hundreths 
        call    bcd2hex                   ;convert data 
        mov     es:[bx].dos_hun,al        ;store hundreths 
        inc     dx                        ;base+2  
        in      al,dx                     ;get seconds
        call    bcd2hex                   ;convert data 
        mov     es:[bx].dos_sec,al        ;store seconds 
        inc     dx                        ;base+3  
        in      al,dx                     ;get minutes 
        call    bcd2hex                   ;convert data 
        mov     es:[bx].dos_min,al        ;store minutes 
        inc     dx                        ;base+4  
        in      al,dx                     ;get hours 
        call    bcd2hex                   ;convert data 
        mov     es:[bx].dos_hr,al         ;store hours 

;now convert chip date (BCD format) to DOS date format (hex)

;first check to see if month (and therefore year) has changed
;by comparing the months counter against the month RAM location
incheck:
        mov     dx,cs:clock_port          ;get base clock address         
        add     dx,7                      ;base+7        
        in      al,dx                     ;get chip's month counter
        call    bcd2hex                   ;convert to hex
        mov     bl,al                     ;save in bl
        add     dx,2                      ;base+9
        in      al,dx                     ;get RAM version of month
        call    bcd2hex                   ;convert to hex
        cmp     al,bl                     ;is RAM & counter same?
        jg      newyear                   ;last month > current (12>1)
        jl      updatemonth               ;last month < current
        jmp     prev_days                 ;same month
;December rolled over to January - update the Year count in RAM
newyear:
        inc     dx                        ;base+10
        in      al,dx                     ;get year (stored in RAM)
        inc     al                        ;add 1 year
        out     dx,al                     ;store in RAM year
        dec     dx                        ;make it base+9
;now update month in RAM
updatemonth:
        mov     al,bl                     ;set current month
        call    hex2bcd                   ;convert for clock chip
        out     dx,al                     ;update month RAM

;determine days in previous years
prev_days:
        inc     dx                        ;base+10 (RAM)
        in      al,dx                     ;get years since 1980
        mov     ah,0                      ;set up for multiply
        push    ax                        ;save for leap year processing
        mov     bx,365                    ;days per year
        mul     bx                        ;times years - AX has days
        xchg    bx,ax                     ;save days in BX
        mov     cl,4                      ;leap divisor
        pop     ax                        ;get year count again
        div     cl                        ;divide for leap years elapsed
        mov     cl,ah                     ;save leap year indicator
;BX has total days and cl has leap year indicator
        mov     ah,0                      ;set up for add
        add     bx,ax                     ;add leap days to total

;we have days since 1/1/80 for all previous years including
; the extra days in leap years past
curr_days:
        push    bx                        ;save total days past
        mov     dx,cs:clock_port          ;get base clock chip address
        add     dx,7                      ;base+7
        in      al,dx                     ;get month counter
        call    bcd2hex                   ;convert to hex
        mov     ah,0                      ;set up for index
        push    cs                        ;days per month table
        pop     es                        ; addressed by ES
        lea     di,cs:table               ; and DI
        mov     cx,0                      ;clear current year day count
        xchg    ax,cx                     ;month loop count in cx
        push    cx                        ;save for leap year check 
        mov     bh,0                      ;clear hi-order
cvt2days:
        mov     bl,es:[di]                ;days in this month
        inc     di                        ;increment for next month
        add     ax,bx                     ;add to total days
        loop    cvt2days                  ;until month count exhausted 
        pop     cx                        ;restore months 
        pop     bx                        ;total days past 
        add     ax,bx                     ;add to days in current year
        cmp     cl,3                      ;past March?
        jl      leapyr                    ;no
        inc     ax                        ;yes - add 1 for 2/29
leapyr: pop     bx                        ;restore DOS date offset
        mov     es:[bx].dos_day,ax        ;restore DOS date segment 
        mov     ax,0                      ;status ok
        mov     bx,6                      ;count of 6
        jmp     load_status               ;restore es:bx exit
        
;command 5      ND_Input
ND_Input:
       
        jmp     busy                      ;set busy bit and exit

;command 6      Input_Status     
Input_Status:
       
        jmp     done                      ;set done bit and exit

;command 7      Input_Flush     
Input_Flush:
       
        jmp     done                      ;set done bit and exit

;command 8      Output  Set the Clock Chip Time and Date     
Output:
       
;Convert the date in DOS date format to clock chip format
; for writing to the clock chip

;let ES:BX point to begginning of the DOS date
        mov     si,es:[bx].rh8_buf_ofs    ;get data offset
        mov     ax,es:[bx].rh8_buf_seg    ;get data segment 
        mov     ds,ax                     ;save offset
        push    si                        ;save offset
        push    ds                        ;save segment
        push    cs                        ;
        pop     es                        ;ES points to here
        lea     di,cs:dosdays             ;destination address
        mov     cx,2                      ;move count = 2
        cld                               ;direction is forward
        rep     movsb                     ; from DOS to us
        push    cs                        ;restore DS
        pop     ds                        ; by using CS
;update clock chip with time from DOS date data
outchip:
        pop     es                        ;restore DOS date segment
        pop     bx                        ;restore DOS date offset
        mov     dx,cs:clock_port          ;get clock port 
        inc     dx                        ;base+1
        mov     al,es:[bx].dos_hun        ;get hundredths
        call    hex2bcd                   ;convert for clock use
        out     dx,al                     ;send to clock chip
        inc     dx                        ;base+2
        mov     al,es:[bx].dos_sec        ;get seconds
        call    hex2bcd                   ;convert for clock use 
        out     dx,al                     ;send to clock chip
        inc     dx                        ;base+3
        mov     al,es:[bx].dos_min        ;get minutes 
        call    hex2bcd                   ;convert for clock use  
        out     dx,al                     ;send to clock chip
        inc     dx                        ;base+4
        mov     al,es:[bx].dos_hr         ;get hours 
        call    hex2bcd                   ;convert for clock use    
        out     dx,al                     ;send to clock chip

;chip loaded with time - now calc chip date from DOS date
out_years:
        mov     ax,cs:dosdays             ;get days since 1/1/80
        cmp     ax,0                      ;date not set?
        je      out8                      ;skip everything
        mov     bx,0                      ;BX = year count
out1:   cmp     ax,365                    ;day count within a year?
        jle     out2                      ;yes
        sub     ax,365                    ;no - substract 365
        inc     bx                        ;increment year count
        jmp     out1                      ;continue until w/i 1 yr

;BX has years since 1980 - now adjust for leap years
out2:   push    ax                        ;save leftover days
        mov     ax,bx                     ;AX now has years     
        mov     cl,4                      ;divisor for leap years
        div     cl                        ;al=leaps, ah=reminder
        mov     cl,ah                     ;remainder=0 is leap itself
        mov     ah,0                      ;set up for subtract
        inc     ax                        ;add 1 to leap year count
        mov     dx,ax                     ;DX has 1 day/leap yr passed
        pop     ax                        ;restore days remaining
        sub     ax,dx                     ;subtract 1 day for each leap yr
        cmp     ax,0                      ;are we negative?
        jg      out3                      ;no - we are ok
        add     ax,365                    ;add back 365 days
        dec     bx                        ;subtract 1 year
out3:   push    bx                        ;save year count        
        cmp     cl,0                      ;leap year if 0
        jne     out5                      ;not a leap year 
        cmp     ax,59                     ;Feb 29?
        je      out4                      ;yes - set and exit
        jg      out5                      ;past Feb 29
        inc     ax                        ;before - reverse subtraction
        jmp     out5                      ;
out4:   mov     cx,2                      ;Feb 
        mov     ax,29                     ; 29
        jmp     out7                      ;exit
;AX has days left in current year - now find month and day
out5:   mov     cx,1                      ;month count
        lea     di,cs:table               ;days per month
        mov     bh,0                      ;clear hi-order
out6:   mov     bl,es:[di]                ;get days in each month
        inc     di                        ;increment to next month
        cmp     ax,bx                     ;less than last day?
        jle     out7                      ;yes (in current month)
        sub     ax,bx                     ;no subtract days in month
        inc     cx                        ;increment month count
        jmp     out6                      ;continue until month found
;AX has days, CX has months - now get years since 1980
out7:   pop     bx                        ;restore year count
        jmp     out9                      ;go load chip
;no date set (special case)
out8:   mov     bx,0                      ;1980
        mov     cx,1                      ;Jan
        mov     ax,1                      ; 1st
;BX = years since 1980, CX = month, AX = days - now load clock chip
out9:   mov     dx,cs:clock_port          ;get chip base address
        add     dx,6                      ;base+6
        push    cx                        ;Hex2bcd destroys cx
        call    hex2bcd                   ;convert for chip use
        out     dx,al                     ;set days counter
        inc     dx                        ;base+7
        pop     ax                        ;restore month count
        call    hex2bcd                   ;convert for chip use
        out     dx,al                     ;set months counter
        add     dx,2                      ;base+9
        out     dx,al                     ;set months RAM
        inc     dx                        ;base+10
        xchg    al,bl                     ;move years to al
        out     dx,al                     ;set years since 1980 RAM
        mov     ax,0                      ;status ok
        mov     cx,6                      ;count of 6
        jmp     load_status               ;set status word & exit

;command 9      Output_Verify 
Output_Verify:
       
        jmp     output                    ;same as output

;command 10     Output_Status 
Output_Status:
       
        jmp     done                      ;set done bit and exit

;command 11     Output_Flush 
Output_Flush:
       
        jmp     done                      ;set done bit and exit

;command 12     IOCTL_Out 
IOCTL_Out:
       
        jmp     unknown                   ;set done bit and exit  
  
;command 13     Open 
Open:
       
        jmp     done                      ;set done bit and exit  

;command 14     Close 
Close:
       
        jmp     done                      ;set done bit and exit  

;command 15     Removable 
Removable:
       
        jmp     unknown                   ;set error bit/code and exit  

;command 16     Output Til Busy 
Output_Busy:
       
        jmp     unknown                    ;set done bit and exit  

;*******************************************************************************
;* ERROR EXIT
;*******************************************************************************
unknown:
        or      es:[bx].rh_status,8003h    ;set error bit and error code
        jmp     done                       ;set done and exit

;*******************************************************************************
;* COMMON EXIT
;*******************************************************************************
load_status:
        mov     cx,cs:rh_seg               ;restore request header
        mov     es,cx                      ; segment to es
        mov     cx,cs:rh_ofs               ;restore offset also
        xchg    bx,cx                      ;switch them
        mov     es:[bx].rh_status,ax       ;return status
        mov     es:[bx].rh8_count,cx       ;return output count
        jmp     done                       ;set done bit and exit

busy:   or      es:[bx].rh_status,0200h    ;set busy bit  

done:   or      es:[bx].rh_status,0100h    ;set done 

        pop     si                         ;restore all registers
        pop     di
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        pop     es
        pop     ds
        ret                                ;return to DOS

;*******************************************************************************
;* END OF PROGRAM
;*******************************************************************************
;this procedure is called from the Initialization command and 
;is executed only once. We tell DOS the next available
;memory location (Break Address) is here. This allows DOS to over
;write this code; we save space.

initial proc	near                      ;display message on console
        lea     dx,cs:msg1                ;part 1 of message
        mov     ah,9                      ;display of console
        int     21h                       ;DOS call
;First find clock chip base address
        lea     si,cs:clock_table         ;get address of table
        mov     cx,3                      ;three addresses
find1:  mov     dx,cs:[si]                ;get 1st address
        add     dx,2                      ;base+2 = seconds
        in      al,dx                     ;get seconds
        test    al,80h                    ;high order bit set?
        jz      find2                     ;no - not empty port
        add     si,2                      ;next address
        loop    find1                     ;search thru clock table
;no port found - don't continue with setup
        lea     dx,cs:msg4                ;no port found
        mov     ah,9                      ;display on console
        int     21h                       ;DOS call
        ret
;Clock chip port found
find2:  mov     dx,3                      ;convert back to port #
        sub     dx,cx                     ;port position
        shl     dx,1                      ;double it
        lea     di,cs:clock_table         ;address of chip table
        add     di,dx                     ;word index
        mov     dx,cs:[di]                ;get port
        mov     cs:clock_port,dx          ;save it
        lea     di,cs:msg2a               ;convert to ASCII
        call    hex2asc                   ;for later display
;Determine type of Video Display adapter in system
        mov     ah,0fh                    ;get video mode
        int     10h                       ;Video BIOS call
        cmp     al,7                      ;mono?
        jne     calc                      ;no - assume color
        mov     cs:mode,1                 ;mono = 1
        mov     cs:scn_port,3bah          ;mono video port
        mov     cs:scn_seg,0b000h         ;mono screen address
;Calculate time string
calc:   call    display                   ;setup initial time
        cli                               ;clear interrupts
        assume  es:timer                  ;new directive
        mov     ax,timer                  ;get segment addr
        mov     es,ax                     ;set ES
        mov     ax,es:timer_ofs           ;get old timer offset
        mov     cs:old1c_ofs,ax           ;save it
        mov     ax,es:timer_seg           ;get old timer segment
        mov     cs:old1c_seg,ax           ;save it
        lea     ax,clkint                 ;get new offset
        mov     es:timer_ofs,ax           ;set new offset
        mov     es:timer_seg,cs           ;also segment 
        assume  es:cseg                   ;restore directive
        sti                               ;restore interrupts
        lea     dx,cs:msg2                ;part 2 of message
        mov     ah,9                      ;display on console
        int     21h                       ;DOS call
        cmp     cs:mode,1                 ;mono?
        jne     exit2                     ;no
        lea     dx,cs:msg3a               ;yes
        jmp     exit3                     ;go print mono message
exit2:  lea     dx,cs:msg3b               ;color
exit3:  mov     ah,9                      ;display on console
        int     21h                       ;DOS call
        mov     bx,rh_ofs                 ;restore BX
        mov     ax,cs:rh_seg              ;restore segment
        mov     es,ax                     ; to ES
        ret
initial endp
msg1    db      'The Waite Group Clock Device Driver´,0dh,0ah,'$´
msg2    db      'using device address ´
msg2a   db      '0000H',0dh,0ah,'$´
msg3a   db      ' with monochrome adapter ´,0dh,0ah,'$´
msg3b   db      ' with color adapter',0dh,0ah,'$´
msg4    db      ' No Clock Found - Driver Aborted´,0dh,0ah,'$´

clock_table     label    byte    ;table of possible chip addresses 
                dw       0240h   ;
                dw       02c0h   ;
                dw       0340h   ;

hex2asc proc

;requires
;               dx = binary number
;               di = address of ASCII string
;uses:
;               ax - for character conversion 
;               cx - loop control
;returns:
;               nothing
	push    cx       ;save cx
        push    ax       ;save ax
        mov     cx,4     ;number of hex digits
h1:     push    cx       ;save cx inside this loop
        mov     cl,4     ;shift count (bits/hex digit)
        rol     dx,cl    ;rotate left 1 hex digit
        mov     al,dl    ;move hex digit to al
        and     al,0fh   ;mask off desired hex digit
        cmp     al,0ah   ;is it above 9h?
        jge     h2       ;yes
        add     al,30h   ;numeric hex digit
        jmp     h3       ;skip
h2:     add     al,37h   ;alpha hex digit
h3:     mov     cs:[di],al       ;store hex digit in string
        inc     di       ;next string address
        pop     cx       ;get saved loop count
        loop    h1       ;loop start
        pop     ax       ;restore ax
        pop     cx       ;restore cx
        ret
hex2asc endp

clock   endp             ;end of clock procedure
cseg    ends             ;end of cseg segment
        end     begin    ;end of program

;that's all folks

