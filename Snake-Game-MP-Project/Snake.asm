INCLUDE Irvine32.inc

.DATA
;---------------------------------------------snakeData---------------------------------------------------
segO struct
    ; the main object that will be used to form the snake
    ; could be used in creating other parts of the game like food seg or wall seg.

    col byte ?  ; holds the column of the segment
    row byte ?  ; holds the row of the segment
segO ends


segArray segO      {47,16},{47,15},{47,14},{47,13},1916 dup({0,0})  
                    ; the array that will hold all the segments forming the snake
                    ; with max 1920 word (the full FrameBuffer)
                    ; with the first four segmants defined to the initial snake state
                    ; the arr starts with the tail since i add only heads 


tmpHead word ?      ; will hold the new head values
tailIndex dd 0      ; get the index of the tail from the snake
headIndex dd 6      ; get the index of the head from the snake

noOfSegments dw 4

cRow BYTE 13            ; The central Row
cColumn BYTE 47         ; The central Column
tmpCounter word 1       ; a Counter that will be used to assign the segmants,
                        ; with there nums for the paint function
;------------------------------------------------------------------------------------------------------------


a WORD 1920 DUP (0)

menus BYTE ".----------------.| snake the game |'----------------'", 0Dh, 0Ah, " 1-start game",0dh,0ah,"2-speed",0Dh,0Ah,
          "3-level",0dh,0ah,"4- Exit",0Dh,0Ah, 0

levels  BYTE "1. None", 0Dh, 0Ah, "2. Box", 0Dh, 0Ah, "3. camera frame", 0Dh, 0Ah, 0
speeds BYTE "1", 0Dh, 0Ah, "2", 0Dh, 0Ah, "3",
             0Dh, 0Ah, "4", 0Dh, 0Ah, 0
hitS    BYTE "Game Over!", 0
highS    BYTE "Highest Score: 0",0
scoreS  BYTE "Score: 0", 0
gamespeed DWORD   60 ;

highestScore DWORD 0

eTail   BYTE    1d  
search  WORD    0d 
eGame   BYTE    0d  
cScore  DWORD   0d  
d       BYTE    'w' 
newD    BYTE    'w' 
foodR   BYTE    0           
foodC   BYTE    0  

myHandle DWORD ?
numInp   DWORD ?    
temp BYTE 16 DUP(?) 
bRead    DWORD ? 



.CODE

main PROC                       ; used to show menus and setup the game for the user and then start it

 menu:
    CALL Randomize              
    CALL Clrscr                 
    MOV EDX, OFFSET menus      
    CALL WriteString            ;write menu string

    wait1:                      ;choose from menu
    CALL ReadChar

    CMP AL, '1'                 
    JE startG

    CMP AL, '2'                
    JE speed

    CMP AL, '3'               
    JE level

    CMP AL, '4'                 
    JNE wait1                   
                                
    EXIT

    level:                    ;choose level
    CALL Clrscr                 
    MOV EDX, OFFSET levels   
    CALL WriteString            

    wait2:                      
    CALL ReadChar

    CMP AL, '1'                
    JE level1

    CMP AL, '2'                
    JE level2

    CMP AL, '3'                 
    JE level3

    JMP wait2                   

    level1:  
    CALL clearMem              
    MOV AL, 1
    CALL GenLevel 
    JMP menu

    level2: 
    CALL clearMem    
    MOV AL, 2 
    CALL GenLevel    
    JMP menu

    level3: 
    CALL clearMem   
    MOV AL, 3    
    CALL GenLevel   
    JMP menu

    speed:                 ; choose speed  
    CALL Clrscr                 
    MOV EDX, OFFSET speedS      
    CALL WriteString            

    wait3:                      
    CALL ReadChar

    CMP AL, '1'                 
    JE speed1

    CMP AL, '2'                 
    JE speed2

    CMP AL, '3'                 
    JE speed3

    CMP AL, '4'                 
    JE speed4
    JMP wait3

    speed1:                     
    MOV gamespeed, 150
    JMP menu

    speed2:                     
    MOV gamespeed, 100
    JMP menu

    speed3:
    MOV gamespeed, 60             
    JMP menu

    speed4:
    MOV gamespeed, 35             
    JMP menu                    

    startG:                     
                                
    MOV EAX, 0                 
    MOV EDX, 0 
    CALL Clrscr           
    CALL initSnake            
    CALL Paint                    
    CALL createFood              
    CALL startGame        
    MOV EAX, white + (black * 16)
    CALL SetTextColor          
    JMP menu                  







main ENDP

;--------------------------------------------------------------snakeLogic----------------------------------------
initSnake PROC USES EBX EDX ECX

; This procedure initializes the snake to the default position
; in the center of the screen

    MOV cx , 4
    
    linit:
       MOV DH, cRow
       INC cRow
       MOV DL, cColumn
       MOV Bx, tmpCounter
       INC tmpCounter
       CALL saveIndex
       loop linit

       
    RET

initSnake ENDP

getHead PROC USES EAX EDX

    

    LEA EAX, segArray
    ADD EAX, headIndex
    mov dx, [eax].segO
    
    mov tmpHead,dx
    
    RET
getHead ENDP

setHead PROC USES EAX EDX

    
    LEA EAX, segArray
    mov dx, tmpHead
    add headIndex, 2
    add EAX, headIndex
    mov [eax], dx
    INC noOfSegments
    CALL GotoXY            
    MOV EAX, blue + (white * 16)
    CALL setTextColor       
    MOV AL, ' '             
    CALL WriteChar
    mov bx, noOfSegments
    call saveIndex
    MOV DH, 23              
    MOV DL, 79
    CALL GotoXY
    

    RET
setHead ENDP


MoveSnake PROC USES EDX EAX EBX
    
    
    cmp eTail, 1
    jne nEtail
    eraseTail:
        lea EAX, segArray
        add EAX, tailIndex
        mov dx, [eax]
        CALL GotoXY            
        MOV EAX, white + ( black * 16)
        CALL setTextColor       
        MOV AL, ' '             
        CALL WriteChar
        mov bx, 0
        call saveIndex
        MOV DH, 23              
        MOV DL, 79
        CALL GotoXY
        ADD tailIndex, 2
    
    nEtail:
    mov eTail, 1
    CALL getHead   ; get the row and column of the current head and mov them to tmpHead
    MOV DX, tmpHead

    .IF d == 'w'
        
        CMP newD, 'w'
        JE sameD
        CMP newD, 's'
        JE sameD

        .IF newD == 'd'
            ADD DL, 1
        .ELSEIF newD == 'a'
            SUB DL , 1
        .ENDIF

        sameD:
            SUB DH,1

    .ELSEIF d == 's'
        
        CMP newD, 'w'
        JE sameD2
        CMP newD, 's'
        JE sameD2

        .IF newD == 'd'
            ADD DL, 1
        .ELSEIF newD == 'a'
            SUB DL, 1
        .ENDIF

        sameD2:
            ADD DH,1

    .ELSEIF d == 'd'
        
        CMP newD, 'a'
        JE sameD3
        CMP newD, 'd'
        JE sameD3

        .IF newD == 'w'
            SUB DH, 1
        .ELSEIF newD == 's'
            ADD DH,1
        .ENDIF
        sameD3:
            ADD DL,1

    .ELSEIF d == 'a'
        
        CMP newD, 'a'
        JE sameD4
        CMP newD, 'd'
        JE sameD4

        .IF newD == 'w'
            SUB DH,1
        .ELSEIF newD == 's'
            ADD DH,1
        .ENDIF
        sameD4:
            SUB DL,1
    .ENDIF


    ;wrap
    ;---------------------------------------------------
    CMP DH, 24              ; Check if new index is getting off screen
    JNE next
        MOV DH, 1           ; Wrap index around screen

    next:
    CMP DL, 80              ; Chekc if new index is getting off screen
    JNE next2
        MOV DL, 1           ; Wrap index around screen

    next2:
    CMP DH, 0               ; Check if new index is getting off sreen
    JGE next3
        MOV DH, 23          ; Wrap index around screen

    next3:
    CMP DL, 0               ; Check if new index is getting off screen
    JGE last
        MOV DL, 79          ; Wrap index around screen
    last:
        NOP
    ;---------------------------------------------------
    ;checkCollision
    ;---------------------------------------------------

    call accessIndex

    cmp BX, 0
    jne hit

    jmp nhit
    hit: 
        MOV EAX, 4000           ; Set delay time to 4000ms
        MOV DH, 24              ; Move cursor to new location, to write game over
        MOV DL, 40              ; message
        CALL GotoXY
        MOV EDX, OFFSET hitS
        CALL WriteString
        MOV EAX, cScore
        .IF EAX > highestScore
            MOV DH, 24
            MOV DL, 25
            CALL WriteDec
        .ENDIF

        CALL Delay              ; Call delay to pause game for 4 seconds
        MOV eGame, 1            ; Set end game flag
        RET
    nhit:
        nop
    ;---------------------------------------------------
    ;checkFood
    ;---------------------------------------------------
    
    .IF foodR != DH
        JMP ex
    .ELSEIF foodC != DL
        JMP ex
    .ENDIF
    ;---------------------------------------------------
     push edx
     MOV DH, 24              ; Move cursor to new location, to update score
     MOV DL, 7
     CALL GotoXY       
     MOV EAX, cScore         ; Move score to EAX and increment it
     INC EAX
     CALL WriteDec
     MOV cScore, EAX         ; Copy updated score value back into memory
     pop edx
     ;-
     call createFood
     mov eTail, 0
     ex:
     ;---------------------------------------------------

    MOV tmpHead, DX
    CALL setHead

    RET
        
MoveSnake ENDP

;-----------------------------------------------------------------------------------------------------------------------------------------

createFood PROC USES EAX EBX EDX   ;generates food for the snake


    check:   
    MOV EAX, 80                 
    CALL RandomRange            
    MOV DL, AL

    MOV EAX, 24                 
    CALL RandomRange            
    MOV DH, AL


    CALL accessIndex              

    CMP BX, 0                   
    JNE check                    

    MOV foodR, DH                 
    MOV foodC, DL                 

    MOV EAX, white + (Magenta * 16)
    CALL setTextColor
    CALL GotoXY                 
    MOV AL, ' '                 
    CALL WriteChar

    RET

createFood ENDP

paint PROC USES EAX EBX EDX ESI

        MOV EAX , blue + (white *16)  ; set background color to white and foreground to blue
        CALL SetTextColor

        MOV DH, 0  ; set row num to zero

        loop1:
            CMP DH, 24 ; loop over index of rows
            JGE endloop1
            MOV DL, 0  ; set column num to zero
      
                loop2:
                        CMP DL, 80  ; loop over index of columns
                        JGE endloop2
           
                        call GOTOXY ; move cursor to current position
           
                        MOV BL, DH  ; store row value in bl
                        MOV AL, 80  
                        MUL BL      
                        PUSH DX    ; store value of dx
                        MOV DH, 0  ; clear upper bites of dx
                        ADD AX, DX ; store index of pixel in ax
                        POP DX     ; restore value of dx
       
                        MOV ESI, 0 ; clear index reg
                        MOV SI, AX ; si has pixel address
                        SHL SI, 1  ; shift left by 1 word to fit 32 bit 
                        MOV BX, a[SI] ; value of pixel 
                                        ; now bx has pixel value to start printing we will compare it
                                   
                        CMP BX, 0  
                        JE noPrint    ; empty pixel
                                   
                        CMP BX, 0FFFFh ; wall pixel
                        JE printHurdle ; jump to printing wall segment
                                   
                        MOV AL, ' ' ; if not empty pixel and not wall pixel so it's part
                        CALL WriteChar ; of snake pixel so print space
                        JMP noPrint 
                                   
                        printHurdle:         ; to print walls
                        MOV EAX, blue + (yellow *16) ; set wall color to yellow 
                        call SetTextColor
                        MOV AL, ' '                   ; print white space
                        CALL WriteChar
                                   
                        MOV EAX, blue + (white * 16)  ; return text color to blue 
                        CALL SetTextColor             ; foreground and white background
                                   
                        noPrint:
                        INC DL                        ; increment column num
                        JMP loop2                     ; get back to loop over columns
                                  
                    endloop2:
                        INC DH                        ; increment row num
                        JMP loop1                     ; get back to loop over rows
                               
            endLoop1:
                    RET 

paint ENDP 


    genLevel PROC 
    
     CMP AL, 1               ; Check if level choice is without obstacles
    JNE nextL               ; If not, jump to next level selection

    RET                     ; Exit procedure, don't generate any obstacles.

    nextL:                  ; Check if level choic is box level
    CMP AL, 2
    JNE nextL2              ; If not, jump to next level selection

    MOV DH, 0               ; first row
    MOV BX, 0FFFFh          ; Set data to be written to indicate to print or not

    rLoop:                  ; Loop for generating vertical line
        CMP DH, 24          ; Check if bottom of screen reached          
        JE endRLoop         ; Break if bottom of screen is reched

        MOV DL, 0           ; first column from left
        CALL saveIndex      ; Write value stored in BX to framebuffer
        MOV DL, 79          ; last column
        CALL saveIndex      ; Write value stored in BX to framebuffer
        INC DH              ; Increment row value
        JMP rLoop           ; Continue loop
    endRLoop:

    MOV DL, 0               ; Set column index to 0

    cLoop:                  ; Loop for generating horizontal lines
        CMP DL, 80          ; Check if end of screen from right is reached
        JE endCLoop         ; Break if right side of screen is reached

        MOV DH, 0           ; first row
        CALL saveIndex      ; Write value stored in BX to framebuffer
        MOV DH, 23          ; last row
        CALL saveIndex      ; Write value stored in BX to framebuffer
        INC DL              ; Increment column value
        JMP cLoop           ; Continue loop

        endCLoop:

    RET
   

    nextL2:                 ; Section for generating camera frame level

        MOV newD, 'd'       ; Set the default direction to down, as not to run
        MOV DH, 0           ; immediately into a wall
        MOV DL, 0           ; Set row and column numbers to 0 and 0
        MOV BX, 0FFFFh      ; set value of bx to pass to print proc

        cLoop2:             ; Loop for printing the first horizontal line at top left of screen (row 0 col 0)                         
            CMP DL, 8      ; print first 7  horizontal pixels 
            JE endCLoop2

            CALL saveIndex  ; Write  value to framebuffer                          first row at left
            INC DL          ; Increment column number
            JMP cLoop2      ; loop for first 7 pixels

        endCloop2:          ; Prepare for printing the bottom left row
        MOV DH, 23           ; at bottom of screen
        MOV DL, 0          ; start from first column

        cLoop3:             ; Loop for painting bottom left row                      
            CMP DL, 8      ; first 7 horizontal pixels at last row
            JE endCLoop3

            CALL saveIndex  ; Write value to framebuffer
            INC DL          ; Increment column number
            JMP cLoop3      ; loop for first 7 pixels at last row

         endCloop3:          ; Prepare for printing the top right row
        MOV DH, 0           ; Set to first row
        MOV DL, 71          ; to print on last 7 pixels of first row

        cLoop4:             ; Loop for painting top right row
            CMP DL, 79      ; Check if right side of screen was reached                     
            JE endCLoop4

            CALL saveIndex  ; Write  value to framebuffer
            INC DL          ; Increment column number
            JMP cLoop4      ; loop over last 7 pixels of first row

            endCloop4:          ; Prepare for bottom right row
        MOV DH, 23           ; Start from last row
        MOV DL, 71          ; last 7 pixels of last row will be painted

        cLoop5:             ; Loop for painting last row at right                                        
            CMP DL, 79      ; Check if right side of screen was reached
            JE endCLoop5

            CALL saveIndex  ; Write  value to framebuffer
            INC DL          ; Increment column number
            JMP cLoop5      ; loop over last 7 pixels of last row

            endCloop5:          ; Prepare for vertical lines painting
        MOV DH, 0           ; Start from first row
        MOV DL, 0           ; and first column
        
       

        rLoop2:             ; Loop for painting top left vertical line
            CMP DH, 7      ; first 7 vertical pixels
            JE endRLoop2

            CALL saveIndex  ; Write value to framebuffer
            INC DH          ; Increment row number
            JMP rLoop2      ; Continue until looping over first 7 vertical pixels

        endRLoop2:          ; prepare to paint bottom left 7 pixels
            MOV DH, 15      ; last 7 vertical pixels
            MOV DL, 0       ; first column

             rLoop3:             ; Loop for patining a last 7 vertical pixels at left
            CMP DH, 23      ; if bottom of screen is reached
            JE endRLoop3

            CALL saveIndex  ; Write value to framebuffer
            INC DH          ; Increment row number
            JMP rLoop3      ; Continue until bottom of screen is reached

        endRLoop3:          ; prepare to paint top right 7 pixels
            MOV DH, 0       ; first row
            MOV DL, 79      ; last column

            rLoop4:         ; Loop for patining a first 7 vertical pixels at right
            CMP DH, 7       ; first 7 vertical pixels  
            JE endRLoop4

            CALL saveIndex  ; Write value to framebuffer
            INC DH          ; Increment row number
            JMP rLoop4      ; Continue until painting first 7 vertical right pixels

        endRLoop4:          ; prepare to paint bottom right 7 pixels
            MOV DH,15       ; last 7 vertical pixel
            MOV DL, 79      ; last row

            rLoop5:         ; Loop for patining last 7 vertical pixels
            CMP DH, 24      ; check if bottom of screen is reached
            JE endRLoop5

            CALL saveIndex  ; Write value to framebuffer
            INC DH          ; Increment row number
            JMP rLoop5      ; Continue until bottom of screen is reached

        endRLoop5:          ; Return from procedure after painting 
            RET 
    
    genLevel ENDP

clearMem PROC                
        ;used to restore defaults after losing
        MOV BX, 0
        MOV DH, 0
        outerLoop:
            CMP DH, 24
            JE ExitOut
            MOV DL, 0
            innerLoop:
                CMP DL, 80
                JE ExitIn
                call saveIndex         ;TO DO
                INC DL
                JMP innerLoop
            ExitIn:
                INC DH
                JMP outerLoop
        ExitOut:

        
        MOV cRow, 13            ; return the index to the initial positon
        MOV cColumn, 47         ; return the index to the initial positon
        MOV tmpCounter, 1       ; return the counter to the initial value
        MOV headIndex, 6        ; return the index to the initial positon
        MOV tailIndex, 0        ; return the index to the initial positon
        
        MOV eGame, 0            
        MOV eTail, 1            
        MOV d, 'w'              
        MOV newD, 'w'           
        MOV cScore, 0 
        RET
clearMem ENDP
          
CalcIndex PROC USES EAX EDX             
    ; This procedure used to calculat the value of the index, 
    ; that i can access the value of our array or save the value to our array by it.
    ; The value returned through SI register.         
    
        MOV BL, DH      
        MOV AL, 80      
        MUL BL          
        PUSH DX         
        MOV DH, 0       
        ADD AX, DX      
        POP DX         
        MOV ESI, 0      
        MOV SI, AX      
   
        SHL SI, 1      
   
        RET
 CalcIndex ENDP
 
 saveIndex PROC USES EAX ESI EDX                  ; This procedure saved value of BX in our array of index, that get value of the index 
 ; by the value that returned from CalcIndex procedure and saved in the register SI. 

        PUSH EBX        
        CALL CalcIndex
        POP EBX         
        MOV a[SI], BX   
        RET
 saveIndex ENDP
 
 accessIndex PROC USES EAX ESI EDX                ; This procedure accesses the value of the array of index, that get value of the index 
 ; by the value that returned from CalcIndex procedure and saved in the register SI. 
 ; that saved in SI register, and returend the value of it in the register BBX

        CALL CalcIndex
        MOV BX, a[SI]   
        RET

 accessIndex ENDP
 
 startGame PROC USES EAX EBX ECX EDX
    MOV EAX, white + (black * 16)
    CALL setTextColor

    MOV DH, 24
    MOV DL, 0
    CALL GotoXY

    MOV EDX, OFFSET scoreS
    CALL WriteString

    MOV DH, 24
    MOV DL, 10
    CALL GotoXY

    MOV EDX, OFFSET highS
    CALL WriteString

    INVOKE getStdHandle, STD_INPUT_HANDLE
    MOV myHandle, EAX
    MOV ECX, 10

    INVOKE ReadConsoleInput, myHandle, ADDR temp, 1, ADDR bRead
    INVOKE ReadConsoleInput, myHandle, ADDR temp, 1, ADDR bRead

    gameLoop:
        INVOKE GetNumberOfConsoleInputEvents, myHandle, ADDR numInp
        MOV ECX, numInp

        ;check if the buffer is empty and continue
        CMP ECX, 0                          
        JE finished

        ;read one event from input buffer and save it at temp
        INVOKE ReadConsoleInput, myHandle, ADDR temp, 1, ADDR bRead

        ;check if input is KEY_EVENT

        MOV DX, WORD PTR temp               
        CMP DX, 1                           
        JNE IgnoreEvent                       

        MOV DL, BYTE PTR [temp+4]
        CMP DL, 0
        JE IgnoreEvent
        MOV DL, BYTE PTR [temp+10]  ; Copy pressed key into DL

        ;check if ESC is pressed and Exit the game if so
        CMP DL, 1Bh                 
        JE quit  

        ;check if the snake is moving vertical 
        CMP d, 'w'                  
        JE moveHorizontal                    
        CMP d, 's'                  
        JE moveHorizontal                    

        ;the snake is moving horizontal
        JMP moveVertical

        moveHorizontal:
            CMP DL, 25h             ;check if left arrow was in input
            JE moveLeft
            CMP DL, 27h             ;check if right arrow was in input
            JE moveRight
            JMP IgnoreEvent

            moveLeft:
                MOV newD, 'a'       ;set new direction to left
                JMP IgnoreEvent
            moveRight:
                MOV newD, 'd'       ;set new direction to right
                JMP IgnoreEvent

        moveVertical:
            CMP DL, 26h             ;check if up arrow was in input
            JE moveUp
            CMP DL, 28h             ;check if down arrow was in input
            JE moveDown
            JMP IgnoreEvent           
            moveUp:
                MOV newD, 'w'       ;set new direction to up
                JMP IgnoreEvent
            moveDown:
                MOV newD, 's'       ;set new direction to down
                JMP IgnoreEvent

    IgnoreEvent:
        JMP gameLoop                            ;continue game loop

    finished:

        MOV BL, newD                        ;set new direction as snake
                                            ;direction
        MOV d, BL
        CALL MoveSnake                      ;TO DO
        MOV EAX, gamespeed  
        CALL Delay                          

        CMP eGame, 1                        ;check if end game flag is set
        JE quit                             ;(from a collision)

        JMP gameLoop                        ; Continue main loop

        quit:
        CALL clearMem                    
        MOV gamespeed, 100
    RET
  startGame ENDP


END main




