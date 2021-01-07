INCLUDE Irvine32.inc

.DATA

a WORD 1920 DUP (0)

menus BYTE " 1-start game",0dh,0ah,"2-speed",0Dh,0Ah,
          "3-level",0dh,0ah,"4- Exit",0Dh,0Ah, 0

levels  BYTE "1. None", 0Dh, 0Ah, "2. Box", 0Dh, 0Ah, "3. Rooms", 0Dh, 0Ah, 0
speeds BYTE "1", 0Dh, 0Ah, "2", 0Dh, 0Ah, "3",
             0Dh, 0Ah, "4", 0Dh, 0Ah, 0
hit    BYTE "Game Over!", 0
score  BYTE "Score: 0", 0
gamespeed DWORD   60 ;

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
    JE startGame

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
    CALL clearMem          ; to be implemented     
    MOV AL, 1
    CALL GenLevel ; to be implemented
    JMP menu

    level2: 
    CALL clearMem    ; to be implemented
    MOV AL, 2 
    CALL GenLevel    ; to be implemented
    JMP menu

    level3: 
    CALL clearMem   ;to be implemented
    MOV AL, 3    
    CALL GenLevel   ;to be implemented
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
    CALL initSnake        ;to be implemented       
    CALL Paint            ;to be implemented          
    CALL createFood       ;to be implemented         
    CALL startGame        
    MOV EAX, white + (black * 16)
    CALL SetTextColor          
    JMP menu                  







main ENDP

initSnake PROC USES  EBX EDX ECX

; This procedure initializes the snake to the default position
; in almost the center of the screen
    .data
    
        cRow BYTE 13
        cColumn BYTE 47
        tmpCounter word 1

    .code

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






END main




