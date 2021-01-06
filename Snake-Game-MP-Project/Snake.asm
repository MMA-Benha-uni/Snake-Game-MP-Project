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

tR BYTE 16d
tC BYTE 47d
hR BYTE 13d
hC BYTE 47d
eTail   BYTE    1d  
search  WORD    0d 
eGame   BYTE    0d  
cScore  DWORD   0d  
d       BYTE    'w' 
newD    BYTE    'w' 


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



          genLevel ENDP

          clearMem PROC
              MOV BX, 0
              MOV DH, 0
              outerLoop:
                  CMP DH, 24
                  JE ExitOut
                  MOV DL, 0
                  innerLoop:
                      CMP DL, 80
                      JE ExitIn
                      call saveIndex
                      INC DL
                      JMP innerLoop
                  ExitIn:
                      INC DH
                      JMP outerLoop
              MOV tR, 16              
              MOV tC, 47              
              MOV hR, 13              
              MOV hC, 47
              MOV eGame, 0            
              MOV eTail, 1            
              MOV d, 'w'              
              MOV newD, 'w'           
              MOV cScore, 0 
              RET
          clearMem ENDP


END main




