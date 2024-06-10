IDEAL ;Elay's Base Asm
MODEL small
STACK 100h
RedC = 79
BlueC = 210
YellowC = 251

BlackC = 0
OgBallx = 130
OgBally = 110
Ogpaddlex = 130
Ogpaddleleftx = 130
Ogpaddlerightx = 170

DATASEG

	GameOverA db "Game Over!$"
	
	SCREEN_WIDTH = 320
	ScrLine db SCREEN_WIDTH dup (0)  ; One Color line read buffer
 
	FileHandle	dw ?
	Header 	    db 54 dup(0)
	Palette 	db 400h dup (0)
	
	Color db ?
	Xclick dw ?
	Yclick dw ?
	Xp dw ?
	Yp dw ?
	SquareSize dw ?	 
	BmpLeft dw ?
	BmpTop dw ?
	BmpWidth dw ?
	BmpHeight dw ?

	BmpFileErrorMsg    	db 'Error At Opening Bmp File ', 0dh, 0ah,'$'
	ErrorFile           db 0
	RedBricks 	db 'red.bmp' ,0
	BlueBricks 	db 'blue.bmp' ,0
	YellowBricks 	db 'yellow.bmp' ,0
	BlackBrick db 'black.bmp',0
	WhiteBrick db 'white.bmp',0
	
	BmpAdd dw 32
	
	paddlex dw 130
	paddleleftx dw 130
	paddlerightx dw 170
	paddley dw 170
	
	pixelreadx dw ?
	pixelready dw ?
	
	ballx dw 130
	bally dw 110
	
	ballxspeed dw -1
	ballyspeed dw 1
	
	WallRight dw 310
	WallLeft dw 10
	
	Ceiling dw 15
	Floor dw 190
	
	GameFlag db 0;if 0 game continue,if 1 game over.
	EscFlag  db ?;if 0 game start,if 1 end game
	EndFlag  db ?;if 0 you restart,if 1 you leave
	JmpFlag  db 0;if 1 there is a pop of a brick
	
	PixelFind db 0
	
	BrickToPopX dw ?
	BrickToPopY dw ?
	
	savex dw 0
	savey dw 20
		
	
			  ;brick number:   1, 2, 3, 4,  5,  6,  7,  8,  9, 10
	;every brick starts at x = 1,33,65,97,129,161,193,225,257,289
	
	
	Cry1 db 'cry1.bmp',0
	
	Screen db 'screen.bmp',0
	WinPic db 'WinPic.bmp',0
	
CODESEG
start:
	mov ax, @data 
	mov ds, ax
	
	call SetGraphic
	call StartScreen
	call ReadKeyBoard
	cmp [EscFlag],0
	jmp @@Setup
	jmp exit
@@Setup:
	call ClearScreen
	call DrawPaddle
	call DrawBall
	call RedBrickLoop
	call BlueBrickLoop
	call YellowBrickLoop
@@gamestart:
    call ClearPaddle
    call DrawPaddle
    call MoveBall
	; Update game state by checking ball collisions with borders
    call Borders

    ; Check if the game is over
    cmp [GameFlag], 1
    je @@GameOver
@@Input:
    ; Check if any key has been pressed
    mov al, 0
    mov ah, 1
    int 16h
    jz @@ContinueA  ;Nothing.

    ;Check which key has benn pressed
    mov ah, 0
    int 16h
    ; Check for left arrow key press
    cmp ah, 4Bh
    je @@MoveLeft

    ; Check for right arrow key press
    cmp ah, 4Dh
    jne @@ContinueA

    ; Move paddle right if in borders
    cmp [paddlex], 270
    ja @@ContinueA
    call MovePaddleRight
    jmp @@ContinueA

@@MoveLeft:
    ; Move paddle left if in borders
    cmp [paddlex], 10
    jb @@ContinueA
    call MovePaddleLeft

@@ContinueA:

	;call AutoWin
	
	call ReadPixels
	
	call WinCheck
	cmp [PixelFind],1
	je @@GameWin
	
	
    jmp @@gamestart 	; Return back
	

	
@@GameOver:
    call ClearScreen
	call GameOverScreen
	call ReadKeyGameOver
	cmp [EndFlag],0
	jne exit
	call ResetValues
	mov [GameFlag],0
	jmp @@Setup
	
@@GameWin:
	call GameWinScreen
	call ReadKeyBoard
	cmp [EscFlag],1
	je exit
	call ClearScreen
	call ResetValues
	jmp @@Setup
exit:
	mov ah,0
	int 16h
	
	mov ah,7 ;silent input
	int 21h
	
	mov ax,2  ; back to mode text 
	int 10h
	
	mov ax, 4c00h
    int 21h



proc ReadPixels	; קריאה ובדיקה של פיקסלים על המסך בנוגע לבדיקת נגיעת כדור עם הלבנים
	call CheckLeftX
	cmp [JmpFlag],1
	je @@Return
	call CheckRightX
	cmp [JmpFlag],1
	je @@Return
	call CheckUpperY
	cmp [JmpFlag],1
	je @@Return
	call CheckLowerY
	cmp [JmpFlag],1
	je @@Return
	call CheckCornerUpRight
	cmp [JmpFlag],1
	je @@Return
	call CheckCornerUpLeft
	cmp [JmpFlag],1
	je @@Return
	call CheckCornerLowRight
	cmp [JmpFlag],1
	je @@Return
	call CheckCornerLowLeft
	cmp [JmpFlag],1
	je @@Return
	call CheckRightMidUp1
	cmp [JmpFlag],1
	je @@Return
	call CheckRightMidUp2
	cmp [JmpFlag],1
	je @@Return
	call CheckLeftMidUp1
	cmp [JmpFlag],1
	je @@Return
	call CheckLeftMidUp2
@@Return:
	mov [JmpFlag],0	
	ret
endp ReadPixels

proc FindX; מציאת איקס הלבנה שצריך לפוצץ
	call CheckBrickNumber
	cmp [BrickToPopX],-1
	je @@Ret
	call PopBrick
	call FlipBally
@@Ret:
	ret
endp FindX

proc CheckBrickNumber; בדיקה על איזה לבנה מדובר ( איזה אחת צריך לפוצץ)
    push ax
    mov ax, [ballx]
@@brick1:
    cmp ax, 1
    jb @@CheckPoint
    cmp ax, 31
    ja @@brick2
    mov [BrickToPopX], 1
    jmp @@Ret
@@brick2:
    cmp ax, 33
    jb @@CheckPoint
    cmp ax, 63
    ja @@brick3
    mov [BrickToPopX], 33
    jmp @@Ret
@@brick3:
    cmp ax, 65
    jb @@CheckPoint
    cmp ax, 95
    ja @@brick4
    mov [BrickToPopX], 65
    jmp @@Ret
@@brick4:
    cmp ax, 97
    jb @@CheckPoint
    cmp ax, 127
    ja @@brick5
    mov [BrickToPopX], 97
    jmp @@Ret
@@brick5:
    cmp ax, 129
    jb @@CheckPoint
    cmp ax, 159
    ja @@brick6
    mov [BrickToPopX], 129
    jmp @@Ret


@@CheckPoint:
    jmp @@NoBrickFound
@@brick6:

    cmp ax, 161
    jb @@NoBrickFound
    cmp ax, 191
    ja @@brick7
    mov [BrickToPopX], 161
    jmp @@Ret
@@brick7:
    cmp ax, 193
    jb @@NoBrickFound
    cmp ax, 223
    ja @@brick8
    mov [BrickToPopX], 193
    jmp @@Ret
@@brick8:
    cmp ax, 225
    jb @@NoBrickFound
    cmp ax, 255
    ja @@brick9
    mov [BrickToPopX], 225
    jmp @@Ret
@@brick9:
    cmp ax, 257
    jb @@NoBrickFound
    cmp ax, 287
    ja @@brick10
    mov [BrickToPopX], 257
    jmp @@Ret
@@brick10:
    cmp ax, 289
    jb @@NoBrickFound
    cmp ax, 319
    ja @@NoBrickFound
    mov [BrickToPopX], 289
    jmp @@Ret
@@Ret:
    pop ax
    ret
@@NoBrickFound:
    mov [BrickToPopX], -1
    pop ax
    ret
endp CheckBrickNumber


proc PopBrick; פיצוץ לבנה על ידי צביעתה לשחור
	push ax
	push bx
	push dx
	mov ax,[BrickToPopX]
	mov bx,[BrickToPopY]
	
	mov dx,offset BlackBrick ;black brick like the background
	mov [BmpLeft],ax
	mov [BmpTop],bx
	mov [BmpWidth], 30 ;same for every brick
	mov [BmpHeight] ,15
	
	call OpenShowBmp
	pop dx
	pop bx
	pop ax
	ret
endp PopBrick

proc Borders;בדיקת גבולות המסך
	push cx
	push dx
@@XCheck:
	mov cx,[ballx]
	cmp cx,[WallLeft]
	je @@FlipX
	cmp cx,[WallRight]
	je @@FlipX
	jmp @@YCheck
@@FlipX:
	call FlipBallx
@@YCheck:
	mov dx,[bally]
	cmp dx,[Ceiling]
	je @@FlipY
	;Checking game over status
	cmp dx,[paddley]
	je @@CheckPaddle
	cmp dx,[Floor]
	jne @@Return
	mov [GameFlag],1
	jmp @@Return
@@CheckPaddle:
	cmp cx,[paddleleftx]
	jae @@CheckX
	jb @@Return
@@CheckX:
	cmp cx,[paddlerightx]
	jbe @@FlipY
	jmp @@Return
@@FlipY:
	call FlipBally
@@Return:
	pop dx
	pop cx
	ret
endp Borders

proc OpenShowBmp near;פתיחת התמונה
	mov [ErrorFile],0
	 
	call OpenBmpFile
	cmp [ErrorFile],1
	je @@ExitProc
	
	call ReadBmpHeader
	
	call ReadBmpPalette
	
	call CopyBmpPalette
	
	call ShowBMP
	
	 
	call CloseBmpFile

@@ExitProc:
	ret
endp OpenShowBmp

proc OpenBmpFile	near;פתיחת הקובץ	
				 
	mov ah, 3Dh ;Open file using handle
	xor al, al
	int 21h
	
	jc @@ErrorAtOpen ;if there is a carry its an error
	mov [FileHandle], ax
	jmp @@ExitProc
	
@@ErrorAtOpen:
	mov [ErrorFile],1
@@ExitProc:	
	ret
endp OpenBmpFile

proc CloseBmpFile near;סגירת קובץ
	mov ah,3Eh ;close file using handle
	mov bx, [FileHandle]
	int 21h
	ret
endp CloseBmpFile

proc ReadBmpHeader	near	;קורא מידע על הקובץ		
	push cx
	push dx
	
	mov ah,3fh ;Read file or device using handle
	mov bx, [FileHandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	
	pop dx
	pop cx
	ret
endp ReadBmpHeader

proc ReadBmpPalette near ; Read BMP file color palette, 256 colors * 4 bytes (400h)
						 ; 4 bytes for each color BGR + null)			
	push cx
	push dx
	
	mov ah,3fh ;Read file or device using handle
	mov cx,400h
	mov dx,offset Palette
	int 21h
	
	pop dx
	pop cx
	
	ret
endp ReadBmpPalette

proc ShowBMP;
; BMP graphics are saved upside-down.
; Read the graphic line by line (BmpHeight lines in VGA format),
; displaying the lines from bottom to top.
	push cx
	
	mov ax, 0A000h ;go to screen
	mov es, ax
	
	;
	mov ax,[BmpWidth] ; row size must dived by 4 so if it less we must calculate the extra padding bytes
	mov bp, 0
	and ax, 3
	jz @@row_ok
	mov bp,4
	sub bp,ax

@@row_ok:	
	mov cx,[BmpHeight]
    dec cx
	add cx,[BmpTop] ; add the Y on entire screen
	; next 5 lines  di will be  = cx*320 + dx , point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	add di,[BmpLeft]
	cld ; Clear direction flag, for movsb forward
	
	mov cx, [BmpHeight]
@@NextLine:
	push cx
 
	; small Read one line
	mov ah,3fh
	mov cx,[BmpWidth]  
	add cx,bp  ; extra  bytes to each row must be divided by 4
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory es:di
	mov cx,[BmpWidth]  
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	sub di,[BmpWidth]            ; return to left bmp
	sub di,SCREEN_WIDTH  ; jump one screen line up
	
	pop cx
	loop @@NextLine
	
	pop cx
	ret
endp ShowBMP

proc CopyBmpPalette		near	;העתקת הצבעים של התמונה			
										
	push cx
	push dx
	
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0  ; black first							
	out dx,al ;3C8h
	inc dx	  ;3C9h
CopyNextColor:
	mov al,[si+2] 		; Red				
	shr al,2 			; divide by 4 Max (cos max is 63 and we have here max 255 ) (loosing color resolution).				
	out dx,al 						
	mov al,[si+1] 		; Green.				
	shr al,2            
	out dx,al 							
	mov al,[si] 		; Blue.				
	shr al,2            
	out dx,al 							
	add si,4 			; Point to next color.  (4 bytes for each color BGR + null)				
								
	loop CopyNextColor
	
	pop dx
	pop cx
	
	ret
endp CopyBmpPalette

proc SetGraphic;פתיחת מסך גרפי
	; http://stanislavs.org/helppc/int_10-0.html
	mov ax,13h   ; 320 X 200 
				 ;Mode 13h is an IBM VGA BIOS mode. It is the specific standard 256-color mode 
	int 10h
	ret
endp SetGraphic
		
proc PaddleTouch;בדיקת נגיעה של הכדור במשטח
	push cx
	push dx
	mov dx,[bally]
	cmp dx,[paddley]
	jne @@Return
	mov cx,[ballx]
	mov dx,[paddleleftx]
	cmp cx,dx
	jae @@CheckRight
	jmp @@Return
@@CheckRight:
	mov dx,[paddlerightx]
	cmp cx,dx
	jbe @@BallStillIn
	jmp @@Return	
@@BallStillIn:
	call FlipBallx
@@Return:
	pop dx
	pop cx
	ret
endp PaddleTouch

proc DrawBall ;ציור בלבן של הכדור
     mov       di,[bally]             ;x
	 mov       dx,[ballx]              ;y
     mov       al,-1      ;colour
     mov       bx,2            ;radius
     mov bp,0                ;x coordinate
     mov si,bx               ;y coordinate
@@c00:                
	xchg      bp,si               ;swap x and y
	call      _2pixelsV1            ;2 pixels
	call      _2pixelsV1 
					
	xchg      bp,si               ;swap x and y
	call      _2pixelsV1            ;2 pixels
	call      _2pixelsV1 
					
					
	sub       bx,bp               ;d=d-x
    inc       bp                  ;x+1
	sub       bx,bp               ;d=d-(2x+1)
	jg        @@c01                 ;>> no step for y
    add       bx,si               ;d=d+y
    dec       si                  ;y-1
	add       bx,si               ;d=d+(2y-1)
@@c01:
	cmp       si,bp               ;check x>y
    jae       @@c00                 ;>> need more pixels
					
                    ret
endp DrawBall

proc UndrawBall; לצייר את הכדור פשוט בשחור
	 mov       di,[bally]             ;x
	 mov       dx,[ballx]              ;y
     mov       al,0 ;UndrawBall             ;colour
     mov       bx,2           ;radius
     mov bp,0                ;x coordinate
     mov si,bx               ;y coordinate
@@c00:                
	xchg      bp,si               ;swap x and y
	call      _2pixelsV1            ;2 pixels
	call      _2pixelsV1 
					
	xchg      bp,si               ;swap x and y
	call      _2pixelsV1            ;2 pixels
	call      _2pixelsV1 
					
					
	sub       bx,bp               ;d=d-x
    inc       bp                  ;x+1
	sub       bx,bp               ;d=d-(2x+1)
	jg        @@c01                 ;>> no step for y
    add       bx,si               ;d=d+y
    dec       si                  ;y-1
	add       bx,si               ;d=d+(2y-1)
@@c01:
	cmp       si,bp               ;check x>y
    jae       @@c00                 ;>> need more pixels
					
                    ret
endp UndrawBall

proc _2pixelsV1 ;כדור
					neg       si
                    push      di
                    add       di,si
					
					mov   cx,di    ; next 5 lines mul by 320 and add column
					shl   di,6     
					shl   cx,8
					add   di,cx
					add   di,dx
					 			
					mov       [es:di+bp],al
 					sub       di,bp
                    mov       [es:di],al

 					pop       di
                    ret 
endp _2pixelsV1

proc  RegisterAsyncMouse; עכבר אסינכרוני לבדיקות
									
	
;----- Define interrupt subroutine parameters
        ; mov ax, seg ClickHereToPlay
        mov es, ax
        ; mov dx, offset ClickHereToPlay  ; ES:DX ->Far routine
        mov cx,0000000000011110b ;
		mov ax,0Ch
        int 33h                 
       	ret
endp RegisterAsyncMouse

proc DrawRegularLine ;

	mov cx,20
	mov dx,10
	mov al,15
	mov si,40
@@DrawLine:
	cmp si,0
	jz @@ExitDrawLine		 
    mov ah,0ch
	int 10h    ; put pixel  cx=x dx=y al = color
	 	
	inc cx
	dec si
	jmp @@DrawLine
		
@@ExitDrawLine:
	ret
endp DrawRegularLine

proc ClearScreen ;לנקות את כל המסך בשחור
	mov cx,32000
	mov ax, 0a000h
	mov es,ax
	mov di,0
@@nextW:
	mov [word es:di],0
	add di,2
	loop @@nextW
	ret
endp ClearScreen

proc ClearPaddle ;מחיקת הכדור
	push si
	push ax
	push cx
	push dx
	mov cx,[paddlex]
	mov dx,[paddley]
	mov al,0
	mov si,40
@@DrawLine:
	cmp si,0
	jz @@ExitDrawLine		 
    mov ah,0ch
	int 10h    ; put pixel  cx=x dx=y al = color
	 	
	inc cx
	dec si
	jmp @@DrawLine
		
@@ExitDrawLine:
	pop si
	pop ax
	pop cx
	pop dx
	ret
endp ClearPaddle

proc DrawPaddle ; ציור הכדור
	push si
	push ax
	push cx
	push dx
	mov cx,[paddlex]
	mov dx,[paddley]
	mov al,-1
	mov si,40
@@DrawLine:
	cmp si,0
	jz @@ExitDrawLine		 
    mov ah,0ch
	int 10h    ; put pixel  cx=x dx=y al = color
	 	
	inc cx
	dec si
	jmp @@DrawLine
		
@@ExitDrawLine:
	pop si
	pop ax
	pop cx
	pop dx
	ret
endp DrawPaddle

proc MoveBall ;  הזזת הכדור
	push cx
	push dx
	call _200MiliSecDelay
	call UndrawBall
	mov cx,[ballxspeed]
	mov dx,[ballyspeed]
	add [ballx],cx
	add [bally],dx
	call DrawBall
	pop dx
	pop cx
	ret
endp MoveBall

proc MovePaddleRight ;הזזה ימינה של המשטח
	push cx
	call ClearPaddle
	add [paddlex],10
	mov cx,[paddlex]
	mov [paddleleftx],cx
	add [paddlerightx],10
	call DrawPaddle
	;call ShowAxDecimal
	pop cx
	ret
endp MovePaddleRight

proc MovePaddleLeft ;הזזה שמאלה של המשטח
	push cx
	call ClearPaddle
	sub [paddlex],10
	mov cx,[paddlex]
	mov [paddleleftx],cx
	sub [paddlerightx],10
	call DrawPaddle
	;call ShowAxDecimal
	pop cx
	ret
endp MovePaddleLeft

proc ShowAxDecimal ;מדפיס את AX כדצימלי
	   push ax
       push bx
	   push cx
	   push dx
	   
	   ; check if negative
	   test ax,08000h
	   jz PositiveAx
			
	   ;  put '-' on the screen
	   push ax
	   mov dl,'-'
	   mov ah,2
	   int 21h
	   pop ax

	   neg ax ; make it positive
PositiveAx:
       mov cx,0   ; will count how many time we did push 
       mov bx,10  ; the divider
   
put_mode_to_stack:
       xor dx,dx
       div bx
       add dl,30h
	   ; dl is the current LSB digit 
	   ; we cant push only dl so we push all dx
       push dx    
       inc cx
       cmp ax,9   ; check if it is the last time to div
       jg put_mode_to_stack

	   cmp ax,0
	   jz pop_next  ; jump if ax was totally 0
       add al,30h  
	   mov dl, al    
  	   mov ah, 2h
	   int 21h        ; show first digit MSB
	       
pop_next: 
       pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
	   mov dl, al
       mov ah, 2h
	   int 21h        ; show all rest digits
       loop pop_next
		
	   mov dl, ' '
       mov ah, 2h
	   int 21h
   
	   pop dx
	   pop cx
	   pop bx
	   pop ax
	   
	   ret
endp ShowAxDecimal

proc _200MiliSecDelay;דילאיי
	push cx
	
	mov cx ,150
@@Self1:
	
	push cx
	mov cx,300 

@@Self2:	
	loop @@Self2
	
	pop cx
	loop @@Self1
	
	pop cx
	ret
endp _200MiliSecDelay

proc FlipBallx;הופך את ערך האיקס של הכדור מחיובי לשלילי או הפוך
	neg [ballxspeed]
	ret
endp FlipBallx

proc FlipBally;הופך את ערך הווי של הכדור מחיובי לשלילי או הפוך
	neg [ballyspeed]
	ret
endp FlipBally

proc ShowRedBricks;מראה לבנה אדומה אחת (לבדיקות שהיו לי)
	push dx
	mov dx,offset RedBricks
	mov [BmpLeft],20
	mov [BmpTop],20
	mov [BmpWidth], 25
	mov [BmpHeight] ,15
	
	call OpenShowBmp
	pop dx
	ret
endp ShowRedBricks

					 ;brick number:         1, 2, 3, 4,  5,  6,  7,  8,  9, 10
proc RedBrickLoop;every brick starts at x = 1,33,65,97,129,161,193,225,257,289 ; לולאה השמה על המשך לבנים בצבע אדום
	push ax
	push cx
	mov [BmpLeft],1
	mov [BmpTop],20
	mov [BmpWidth], 30
	mov [BmpHeight] ,15
	;pixel at x= 34, y = 22
	mov cx,10
@@DrawRed:
	mov dx,offset RedBricks
	call OpenShowBmp
	mov ax,[BmpAdd]; bmp add = 32
	add [BmpLeft],ax
	loop @@DrawRed
	pop cx
	pop ax
	ret
endp RedBrickLoop
	
proc BlueBrickLoop; לולאה השמה על המשך לבנים בצבע כחול
	push ax
	push cx
	mov [BmpLeft],1
	mov [BmpTop],40
	mov [BmpWidth], 30
	mov [BmpHeight] ,15
	
	mov cx,10
@@DrawBlue:
	mov dx,offset BlueBricks
	call OpenShowBmp
	mov ax,[BmpAdd]
	add [BmpLeft],ax
	loop @@DrawBlue
	pop cx
	pop ax
	ret
endp BlueBrickLoop

proc YellowBrickLoop; לולאה השמה על המשך לבנים בצבע צהוב 
	push ax
	push cx
	mov [BmpLeft],1
	mov [BmpTop],60
	mov [BmpWidth], 30
	mov [BmpHeight] ,15
	
	mov cx,10
@@DrawYellow:
	mov dx,offset YellowBricks
	call OpenShowBmp
	mov ax,[BmpAdd]
	add [BmpLeft],ax
	loop @@DrawYellow
	pop cx
	pop ax
	ret
endp YellowBrickLoop

proc GameOverScreen;מסך הפסד
	mov dx,offset Cry1
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpWidth], 320
	mov [BmpHeight] ,200
	call OpenShowBmp

	call ReadKeyGameOver
	ret
endp GameOverScreen

proc GameWinScreen;מסך נצחון
	mov dx,offset WinPic
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpWidth], 320
	mov [BmpHeight] ,200
	call OpenShowBmp
	ret
endp GameWinScreen

proc StartScreen;מסך פתיחה
	mov dx,offset Screen
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpWidth], 320
	mov [BmpHeight] ,200
	call OpenShowBmp
	ret
endp StartScreen

proc Sound;סאונד פגיעה
	mov cx, 3               ; Initialize outer loop counter for 3 repetitions
@@new:
    push cx                 ; Save CX to stack

    ; Configure the timer
    mov al, 10110110b       ; Control Word for timer mode
    out 43h, al             ; Send Control Word to timer port

    ; Define frequency and duration patterns for a "Game Over" sound
    mov bx, 2000            ; Starting frequency for a lower tone
    mov dx, 30              ; Duration for each frequency step

@@next_frequency:
    mov ax, bx              ; Load the current frequency into AX
    out 42h, al             ; Send lower byte of frequency to port
    mov al, ah
    out 42h, al             ; Send upper byte of frequency to port

    in al, 61h              ; Read the speaker control port
    or al, 00000011b        ; Turn on the speaker
    out 61h, al

    ; Delay loop for sound duration
    mov cx, 6000            ; Adjust duration of each tone
@@delay_loop:
    loop @@delay_loop       ; Loop to create delay (tone duration)

    ; Reduce frequency for descending tone effect
    sub bx, 100             ; Decrease frequency to lower the pitch
    dec dx                  ; Decrement inner loop counter
    cmp dx, 0
    jnz @@next_frequency    ; Repeat inner loop if DX is not zero

    ; Silence the speaker briefly between tones
    in al, 61h
    and al, 11111100b       ; Turn off the speaker
    out 61h, al

    ; Brief pause between repetitions
    mov cx, 5000            ; Delay for the pause
@@pause_loop:
    loop @@pause_loop

    pop cx                  ; Restore CX from stack
    loop @@new              ; Repeat the outer loop
    ret
endp Sound

proc ResetValues ; מאפס ערכים
	mov [ballx],OgBallx
	mov [bally],OgBally
	mov [paddlex],Ogpaddlex
	mov [paddleleftx],Ogpaddleleftx
	mov [paddlerightx],Ogpaddlerightx
	mov [GameFlag],0
	mov [EscFlag],0
	mov [EndFlag],0
	mov [JmpFlag],0
	mov [PixelFind],0
	mov [savex],0
	mov [savey],20
	call FlipBally
	ret
endp ResetValues

proc ReadKeyBoard; קריאת מקלדת בתחילת משחק (אם יציאה או כניסה לפעולה)
	push ax
	mov al, 0
    mov ah, 1
    int 16h
    jz @@Loop
@@Pooling:
	mov ah, 0
    int 16h
	cmp ah,1ch
	je @@Start
	cmp ah,01h
	je @@EndScreen
	jmp @@Loop
@@Start:
	mov [EscFlag],0
	jmp @@End
@@EndScreen:
	mov [EscFlag],1
	jmp @@End
@@Loop:
	jmp @@Pooling
@@End:
	pop ax
	Ret
endp ReadKeyBoard

proc ReadKeyGameOver ;בודק האם לחצת על מקש שייסים את המשחק או יתחיל מחדש את המשחק (כאשר הפסדת)
	push ax
	mov al, 0
    mov ah, 1
    int 16h
    jz @@Loop
@@Pooling:
	mov ah, 0
    int 16h
	cmp ah,1ch
	je @@Start
	cmp ah,01h
	je @@EndScreen
	jmp @@Loop
@@Start:
	mov [EndFlag],0
	jmp @@End
@@EndScreen:
	mov [EndFlag],1
	jmp @@End
@@Loop:
	jmp @@Pooling
@@End:
	pop ax
	Ret
endp ReadKeyGameOver



;מהשורה הזאת עד שורה 2110 הכל בדיקות של נגיעת כדור (הכל כמעט אותו הדבר)
proc CheckLeftX;good! (-3,0)
	push ax
	push bx
	push cx
	push dx
	
@@YellowCheck:

	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,3
	mov dx,[bally]
	int 10h
	
	cmp al,YellowC
	jne @@BlueCheck

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
	
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,3
	mov dx,[bally]
	int 10h
	
	
	
	cmp al,BlueC
	jne @@RedCheck
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
	
@@RedCheck:	
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,3
	mov dx,[bally]
	int 10h
	
	
	
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
	
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckLeftX

proc CheckRightX;good!(3,0)
	push ax
	push bx
	push cx
	push dx
	
@@YellowCheck:

	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,3
	mov dx,[bally]
	int 10h
	
	cmp al,YellowC
	jne @@BlueCheck

	mov ah,0
	
	mov [BrickToPopY],60
	mov [JmpFlag],1
	call FindX
	jmp @@NoChanges
	
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,3
	mov dx,[bally]
	int 10h
		
	cmp al,BlueC
	jne @@RedCheck
	
	mov ah,0
		
	mov [BrickToPopY],40
	mov [JmpFlag],1
	call FindX	
	jmp @@NoChanges
	
@@RedCheck:	
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,3
	mov dx,[bally]
	int 10h
		
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	mov [JmpFlag],1
	call FindX
	jmp @@NoChanges
	
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckRightX

proc CheckUpperY;good!(0,3) fix
	push ax
	push bx
	push cx
	push dx
	
@@YellowCheck:

	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	mov dx,[bally]
	sub dx,5
	int 10h
	
	cmp al,YellowC
	jne @@BlueCheck

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
	
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	mov dx,[bally]
	sub dx,5
	int 10h
	
	
	
	cmp al,BlueC
	jne @@RedCheck
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
	
@@RedCheck:	
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	mov dx,[bally]
	sub dx,5
	int 10h
	
	
	
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
	
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckUpperY

proc CheckLowerY;good!(0,-3)
	push ax
	push bx
	push cx
	push dx
	
	; mov ax,[bally]
	; sub ax,3
	; cmp ax,68
	; je @@YellowCheck
	; cmp ax,48
	; je @@BlueCheck
	; cmp ax,28
	; je @@RedCheck	
	; jmp @@NoChanges
	
@@YellowCheck:

	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	mov dx,[bally]
	add dx,3
	int 10h
	
	; mov ax,0A000h
	; mov es,ax
	; mov bx,dx
	; mov al,200
	; mul bl
	; add al,cl
	; mov bl,al
	; xor bh,bh
	; mov al,[es:bx]

	cmp al,YellowC
	jne @@BlueCheck

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
	
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	mov dx,[bally]
	add dx,3
	int 10h
	
	
	
	cmp al,BlueC
	jne @@RedCheck
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
	
@@RedCheck:	
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	mov dx,[bally]
	add dx,3
	int 10h
	
	
	
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
	
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckLowerY

proc CheckCornerUpRight;good!(3,3) fix
	push ax
	push bx
	push cx
	push dx
@@YellowCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,3
	mov dx,[bally]
	sub dx,5
	int 10h
	
	cmp al,YellowC
	jne @@Return

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,3
	mov dx,[bally]
	sub dx,5
	int 10h
	
	cmp al,BlueC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@RedCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,3
	mov dx,[bally]
	sub dx,5
	int 10h
		
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:		
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckCornerUpRight

proc CheckCornerUpLeft;good!(-3,3) fix
	push ax
	push bx
	push cx
	push dx
@@YellowCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,3
	mov dx,[bally]
	sub dx,5
	int 10h
	
	cmp al,YellowC
	jne @@Return

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,3
	mov dx,[bally]
	sub dx,5
	int 10h
	
	cmp al,BlueC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@RedCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,3
	mov dx,[bally]
	sub dx,5
	int 10h
		
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:		
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckCornerUpLeft

proc CheckCornerLowRight;good!(3,-3)
	push ax
	push bx
	push cx
	push dx
@@YellowCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,3
	mov dx,[bally]
	add dx,3
	int 10h
	
	cmp al,YellowC
	jne @@Return

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,3
	mov dx,[bally]
	add dx,3
	int 10h
	
	cmp al,BlueC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@RedCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,3
	mov dx,[bally]
	add dx,3
	int 10h
		
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:		
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckCornerLowRight

proc CheckCornerLowLeft;good!(-3,-3)
	push ax
	push bx
	push cx
	push dx
@@YellowCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,3
	mov dx,[bally]
	add dx,3
	int 10h
	
	cmp al,YellowC
	jne @@Return

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,3
	mov dx,[bally]
	add dx,3
	int 10h
	
	cmp al,BlueC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@RedCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,3
	mov dx,[bally]
	add dx,3
	int 10h
		
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:		
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckCornerLowLeft

proc CheckRightMidUp1;(1,3)
	push ax
	push bx
	push cx
	push dx
@@YellowCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,1
	mov dx,[bally]
	sub dx,4
	int 10h
	
	cmp al,YellowC
	jne @@Return

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,1
	mov dx,[bally]
	sub dx,3
	int 10h
	
	cmp al,BlueC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@RedCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,1
	mov dx,[bally]
	sub dx,3
	int 10h
		
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:		
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckRightMidUp1

proc CheckRightMidUp2;(2,3)
	push ax
	push bx
	push cx
	push dx
@@YellowCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,2
	mov dx,[bally]
	sub dx,3
	int 10h
	
	cmp al,YellowC
	jne @@Return

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,2
	mov dx,[bally]
	sub dx,4
	int 10h
	
	cmp al,BlueC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@RedCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	add cx,2
	mov dx,[bally]
	sub dx,3
	int 10h
		
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:		
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckRightMidUp2

proc CheckLeftMidUp1;(-1,3)
	push ax
	push bx
	push cx
	push dx
@@YellowCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,1
	mov dx,[bally]
	sub dx,3
	int 10h
	
	cmp al,YellowC
	jne @@Return

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,1
	mov dx,[bally]
	sub dx,4
	int 10h
	
	cmp al,BlueC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@RedCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,1
	mov dx,[bally]
	sub dx,3
	int 10h
		
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:		
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckLeftMidUp1

proc CheckLeftMidUp2;(-2,3) 
	push ax
	push bx
	push cx
	push dx
@@YellowCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,2
	mov dx,[bally]
	sub dx,3
	int 10h
	
	cmp al,YellowC
	jne @@Return

	mov ah,0
	
	mov [BrickToPopY],60
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@BlueCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,2
	mov dx,[bally]
	sub dx,4
	int 10h
	
	cmp al,BlueC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],40
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@RedCheck:
	mov ah,0dh
	mov bh,0
	mov cx,[ballx]
	sub cx,2
	mov dx,[bally]
	sub dx,3
	int 10h
		
	cmp al,RedC
	jne @@Return
	
	mov ah,0
		
	mov [BrickToPopY],20
	call FindX
	mov [JmpFlag],1
	jmp @@NoChanges
@@Return:
	mov [BrickToPopX],-1
@@NoChanges:		
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp CheckLeftMidUp2

proc WinCheck ;בדיקת נצחון
	push ax
	push bx
	push cx
	push dx
@@check_pixel:
	mov ah,0dh
	mov bh,0
	mov cx,[savex]
	mov dx,[savey]
	int 10h
	
	cmp al,YellowC
	je @@found
	cmp al,BlueC
	je @@found
	cmp al,RedC
	je @@found
	
	inc [savex]
	cmp [savex],320
	jne @@check_pixel
	
	mov [savex],0
	inc [savey]
	cmp [savey],90
	je @@not_found
	
	jmp @@check_pixel
	
@@not_found:
	mov [PixelFind],1
@@found:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp WinCheck

proc AutoWin
	mov [PixelFind],1
	ret
endp AutoWin

END start

