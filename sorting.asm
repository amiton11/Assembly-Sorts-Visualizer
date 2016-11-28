;Code written by Amit Alon

jumps

regularColor MACRO
	mov ax, 31
    push ax
    push ax
ENDM

showMainArr MACRO
    push offset mainArr
    push len
    call showArr
ENDM

defaultShowArr MACRO
	regularColor
	showMainArr
ENDM

prepareForSort MACRO
	mov is_continuos, 0
    mov ah, 9
    int 21h
    inc cursorY
    mov cursorX, 0
    
    defaultShowArr    
ENDM

restoreMainArr MACRO
	push len
    push offset mainArr
    push offset copirdArr
    call copyArr
ENDM

data	segment
    mainArr db 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
    copirdArr db 30 dup(?) 
    len dw 30
    numStr db 4, 0, 0, 0, 0, 0
    inputStr db 2, 0, 0, 0
    Str1 db 'Enter length of array(2-30): $'
    Str2 db 'Enter 1 for entering values for the array by yourself', 10, 13, 'enter 2 for random value', 10, 13, 'enter 3 for default values', 10, 13, '$'
    Str3 db 'Enter a number(0-255): $'
    Str4 db '1 - Bubble sort', 10, 13, '2 - Insertion sort', 10, 13, '3 - Radix sort', 10, 13, '4 - Quick sort', 10, 13, '5 - all of them', 10, 13, 'Enter num of sorting algorithem: $' 
    Str5 db 'Press right arrow key for next swap,',10,13,'up arrow key for continuous sorting', 10, 13, '$'
    Str6 db 'Press right arrow key for next sorting algorithem', 10, 13, '$'
    Str7 db 'Press anything to continue', 10, 13, '$'
    ErrStr db 'Problem with entered num!', 10, 13, 'Try again', 10, 13, '$'
    inputErrStr db 'Problem with entered input!', 10, 13, 'Try again', 10, 13, '$'
    is_continuos db 0
    comaAndSpace db ', $'
    cursorX db 0
    cursorY db 0
    strBubble db 'Bubble Sort', 10, 13, '$'
    strInsertion db 'Insertion Sort', 10, 13, '$'
    strRadix db 'Radix Sort', 10, 13, '$'
    strQuick db 'Quick Sort', 10, 13, '$'
    strMigration db 'Migration Sort', 10, 13, '$' 
	tempRadixArr db 30 dup(?)
	radixIndex db 0    
data    ends

stac    segment stack
            dw 300h dup(?)
stac    ends

code    segment
assume cs:code, ds:data, ss:stac

;shows the menu
menu PROC; finished
	push ax bx cx dx
    
    ;gets the wanted size of array
    mov dx, offset Str1
    mov ah, 9h
    int 21h
    getLength:
    call getNum
    cmp dl, 2
    JL inputLengthErr
    cmp dl, 30
    JG inputLengthErr
    JMP okLength
    inputLengthErr:
    mov dx, offset ErrStr
    mov ah, 9
    int 21h
    add cursorY, 2
    mov cursorX, 0
    JMP getLength 
    okLength:
    xor dh, dh
    mov len, dx
    
    ;asks the user if he wants to set the values, want them to be random, or want them to be default
    rnd_or_user:
    mov dx, offset Str2
    mov ah, 9h
    int 21h
    add cursorY, 3
    mov cursorX, 0
    mov dx, offset inputStr
    mov bx, dx
    mov ah, 0Ah
    int 21h
    call newLine
    xor ah, ah
    mov bx, dx
    mov al, [bx + 2]
    cmp al, '1'
    JZ enter_val_by_user
    cmp al, '2'
    JZ enter_rndm_val
    cmp al, '3'
    JZ sort_menu
    mov dx, offset inputErrStr
    mov ah, 9
    int 21h
    add cursorY, 2
    mov cursorX, 0
    JMP rnd_or_user
    
    enter_val_by_user:
    call valByUser
    JMP sort_menu
    
    enter_rndm_val:
    call rndmVal
    
    sort_menu:
    cmp cursorY, 18
    JL askWhichSort
    call ClearScreen
    askWhichSort:
    mov dx, offset Str4
    mov ah, 9h
    int 21h
    add cursorY, 6
    call getNum
    cmp dl, 1
    JZ callBubbleSort
    cmp dl, 2
    JZ callInsertionSort
    cmp dl, 3
    JZ callRadixSort
    cmp dl, 4
    JZ callQuickSort
    cmp dl, 5
    JZ callAllSorts 
    mov dx, offset ErrStr
    mov ah, 9
    int 21h
    add cursorY, 2
    mov cursorX, 0
    JMP sort_menu
    
    callBubbleSort:
    mov dx, offset strBubble

    prepareForSort 
    
    call bubbleSort
    JMP endSortMenu
    
    callInsertionSort:
    mov dx, offset strInsertion

    prepareForSort
    
    call insertionSort
    JMP endSortMenu
    
    callRadixSort:
    mov dx, offset strRadix

    prepareForSort
    
    call radixSort
    JMP endSortMenu
    
    callQuickSort:
    mov dx, offset strQuick

    prepareForSort
    
    call quickSort
    JMP endSortMenu
    
    callAllSorts:

    push len
    push offset copirdArr
    push offset mainArr
    call copyArr

    mov dx, offset strBubble

    prepareForSort
     
    call bubbleSort

    restoreMainArr

    call sortsMenu
    
    mov dx, offset strInsertion

    prepareForSort
    
    call insertionSort

    restoreMainArr

    call sortsMenu
                  
    mov dx, offset strRadix

    prepareForSort
    
    call radixSort

	restoreMainArr

    call sortsMenu
    
    mov dx, offset strQuick

    prepareForSort
    
    call quickSort

    restoreMainArr             
    
    endSortMenu:
    defaultShowArr
    
    pop dx cx bx ax
    
    ret
menu ENDP

;puts random values in the main array
rndmVal PROC; finished
    push ax bx cx dx
   	 
    mov ah, 00h
    int 1Ah ; return clock count in cx:dx
    mov bx, len
    dec bx
    getRndNum:
    	push bx

    	mov ax, cx
    	mov cx, dx
    	mov bx, 1049
    	mul bx ; ax:dx = ax * bx
    	add ax, 1447
    	xor dx, ax
    	mov ax, cx
    	mov cx, dx
    	mov bx, 1273
    	mul bx
    	add ax, 2203
    	xor dx, ax
    	mov al, dl
    	xor al, dh
    	xor al, cl
    	xor al, ch

    	pop bx
    	mov byte ptr mainArr[bx], al
    	dec bx
    	JGE getRndNum

    pop dx cx bx ax

    ret
rndmVal ENDP

;calls the recursive 
quickSort PROC; finished
    push ax

	xor ax, ax
	push ax
	mov ax, len
	dec ax
	push ax

	call QuickSortRecursive

	pop ax
	ret
quickSort ENDP

;sorts the main array using quickSort
QuickSortRecursive PROC; finished
	RightIndex equ 4
    LeftIndex equ 6

    push bp
    mov bp, sp
    push ax bx cx dx si di

    mov bx, [bp + rightIndex]
    mov di, [bp + LeftIndex]
    cmp bx, di
    JLE EndQuickSort

    mov si, bx
    add si, di
    shr si, 1 ; si is the pivot, the index of the middle element in the array, si = (RightIndex + LeftIndex) / 2, 
    mainQuickLoop:
        cmp di, bx
        JG callRecursiveQuick ; stop when left index bigger then right index
        leftIndexLoop: ; find an element from the left of the pivot that is bigger or even to the pivot
            mov al, byte ptr mainArr[si]
            cmp al, byte ptr mainArr[di]
            JBE RightIndexLoop
            inc di
            JMP leftIndexLoop
        RightIndexLoop: ; find an element from the right of the pivot that is smaller or even to the pivot
            mov al, byte ptr mainArr[si]
            cmp al, byte ptr mainArr[bx]
            JAE endRightIndexLoop
            dec bx
            JMP RightIndexLoop
        endRightIndexLoop:
        cmp di, bx
        JG callRecursiveQuick
        JE endQuickShowArr
        cmp cursorY, 24
        JL swap_quick
        call clearScreen
        swap_quick:
        mov dx, offset mainArr
        add dx, di
        push dx
        sub dx, di
        add dx, bx
        push dx
        call Swap
        cmp si, di
        JNE leftNotPivot
        mov si, bx
        JMP finishPivotCheck
        leftNotPivot:
        cmp si, bx
        JNZ finishPivotCheck
        mov si, di
        finishPivotCheck:
        push di
        push bx
        showMainArr
        call swap_menu
        endQuickShowArr:
        inc di
        dec bx
        JMP mainQuickLoop
    callRecursiveQuick:
    mov ax, [bp + rightIndex]
    mov cx, [bp + LeftIndex]
  	cmp si, cx
    JLE callRightQuickSort
    push cx
    mov dx, si
    dec dx
    push dx
    call QuickSortRecursive
    callRightQuickSort:
    cmp ax, si
    JLE EndQuickSort
   	mov dx, si
   	inc dx
   	push dx
    push ax
    call QuickSortRecursive
    EndQuickSort:
    pop di si dx cx bx ax bp

    ret 4
QuickSortRecursive ENDP

;sorts the array mainArr from small to large using binary radix sort
radixSort PROC ; finished
    shift equ -1
    arr_pointer equ -3

    push bp ax bx cx si di

    mov bp, sp
    
    sub sp, 3 ; because of local variables

    mov ah, 0
    mov byte ptr [bp + shift], 1

    radix_outer_loop:
    	mov word ptr [bp + arr_pointer], 0
    	xor si,si

    	radix_inner_loop:
    		mov al, mainArr[si]
    		mov cl, [bp + shift]
    		rcr al, cl
    		mov al, mainArr[si]
    		JC radix_one

    		mov di, si
    		sub di, [bp + arr_pointer]
    		mov mainArr[di], al
    		JMP end_shift

    		radix_one:
    		mov di, word ptr [bp + arr_pointer]
    		mov tempRadixArr[di], al
    		inc word ptr [bp + arr_pointer]

    		end_shift:
    		inc si
    		cmp si, len
    		JNZ radix_inner_loop

    	cmp cursorY, 18
        JL copy_radix_temp
        call clearScreen 

        copy_radix_temp:
        cmp [bp + arr_pointer], 0
        jz end_copy_radix
        ;print radix arr
        call newLine
        regularColor
        push offset tempRadixArr
        push [bp + arr_pointer]
        call showArr
        call newLine
        regularColor
        push offset mainArr
        mov ax, len
    	sub ax, [bp + arr_pointer]
    	push ax
    	call showArr
    	call newLine
        ;print main arr before copying radix	
    	push [bp + arr_pointer]
    	add ax, offset mainArr
    	push ax
	    push offset tempRadixArr
		call copyArr

	    defaultShowArr
	    call swap_menu
	    end_copy_radix:
	    inc byte ptr [bp + shift]
	    cmp byte ptr [bp + shift], 9
	    JNZ radix_outer_loop
    
    add sp, 3 ; because of local variables
    
    pop di si cx bx ax bp

    ret
radixSort ENDP

;waits for user to press the right arrow key
sortsMenu PROC ;finished
    push ax dx
    
    cmp cursorY, 24
    JL startSortsMenu
    call ClearScreen
    startSortsMenu:
    mov dx, offset Str6
    mov ah, 9
    int 21h
    add cursorY, 1
    mov cursorX, 0
    
    check_key:
    mov ah, 1
    int 16h
    JE check_key
    mov ah, 0
    int 16h         
    cmp ah, 4Dh
    JE end_menu
    JMP check_key
    
    end_menu:
    pop dx ax
    
    ret    
sortsMenu ENDP

; sorts the array mainArr from small to large using insertion sort
insertionSort PROC ; finished
    push ax bx cx dx
    
    mov cx, 1
    mov bx, offset mainArr
    outerInsertionLoop:
        mov dx, cx
        mov si, cx
        mov al, mainArr[si]
        innerInsertionLoop:
            dec dx 
            mov si, dx
            cmp al, mainArr[si] 
            JAE noSwapInsertion
            cmp cursorY, 24
            JL swap_insertion
            call clearScreen 
            swap_insertion:
            add bx, dx
            push bx
            inc bx
            push bx
            call swap
            push dx
            inc dx
            push dx
            dec dx
            showMainArr
            call swap_menu
            noSwapInsertion:
            mov bx, offset mainArr
            cmp dx, 0
            JNZ innerInsertionLoop
        inc cx
        cmp cx, len
        JNZ outerInsertionLoop
    mov is_continuos, 0
    
    pop dx cx bx ax
    
    ret 
insertionSort ENDP

;bubble sorts the array mainArr from small to large 
bubbleSort PROC ; finished
	push ax bx cx dx
    
    mov cx, len
    dec cx
    outerBubbleLoop:
        mov dx, cx
        dec dx
        mov bx, offset mainArr
        
        innerBubbleLoop: 
            add bx, dx
            mov al, [bx]
            sub bx, dx
            add bx, cx
            cmp al, [bx]
            JBE dont_swap_bubble
            cmp cursorY, 24
            JL swap_bubble
            call clearScreen
            swap_bubble:
            push bx
            sub bx, cx
            add bx, dx
            push bx
            sub bx, dx
            call swap
            push cx
            push dx
            showMainArr
            call swap_menu
            JMP next_swap_bubble
            dont_swap_bubble:
            sub bx, cx
            
            next_swap_bubble:      
            dec dx
            JGE innerBubbleLoop
            
        dec cx
        cmp cx, 1
        JGE outerBubbleLoop
    mov is_continuos, 0
                       
    pop dx cx bx ax
     
    ret
bubbleSort ENDP

;sets the values of the array by user input
valByUser PROC ; finished
    push ax bx cx dx
    
    mov cx, len
    mov bx, offset mainArr
    getVal:
    	cmp cursorY, 24
    	JL startGetVal
    	call ClearScreen
    	startGetVal:
        mov dx, offset Str3
        mov ah, 9h
        int 21h
        call getNum
        mov [bx], dl
        inc bx
        loop getVal
    
    pop dx cx bx ax
        
    ret    
valByUser ENDP


;lets the user to choose if to run step by step or in continuos mode
swap_menu PROC ;finished
    cmp is_continuos, 1
    JNE startSwapMenu
    call WaitSomeTime
    JMP endMenuNoPush
    startSwapMenu:
    push ax dx

    mov dx, offset Str5
    mov ah, 9
    int 21h
    add cursorY, 2
    mov cursorX, 0
    
    checkKey:
    mov ah, 1
    int 16h
    JE checkKey
    mov ah, 0
    int 16h
    cmp ah, 48h
    JE UpKeyPressed
    cmp ah, 4Dh
    JE endMenu
    JMP checkKey
    
    UpKeyPressed:
    mov is_continuos, 1
    
    endMenu:
    pop dx ax
    endMenuNoPush:
    
    ret    
swap_menu ENDP

;gets a number fr0om the user
getNum PROC ;finished
    push ax bx cx
    
    enterNum:
        mov dx, offset numStr
        mov bx, dx
        mov ah, 0Ah
        int 21h
        call newLine
        xor ah, ah
        mov al, [bx + 1]
        cmp al, 1
        JL inputErr
        add bx, ax
        mov al, [bx + 2]
        cmp al, 13
        JZ okStr
        inputErr: 
        mov dx, offset ErrStr
        mov ah, 9
        int 21h
        add cursorY, 2
        mov cursorX, 0
        JMP enterNum
        okStr:
        mov bx, dx
        xor ch, ch
        mov cl, [bx + 1]
        add bx, 2
        checkNum:
            mov al, [bx]
            cmp al, '0' 
            JL inputErr
            cmp al, '9'
            JG inputErr
            inc bx
            loop checkNum
        mov dx, offset numStr
        mov bx, dx
        xor ch, ch
        mov cl, [bx + 1]
        cmp cl, 3
        JNZ okNum
        cmp byte ptr[bx+2], 32h ; '2' in ascii
        JA inputErr
        JL okNum
        cmp byte ptr[bx+3], 35h ; '5' in ascii
        JA inputErr
        JL okNum
        cmp byte ptr[bx+4], 35h ; '5' in ascii
        JA inputErr
        okNum:
        xor ax, ax
        add bx, 2
        calNum:
            mul ah
            mov dl, al
            mov al, [bx]
            sub al, '0'
            add al, dl
            mov ah, 10
            inc bx
            loop calNum         
        mov dl, al
        xor dh, dh

    pop cx bx ax
        
    ret    
getNum ENDP

;increases the position of the cursor
incCursorPos PROC ;finished
    push ax bx dx

    mov bh, 0
    mov ah, 2
    mov dl, cursorX
    cmp dl, 80 ; cheack if it's the end of the line
    JZ lineFull
    inc cursorX ;inc x poistion of cursor
    inc dl
    JMP changePos
    lineFull:
    mov cursorX, 0 ;set to the start of the next line
    mov dl, 0
    inc cursorY
    cmp cursorY, 25
    JB changePos
    call ClearScreen
    changePos: 
    mov dh, cursorY
    int 10h ;change the cursor position to the new position
    
    endChangePos:
    pop dx bx ax
    
    ret  
incCursorPos ENDP

;sets the position of the cursor by the integers cursorX and cursorY 
changeCursorPos PROC ;finished
    push ax bx dx
    
    mov bh, 0
    mov ah, 2
    mov dh, cursorY
    mov dl, cursorX
    int 10h ;change the cursor position to the position of (cursorX, cursorY)
    
    pop dx bx ax

    ret       
changeCursorPos ENDP    
   
;moves the curser to the next row    
newLine PROC ;finished
    cmp cursorY, 25
    JL moveNextLine
    call ClearScreen
    JMP endNewLine
    moveNextLine:
    inc cursorY
    mov cursorX, 0  ;correct cursor position
    
    call changeCursorPos

    endNewLine:
    ret
newLine ENDP

;prints a number between 0 to 255
printNum PROC ;finished
    printColor equ 4
    numToPrint equ 6
    
    push bp
    
	mov bp, sp

    push ax bx cx dx
    
    mov ax, [bp + numToPrint]
    mov bx, 100
    div bl
    cmp al, 0
    JZ second_digit1 
    mov bl, ah
    push bx
    add al, '0'
    mov bx, [bp + printColor]
    mov cx, 1
    mov ah, 9h                         
    int 10h
    call incCursorPos
    mov ah, 2h
    mov bh, 0
    pop ax
    mov bx, 10
    div bl
    mov bl, ah
    push bx
    JMP second_digit2
    second_digit1:
    mov al, ah
    mov ah, 0
    mov bx, 10
    div bl
    mov bl, ah
    push bx 
    cmp al, 0
    JZ third_digit
    second_digit2:
    add al, '0'
    mov bx, [bp + printColor]
    mov cx, 1
    mov ah, 9h
    int 10h
    call incCursorPos         
    third_digit:     
    pop ax 
    add al, '0'
    mov bx, [bp + printColor]
    mov cx, 1
    mov ah, 9h
    int 10h
    call incCursorPos
    
    pop dx cx bx ax
           
    pop bp
    ret 4
printNum ENDP    

;copy one arr to another arr
copyArr PROC; finished
	len_of_arr equ 8
    copiedArr equ 6 ;start of arr to be copied to
    arrToCopy equ 4 ;start of arr to copy
    
    push bp

    mov bp, sp
    
    push ax bx cx dx
    
    mov cx, [bp + len_of_arr]
    mov bx, [bp + arrToCopy]
    mov dx, [bp + copiedArr] 
    copyByte:
        mov al, [bx]
        xchg bx, dx
        mov [bx], al
        xchg bx, dx
        inc bx
        inc dx
        loop copyByte
    
    pop dx cx bx ax bp

    ret 6
copyArr ENDP

;swaps between two places in the memory	
swap PROC ;finished
    SwapPar1 equ 4
    SwapPar2 equ 6
    
    push bp                                       
    
	mov bp, sp

    push ax bx
    
    mov bx, [bp + swapPar2]
    mov al, byte ptr [bx]
    mov bx, [bp + swapPar1]
    xchg al, byte ptr [bx]
    mov bx, [bp + swapPar2]
    mov byte ptr [bx], al
        
    pop bx ax bp
    
    ret 4
swap ENDP

;shows the array, and prints the numbers that was swapped with different color than the rest of the numbers
showArr PROC ;finished
    SwappedNum1 equ 10
    SwappedNum2 equ 8
    ArrToShow equ 6
    ArrLength equ 4

    curNum equ -10
    curDigit equ  -12
    curColor equ -13
    
    push bp
    
	mov bp, sp

    push ax bx cx dx
    
    sub sp, 5 ; because of local variants

    mov word ptr [bp + curNum], 0
    mov word ptr [bp + curDigit], 100 
    
    mov cx, [bp + ArrLength]
    PrintLoop:
        
        ;choose color
        mov byte ptr [bp + curColor], 15
        mov bx, [bp + curNum]
        cmp bx, [bp + SwappedNum1]
        JNZ SwappedColor2
        mov byte ptr [bp + curColor], 12
        JMP EndSwappedColor
            
        SwappedColor2:
        cmp bx, [bp + SwappedNum2]
        JNZ EndSwappedColor
        mov byte ptr [bp + curColor], 12
            
        ;print number
        EndSwappedColor:
        cmp cursorX, 74
        JL DontMoveLine
        call newLine
        DontMoveLine:
        add bx, [bp + ArrToShow]
        xor ah, ah
        mov al, [bx]
        push ax
        mov al, [bp + curColor]
        push ax
        call printNum
            
            
        cmp cx, 1
        JZ NoComa
        mov dx, offset comaAndSpace
        mov ah, 9h
        int 21h
        add cursorX, 2
        call changeCursorPos
        
        NoComa:
        inc word ptr[bp + curNum]
        loop PrintLoop
    call NewLine
    
    add sp, 5 ; because of local variables
    
    pop dx cx bx ax bp
    
    ret 8
showArr ENDP

;toggles the screen to the end
ClearScreen	PROC ; finished
	push ax bx cx dx
	
	mov cx, 25
    mov ah, 2
	mov dl, 10
	Clear:
	int 21h
	loop Clear
	mov dl, 0
	mov cx, 0
	mov bh, 0
	mov ah, 2
	int 10h
	
	mov cursorY, 0
    mov cursorX, 0
    call changeCursorPos
	
	pop dx cx bx ax

	ret
ClearScreen ENDP

;wait for about 0.16 seconds
WaitSomeTime PROC ; finished
	push ax dx cx bx

	mov ah, 00h
	mov bh, 3
	int 1Ah
	mov bl, dl
	WaitForChange:
	int 1Ah
	cmp  dl, bl
	je   WaitForChange
	mov  bl, dl
	dec bh
	cmp bh, 0
	JG WaitForChange

	pop bx cx dx ax
	ret
WaitSomeTime ENDP

;waits for user to press on anything before resuming the program
systemPause PROC ; finished
    push ax dx
    
    cmp cursorY, 24
    JL start_wait_for_key
    call clearScreen 

    start_wait_for_key:
    mov dx, offset Str7
    mov ah, 9
    int 21h
    add cursorY, 1
    mov cursorX, 0
    
    wait_for_key:
    mov ah, 1
    int 16h
    JE wait_for_key

    pop dx ax
    ret
systemPause ENDP

Start:
    mov dx, data
    mov ds, dx
    mov dx, stac
    mov ss, dx
    call ClearScreen
    call menu
    call systemPause
Stop:
    mov ah, 4Ch
    int 21h
code	ends
end 	Start