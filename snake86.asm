; a simple hungry snake game written by m05qu1t0
; this program should run on 16bit DOS with intel 8086
; ax, cx, dx are volatile
; bx, di, si are non-volatile
; return value is stored in ax
; and callee cleans the stack
ASSUME CS:CODE,DS:DATA,SS:STACK
DATA SEGMENT
    ; screen 320*200
    ; each pixel is 4*4, so we have a 80*50 sandbox
    ; we use two bytes for position, which represents its index in sandbox, and one byte for direction
    ; left 0ffh
    ; right 1h
    ; down 50h(80 columns per row)
    ; up -50h=0b0h
    direction db 1
    head dw 81
    tail dw 81
    bodyLength dw 1
    ; main body of the game
    ; we store the status of the snake, the food, etc on this sandbox
    ; to be specific, when the snake step on a block, we change its value to current direction
    ; so when snake's tail reaches this point, it'll know where to go
    ; of course after the snake changed its direction we should update that too
    sandbox db 80 dup(WALL)
    db 48 dup(WALL,78 dup(0),WALL)
    db 80 dup(WALL)
    ; last vaild key got
    lastKey db 'd'
    ; random seed
    seed dw 0
    ; enums
    BLACK equ 0
    WHITE equ 15
    GREEN equ 2
    RED equ 4
    FOOD equ 0bh
    WALL equ 0ah
    TRACE equ 0ch ; trace of the tail, we need to clear that after each frame if it exists
    UP equ 0b0h
    DOWN equ 50h
    LEFT equ 0ffh
    RIGHT equ 1h
DATA ENDS
STACK SEGMENT STACK
    db 256 dup(0)
STACK ENDS
CODE SEGMENT
; draw a 4x4 block
; dh: row(0~49), dl: column(0~79), cl: color
DrawBlock:
    push bx
    push di
    push si
    ; get row
    xor ax,ax
    mov al,dh
    shl ax,1
    shl ax,1
    ; get column
    xor bx,bx
    mov bl,dl
    shl bx,1
    shl bx,1
    ; segments per line(320/16=20)
    xor dx,dx
    mov dl,20
    mul dl
    add ax,0a000h
    mov es,ax
    ; draw 4*4 block, row first
    xor si,si
    _DrawBlock1:
        xor di,di
        _DrawBlock2:
            ; row first
            mov es:[bx+di],cl
            inc di
            cmp di,4
            jb _DrawBlock2
        add ax,20
        mov es,ax
        inc si
        cmp si,4
        jb _DrawBlock1
    pop si
    pop di
    pop bx
    ret
; draw a frame of the game
; infact, we draw the sandbox(80*50)
DrawFrame:
    push bx
    push si
    push di
    ; si: row, di: column
    xor si,si
    _DrawFrame1:
        xor di,di
        _DrawFrame2:
            mov ax,80
            mul si
            mov bx,di
            add bx,ax
            mov cl,ds:sandbox[bx]
            test cl,cl
            jz _DrawNothing
            cmp cl,0bh
            jz _DrawRed
            cmp cl,0ah
            jz _DrawWhite
            cmp cl,0ch
            jz _DrawBlank
            jmp _DrawGreen
            _DrawWhite:
                mov cl,15
                jmp _DrawFrame3
            _DrawGreen:
                mov cl,2
                jmp _DrawFrame3
            _DrawRed:
                mov cl,4
                jmp _DrawFrame3
            _DrawBlank:
                xor cl,cl
                mov ds:sandbox[bx],cl
            _DrawFrame3:
                mov dx,si
                shl dx,1
                shl dx,1
                shl dx,1
                shl dx,1
                shl dx,1
                shl dx,1
                shl dx,1
                shl dx,1
                add dx,di
                call DrawBlock
            _DrawNothing:
            inc di
            cmp di,80
            jb _DrawFrame2
        inc si
        cmp si,50
        jb _DrawFrame1
    pop di
    pop si
    pop bx
    ret
; just do nothing to wait for input
Delay:
    push si
    ; si: rank1 counter, ax: rank2 counter
    xor si,si
    inc si
    xor ax,ax
    _Delay1:
        sub ax,1
        sbb si,0
        test ax,ax
        jnz _Delay1
        test si,si
        jnz _Delay1
    pop si
    ret
; get key from buffer, store it to ds:lastKey
; do nothing if buffer is empty
; and we only get the first key, discard the others
GetKey:
    mov ah,1
    int 16h ; detect buffer, quit if empty
    jz _GetKeyRet
    mov ds:lastKey,al ; capture the key
    xor ah,ah
    int 16h ; consume the buffer till its empty
    jmp GetKey
    _GetKeyRet:
        ret
; set current direction, al: key
; if the key is invalid, ignore it
; if the snake twists its body, record it in sandbox
SetDirection:
    push bx
    mov bx,ds:head
    ; bx: head, dl: previous direction
    xor dx,dx
    mov dl,ds:direction
    cmp al,'w'
    jz W
    cmp al,'a'
    jz A
    cmp al,'s'
    jz S
    cmp al,'d'
    jz D
    jmp _SetDirectionRet
    ; ignore if direction contradicts
    W:
        mov ax,0b0h
        add dl,al
        jz _SetDirectionRet
        mov ds:direction,al
        mov ds:sandbox[bx],al
        jmp _SetDirectionRet
    A:
        mov ax,0ffh
        add dl,al
        jz _SetDirectionRet
        mov ds:direction,al
        mov ds:sandbox[bx],al
        jmp _SetDirectionRet
    S:
        mov ax,50h
        add dl,al
        jz _SetDirectionRet
        mov ds:direction,al
        mov ds:sandbox[bx],al
        jmp _SetDirectionRet
    D:
        mov ax,1
        add dl,al
        jz _SetDirectionRet
        mov ds:direction,al
        mov ds:sandbox[bx],al
    _SetDirectionRet:
        pop bx
        ret
; generate a seed for randomization from current time
sRand:
    mov ah,2
    int 1ah ; get current time
    add dx,cx
    mov ds:seed,dx
    ret
; return a random number from 0 to maxsize
; cx: maxsize+1
; multiplier: 13, increment: 1313, module number maxsize+1
Rand:
    mov ax,ds:seed
    mov dx,13
    mul dx
    add ax,1313
    adc dx,0
    div cx
    ; get the reminder
    mov ax,dx
    mov ds:seed,ax
    ret
; return a random food position
GenFood:
    push bx
    push si
    call sRand
    mov cx,78*48
    mov ax,ds:bodyLength
    sub cx,ax
    call Rand
    ; now ax contains the index of next food
    ; we just simply iterate over all the idle block and place it
    ; cx: counter, si: food index, bx: block index
    xor cx,cx
    mov si,ax
    mov bx,81
    _GenFood1:
        xor ax,ax
        mov al,ds:sandbox[bx]
        test ax,ax
        jnz _GenFood3 ; if it is not idle, just ignore it
        cmp cx,si
        jnz _GenFood2 ; idle, but not the block we want
        ; block match
        mov ax,bx
        jmp _GenFoodRet
        _GenFood2:
        inc cx
        _GenFood3:
        inc bx
        cmp bx,49*80
        jb _GenFood1
    _GenFoodRet:
    pop si
    pop bx
    ret
; detect possible collision with food or wall
; the result is stored in al
; 0bh: food
; 0ah: wall
; 0: clean
; 1, ff, b0, 50: snake body
Detect:
    push bx
    mov al,ds:direction
    cbw ; sign extend byte al to ax
    ; get the value in sandbox[bx]
    mov bx,ds:head
    add bx,ax
    mov al,ds:sandbox[bx]
    pop bx
    ret
; eat food
; we replace food with head
; and regenerate food elsewhere
EatFood:
    push bx
    mov al,ds:direction
    cbw ; sign extend
    ; move head onto the food
    mov bx,ds:head
    add bx,ax
    mov ds:head,bx
    mov ds:sandbox[bx],al
    ; increase snake length by 1
    mov ax,ds:bodyLength
    inc ax
    mov ds:bodyLength,ax
    ; generate food elsewhere
    call GenFood
    mov bx,ax
    mov ax,0bh
    mov ds:sandbox[bx],al
    pop bx
    ret
; move head forward
; and move tail according to the record on sandbox
MoveBody:
    push bx
    mov al,ds:direction
    cbw ; sign extend
    ; move head
    mov bx,ds:head
    add bx,ax
    mov ds:head,bx
    mov ds:sandbox[bx],al
    ; move tail
    mov bx,ds:tail
    mov al,ds:sandbox[bx]
    cbw ; sign extend
    ; mov tail forward
    mov dx,bx
    add bx,ax
    mov ds:tail,bx
    ; erase previous record
    mov bx,dx
    mov dx,0ch
    mov ds:sandbox[bx],dl
    pop bx
    ret
; ***THE***
; ***ENTRY***
; ***POINT***
start:
    mov ax,data
    mov ds,ax ; data segment
    mov ax,stack
    mov ss,ax ; stack segment
    mov sp,256
    mov ax,13h
    int 10h ; enable graphic mode
    ; initialize sandbox
    mov bx,ds:head
    xor ax,ax
    mov al,ds:direction
    mov ds:sandbox[bx],al
    call GenFood
    mov bx,ax
    mov ax,0bh
    mov ds:sandbox[bx],al
    ; one frame per loop
    mainloop:
        ; draw the frame
        call DrawFrame
        ; delay(infact, waiting)
        call Delay
        ; get key from buffer
        call GetKey
        mov al,ds:lastKey
        cmp al,'q'
        jz exit
        ; set direction for the snake
        call SetDirection
        ; collision detect
        call Detect
        test al,al
        jz mainloop1
        cmp al,0bh
        jz mainloop2
        jmp exit
        ; move the snake
        mainloop1:
            call MoveBody
            jmp mainloop
        ; eat the food
        mainloop2:
            call EatFood
            jmp mainloop
    ; program terminate
    exit:
        xor ax,ax
        int 16h
        mov ax,03h
        int 10h ; exit the graphic mode
        mov ax,4c00h
        int 21h ; program exit
CODE ENDS
END start