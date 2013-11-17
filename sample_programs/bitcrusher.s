lda r2, [r0] ;load sample
ldi #192
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #128
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #96
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #64
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #48
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #32
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #24
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #16
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #12
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #8
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #6
cmp r2, r1 ; check if r2 is larger than r1
blt =37
ldi #4
cmp r2, r1 ; check if r2 is larger than r1
blt =37
mov r1, r2 ; else, use original value
stb r1, [r0] ; store new value
halt
