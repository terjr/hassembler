lda r2, [r0]    ; load newest sample from A, i.e. A[0]
nop             ; wait for completion
stb r2, [r0]    ; store the sample unaltered to B[0]
ldi #1          ; prepare a #1 to address B[1]
stb r2, [r1]    ; store the same sample to B[1]
nop             ; wait for load to happen
halt            ; die
