(load "asm-program.lisp")

(load "asm-z80.lisp")

(defparameter 
    sr01 
    (lzasm-z80 

    ; speech recognition software
    ; polls the input port for incoming signals indicating
    ; a valid recognized word
    ; the index of this word is encoded in ASCII chars and
    ; transmitted via SIO

    ; z80 clock speed : 2 Mhz

    ; mcu ports
    (let ((inport  #x08)
          (chadata #x04)
          (chactrl #x05)
          (chbdata #x06)
          (chbctrl #x07))
    
    (list

§start      (nop)

; init channel a
; baud factor 64
; 28800, 8, n, 2

§initcha    (ld @c chactrl)
            (ld @c #x18)
            (out @c @a)
            (ld @a 4)
            (out @c @a)
            (ld @a #xcc)
            (out @c @a)
            (ld @a 3)
            (out @c @a)
            (ld @a #xc1)
            (out @c @a)
            (ld @a 5)
            (out @c @a)
            (ld @a #x68)
            (out @c @a)
            (ld @a 1)
            (out @c @a)
            (ld @a 0)
            (out @c @a)

; main

            ; wait about 0.5 sec
§m00        (ld @d 150)
§m01        (ld @c 255)         ; 2.5
§m02        (dec @c)            ; 2.00
            (jr 'nz 'm02)         ; 6.00 (z=0), 3.5 (z=1)
            (dec @d)
            (jr 'nz 'm01)

            (ld @a 0)
            (ld @c 0)
            (ld @d 0)

            (in @a inport)

            (ld @d 49)          ; '1'
            (cp 1)
            (jp 'z 'txdchar)
            (ld @d 50)          ; '2'
            (cp 2)
            (jp 'z 'txdchar)
            (ld @d 51)          ; '3'
            (cp 4)
            (jp 'z 'txdchar)
            (ld @d 52)          ; '4'
            (cp 8)
            (jp 'z 'txdchar)
            (ld @d 53)          ; '5'
            (cp 16)
            (jp 'z 'txdchar)
            (ld @d 54)          ; '6'
            (cp 32)
            (jp 'z 'txdchar)
            (ld @d 55)          ; '7'
            (cp 64)
            (jp 'z 'txdchar)
            (ld @d 56)          ; '8'
            (cp 128)
            (jp 'z 'txdchar)

            (jp 'm00)

; send char
; send content of register d via sio

§txdchar    (nop)
§txdchar1   (in @a chactrl)
            (zbit 2 @a)
            (jp 'z 'txdchar1)

            (ld @a @d)
            (out chadata @a)

            (jp 'm00)))))

(assemble-raw sr01 "/tmp/sr01.bin")
