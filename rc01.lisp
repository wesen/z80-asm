(load "asm-program.lisp")

(load "asm-z80.lisp")

(defparameter 
    rc01 
    (lzasm-z80 

    ; remote control software
    ; receives incoming sio messages and then pokes
    ; the corresponding outpout valu into the output port
    ; it then send this value via sio as a confirmation

    ; z80 clock speed: 4 Mhz

    ; mcu ports
    (let ((outport #x08)
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

            ; clear outport
            (ld @c outport)
            (ld @d 0)
            (out @c @d)

; main loop

§m00        (jp 'rcvchar)

§m01        (ld @d 1)
            (cp 49)
            (jp 'z 'pokechar)
            (ld @d 2)
            (cp 50)
            (jp 'z 'pokechar)
            (ld @d 4)
            (cp 51)
            (jp 'z 'pokechar)
            (ld @d 8)
            (cp 52)
            (jp 'z 'pokechar)
            (ld @d 16)
            (cp 53)
            (jp 'z 'pokechar)
            (ld @d 32)
            (cp 54)
            (jp 'z 'pokechar)
            (ld @d 64)
            (cp 55)
            (jp 'z 'pokechar)
            (ld @d 128)
            (cp 56)
            (jp 'z 'pokechar)

            (jp 'm00)
             
; send char
; sends content of register d via sio
§txdchar    (nop)
§txdchar1   (in @a chactrl)
            (zbit 2 @a)
            (jp 'z 'txdchar1)

            (ld @a @d)
            (out chadata @a)

            (jp 'm00)

; receive char from sio
; stores char into accumulator
§rcvchar    (nop)
§rcvchar1   (in @a chactrl)
            (zbit 0 @a)
            (jp 'z 'rcvchar1)

            (in @a chadata)

            (jp 'm01)

; poke char
; pokes content of register d into outport
§pokechar   (ld @c outport)

            (out @c @d)

            (ld @a @d)

            ; wait 50 ms
            (ld @d 50)

§wait01     (ld @c 250)         ; 1.75
§wait02     (dec @c)            ; 1.00
            (jr 'nz 'wait02)    ; 3.00 (z=0), 1.75 (z=1)
            (dec @d)
            (jr 'nz 'wait01)

            ;clear outport
            (ld @c outport)
            (ld @d 0)
            (out @c @d)

            ;send confirmation
            (ld @d @a)
            (jp 'txdchar)))))
 
(assemble-raw rc01 "/tmp/rc01.bin")
