(load "asm-program.lisp")

(load "asm-z80.lisp")

(defparameter 
    rcv01 
    (lzasm-z80 

    ; rc5 receiver software
    ; polls the input port for incoming rc5 signals
    ; it then send every valid command via SIO
    ; the transmitted command is encoded in ASCII chars

    ; z80 clock speed: 2 Mhz

    ; mcu ports
    (let ((irport  #x08)
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

§m00        (ld @e 0)
            (ld @h 0)
            (ld @l 0)

§m01        (in @a irport)
            (cp 1)
            (jr 'nz 'm01)

            ; wait 0,0001 155 seconds
            (ld @c 144)
§m02        (dec @c)
            (jr 'nz 'm02)

            ; read remote
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc01      (dec @c)
            (jr 'nz 'wrc01)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm03)
            (jp 'm00)
§m03        (cp 1)
            (jr 'nz 'm03a)
            (ld @e 1)

§m03a       (ld @c 109)
§wrc02      (dec @c)
            (jr 'nz 'wrc02)

            ; read toggle
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc03      (dec @c)
            (jr 'nz 'wrc03)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm04)
            (jp 'm00)
§m04        (cp 1)
            (jr 'nz 'm04a)
            (ld @a 2)
            (add @a @e)
            (ld @e @a)

§m04a       (ld @c 109)
§wrc04      (dec @c)
            (jr 'nz 'wrc04)

            ; read s4
            (in @a irport)
            (ld @b @a)
            (ld @c 109)

§wrc05      (dec @c)
            (jr 'nz 'wrc05)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm05)
            (jp 'm00)
§m05        (cp 1)
            (jr 'nz 'm05a)
            (ld @a @h)
            (add @a 16)
            (ld @h @a)

§m05a       (ld @c 109)
§wrc06      (dec @c)
            (jr 'nz 'wrc06)

            ; read s3
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc07      (dec @c)
            (jr 'nz 'wrc07)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm06)
            (jp 'm00)
§m06        (cp 1)
            (jr 'nz 'm06a)
            (ld @a @h)
            (add @a 8)
            (ld @h @a)

§m06a       (ld @c 109)
§wrc08      (dec @c)
            (jr 'nz 'wrc08)

            ; read s2
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc09      (dec @c)
            (jr 'nz 'wrc09)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm07)
            (jp 'm00)
§m07        (cp 1)
            (jr 'nz 'm07a)
            (ld @a @h)
            (add @a 4)
            (ld @h @a)

§m07a       (ld @c 109)
§wrc10      (dec @c)
            (jr 'nz 'wrc10)

            ; read s1
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc11      (dec @c)
            (jr 'nz 'wrc11)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm08)
            (jp 'm00)
§m08        (cp 1)
            (jr 'nz 'm08a)
            (ld @a @h)
            (add @a 2)
            (ld @h @a)

§m08a       (ld @c 109)
§wrc12      (dec @c)
            (jr 'nz 'wrc12)

            ; read s0
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc13      (dec @c)
            (jr 'nz 'wrc13)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm09a)
            (ld @a @h)
            (add @a 1)
            (ld @h @a)

§m09a       (ld @c 109)
§wrc14      (dec @c)
            (jr 'nz 'wrc14)

            ; read c5
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc15      (dec @c)
            (jr 'nz 'wrc15)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm10)
            (jp 'm00)
§m10        (cp 1)
            (jr 'nz 'm10a)
            (ld @a @l)
            (add @a 32)
            (ld @l @a)

§m10a       (ld @c 109)
§wrc16      (dec @c)
            (jr 'nz 'wrc16)

            ; read c4
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc17      (dec @c)
            (jr 'nz 'wrc17)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm11)
            (jp 'm00)
§m11        (cp 1)
            (jr 'nz 'm11a)
            (ld @a @l)
            (add @a 16)
            (ld @l @a)

§m11a       (ld @c 109)
§wrc18      (dec @c)
            (jr 'nz 'wrc18)

            ; read c3
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc19      (dec @c)
            (jr 'nz 'wrc19)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm12)
            (jp 'm00)
§m12        (cp 1)
            (jr 'nz 'm12a)
            (ld @a @l)
            (add @a 8)
            (ld @l @a)

§m12a       (ld @c 109)
§wrc20      (dec @c)
            (jr 'nz 'wrc20)

            ; read c2
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc21      (dec @c)
            (jr 'nz 'wrc21)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm13)
            (jp 'm00)
§m13        (cp 1)
            (jr 'nz 'm13a)
            (ld @a @l)
            (add @a 4)
            (ld @l @a)

§m13a       (ld @c 109)
§wrc22      (dec @c)
            (jr 'nz 'wrc22)

            ; read c1
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc23      (dec @c)
            (jr 'nz 'wrc23)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm14)
            (jp 'm00)
§m14        (cp 1)
            (jr 'nz 'm14a)
            (ld @a @l)
            (add @a 2)
            (ld @l @a)

§m14a       (ld @c 109)
§wrc24      (dec @c)
            (jr 'nz 'wrc24)

            ; read c0
            (in @a irport)
            (ld @b @a)
            (ld @c 109)
§wrc25      (dec @c)
            (jr 'nz 'wrc25)
            (in @a irport)
            (sub @b)
            (cp 0)
            (jr 'nz 'm15)
            (jp 'm00)
§m15        (cp 1)
            (jr 'nz 'm15a)
            (ld @a @l)
            (add @a 1)
            (ld @l @a)

            ; send data
§m15a       (ld @a @e)
            (zand 1)
            (ld @d @a)
§sendcha1   (in @a chactrl)
            (zbit 2 @a)
            (jp 'z 'sendcha1)

            (ld @a @b)
            (out chadata @a)

            (ld @a @e)
            (zand 2)
            (cp 2)
            (jr 'nz 'm15b)
            (ld @a 1)
§m15b       (ld @d @a)
§sendcha2   (in @a chactrl)
            (zbit 2 @a)
            (jp 'z 'sendcha2)

            (ld @a @b)
            (out chadata @a)

            (ld @a @h)
            (ld @d @a)
§sendcha3   (in @a chactrl)
            (zbit 2 @a)
            (jp 'z 'sendcha3)

            (ld @a @d)
            (out chadata @a)

            (ld @a @l)
            (ld @d @a)
§sendcha4   (in @a chactrl)
            (zbit 2 @a)
            (jp 'z 'sendcha4)

            (ld @a @d)
            (out chadata @a)

            (jp 'm00)))))

(assemble-raw rcv01 "/tmp/rcv01.bin")
