; Lextoken types:
;
; number
; 8register, 16register, ix, iy, hl, sp, iv, mr
; ix-indexed, iy-indexed
; hl-indirect, bc-indirect, de-indirect, sp-indirect, imm-indirect
; asmsym, asmsym-indirect

; § is a macro character to define labels
(set-macro-character #\§
   (lambda (s c)
      (let ((sym (read s)))
           (lab sym))))

; @ is a macro character to define registers
(set-macro-character #\@ 
   (lambda (s c) 
      (let ((reg (read s)))
           (case reg
              ('a (make-lextoken :type '8-register :value 'a))
              ('b (make-lextoken :type '8-register :value 'b))
              ('c (make-lextoken :type '8-register :value 'c))
              ('d (make-lextoken :type '8-register :value 'd))
              ('e (make-lextoken :type '8-register :value 'e))
              ('h (make-lextoken :type '8-register :value 'h))
              ('l (make-lextoken :type '8-register :value 'l))
              ('bc (make-lextoken :type '16-register :value 'bc))
              ('de (make-lextoken :type '16-register :value 'de))
              ('hl (make-lextoken :type '16-register :value 'hl))
              ('sp (make-lextoken :type '16-register :value 'sp))
              ('af (make-lextoken :type '16-register :value 'af))
              ('bc2 (make-lextoken :type '16-register2 :value 'bc2))
              ('de2 (make-lextoken :type '16-register2 :value 'de2))
              ('hl2 (make-lextoken :type '16-register2 :value 'hl2))
              ('af2 (make-lextoken :type '16-register2 :value 'af2))
              ('rv (make-lextoken :type 'rv-register :value 'rv))
              ('ix (make-lextoken :type 'ix-register :value 'ix))
              ('iy (make-lextoken :type 'iy-register :value 'iy))
              ('iv (make-lextoken :type 'iv-register :value 'iv))
              ('mr (make-lextoken :type 'mr-register :value 'mr))))))

(defun make-indexed (sign reg val)
   "Create an indexed lextoken"
   (let ((val (if (eq sign '+) (eval val) (- 0 (eval val)))))
        (case (lextoken-type reg)
           ('ix (make-lextoken :type 'ix-indexed :value val))
           ('iy (make-lextoken :type 'iy-indexed :value val)))))

(defun ind-reader (lis)
   "Read indirect adressing modes"
   (let ((first (car lis)))
        (cond 
           ((fixnump first)
              (make-lextoken :type 'imm-indirect :value first))
           ((or (eq '+ first) (eq '- first))
              (apply #'make-indexed lis))
           ((typep first 'lextoken)
              (case (lextoken-value first)
                 ('bc (make-lextoken :type 'bc-indirect))
                 ('hl (make-lextoken :type 'hl-indirect))
                 ('sp (make-lextoken :type 'sp-indirect))
                 ('de (make-lextoken :type 'de-indirect))))

; Is this necessary anymore? check if we somehow need it. Could be
; useful if arithmetic expressions are used with indexing. It should get
; evaled now that most things are functions.
;           ((listp first)
;              (ind-reader (list (eval first))))

           ((symbolp first)
              (make-lextoken :type 'asmsym-indirect :value first)))))
                         
; [ is a macro character for indirect and indexed adressing modes
(set-macro-character #\[
   #'(lambda (s c)
        (let ((lis (read-delimited-list #\] s t)))
             (ind-reader lis))))

; Closing macro character for indirect and indexed adressing modes
(set-macro-character #\] (get-macro-character #\) nil))

; z80 register codes, needed often during instruction assembling
(setq *reg-codes* (make-hash-table :size 11))

(defmacro reg-code (reg)
   `(gethash ,reg *reg-codes*))

(setf (reg-code 'a) 7)
(setf (reg-code 'b) 0)
(setf (reg-code 'c) 1)
(setf (reg-code 'd) 2)
(setf (reg-code 'e) 3)
(setf (reg-code 'h) 4)
(setf (reg-code 'l) 5)
(setf (reg-code 'bc) 0)
(setf (reg-code 'de) 1)
(setf (reg-code 'hl) 2)
(setf (reg-code 'sp) 3)
(setf (reg-code 'af) 3)

; z80 condition codes, needed often during instruction assembling
(setq *con-codes* (make-hash-table :size 11))

(defmacro con-code (reg)
   `(gethash ,reg *con-codes*))

(setf (con-code 'nz) 0)
(setf (con-code 'z) 1)
(setf (con-code 'nc) 2)
(setf (con-code 'c) 3)
(setf (con-code 'pe) 4)
(setf (con-code 'po) 5)
(setf (con-code 'p) 6)
(setf (con-code 'm) 7)

(defun parse-z80 (arg)
  "Parse z80 source code, see the macro definitions and lextoken
  definitions above."
  (cond
    ((null arg) '())
    ((numberp arg)
        (make-lextoken :type 'number
                       :value arg))
    ((symbolp arg)
        (make-lextoken :type 'asmsym :value arg))
    ((typep arg 'lextoken) arg)
    ; lists to embed data into the assembler source code
    ((listp arg) (make-lextoken :type 'list :value arg))
    (T 'XXX)))
  
; Set the assembler parse function to be parse-z80
(setq *parse* #'parse-z80)

(defun lzasm-z80 (insts)
  "Create a new z80 program"
  (make-asm-prog :insts (flatten-list insts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 8-Bit Load group ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(operand ld 1
     "LD r, r'"
     :pass3 (list (logior (logior #b01000000 (lshift (reg-code r1) 3))
                          (reg-code r2)))
     :arg1 8-register 
     :arg2 8-register)

(operand ld 2
     "LD r, n"
     :pass3 (list (logior #b00000110 (lshift (reg-code r1) 3)) r2)
     :arg1 8-register
     :arg2 number)

(operand ld 1
     "LD r, (HL)"
     :pass3 (list (logior #b01000110 (lshift (reg-code r1) 3)))
     :arg1 8-register
     :arg2 hl-indirect)

(operand ld 3
     "LD r, (IX + d)"
     :pass3 (list #b11011101
                  (logior #b01000110 (lshift (reg-code r1) 3))
                  r2)
     :arg1 8-register
     :arg2 ix-indexed) 

(operand ld 3
     "LD r, (IY + d)"
     :pass3 (list #b11111101
                  (logior #b01000110 (lshift (reg-code r1) 3))
                  r2)
     :arg1 8-register
     :arg2 iy-indexed) 

(operand ld 1
     "LD (HL), r"
     :pass3 (list (logior #b01110000 (reg-code r1)))
     :arg1 hl-indirect
     :arg2 8-register)

(operand ld 3
     "LD (IX + d), r"
     :pass3 (list #b11011101
                  (logior #b01110000 (reg-code r1))
                  r2)
     :arg1 ix-indexed
     :arg2 8-register)

(operand ld 3
     "LD (IY + d), r"
     :pass3 (list #b11111101
                  (logior #b01110000 (reg-code r1))
                  r2)
     :arg1 iy-indexed
     :arg2 8-register)

(operand ld 2
     "LD (HL), n"
     :pass3 (list #b00110110 r1)
     :arg1 hl-indirect
     :arg2 number)

(operand ld 4
     "LD (IX + d), n"
     :pass3 (list #b11011101 #b00110110 r1 r2)
     :arg1 ix-indexed
     :arg2 number)

(operand ld 4
     "LD (IY + d), n"
     :pass3 (list #b11111101 #b00110110 r1 r2)
     :arg1 ix-indexed
     :arg2 number)

(operand ld 1
     "LD A, (BC)"
     :pass3 (if (eq r2 'a)
                '(#b00001010))
     :arg1 8-register
     :arg2 bc-indirect)

(operand ld 1
     "LD A, (DE)"
     :pass3 (if (eq r1 'a)
                '(#b00011010))
     :arg1 8-register
     :arg2 de-indirect)

(soperand ld 3
     "LD A, (nn)"
     :pass3 (if (eq r1 'a)
                (list #b00111010 (lower-byte r2) (higher-byte r2)))
     :arg1 8-register
     :arg2 var-indirect)

(operand ld 1
     "LD (BC), A"
     :pass3 (if (eq r2 'a)
                '(#b00000010))
     :arg1 bc-indirect
     :arg2 8-register)

(operand ld 1
     "LD (DE), A"
     :pass3 (if (eq r2 'a)
                '(#b00010010))
     :arg1 de-indirect
     :arg2 8-register)

(soperand ld 3
     "LD (nn), A"
     :pass3 (if (eq r2 'a)
                (list #b00110010 (lower-byte r1) (higher-byte r1)))
     :arg1 var-indirect
     :arg2 8-register)

(operand ld 2
     "LD A, I" 
     :pass3 (if (eq r1 'a)
                '(#b11101101 #b01010111))
     :arg1 8-register
     :arg2 iv-register)

(operand ld 2
     "LD A, R"
     :pass3 (if (eq r1 'a)
                '(#b11101101 #b01011111))
     :arg1 8-register
     :arg2 mr-register)

(operand ld 2
     "LD I, A"
     :pass3 (if (eq r2 'a)
                '(#b11101101 #b01000111))
     :arg1 iv-register
     :arg2 8-register)

(operand ld 2
     "LD R, A"
     :pass3 (if (eq r2 'a)
                '(#b11101101 #b01001111))
     :arg1 mr-register
     :arg2 8-register)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; 16-Bit Load group ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(soperand ld 3
     "LD dd, nn"
     :pass3 (list (logior #b00000001 (shift (reg-code r1) 4))
                  (lower-byte r2) (higher-byte r2))
     :arg1 16-register
     :arg2 variable)

(soperand ld 4
     "LD IX, nn"
     :pass3 (list #b11011101 #b00100001 (lower-byte r2) (higher-byte r2))
     :arg1 ix-register 
     :arg2 variable)

(soperand ld 4
     "LD IY, nn"
     :pass3 (list #b11111101 #b00100001 (lower-byte r2) (higher-byte r2))
     :arg1 iy-register 
     :arg2 variable)

; LD HL, und LD dd kollidieren, bei der ersten Phase LD durch LDHL
; ersetzen, und 3 (eigentliche Groesse) zurueckgeben
(soperand ldhl 3
     "LD HL, (nn)"
     :pass3 (list #b00101010 (lower-byte r2) (higher-byte r2))
     :arg1 16-register 
     :arg2 var-indirect)

(soperand ld 4
     "LD dd, (nn)"
     :pass1 (if (eq r1 'hl)
                (progn 
                   (setf (inst-op inst) 'ldhl)
                   3)
                4)
     :pass3 (list #b11101101 
                  (logior #b01001011 (lshift (reg-code r1) 4)) 
                  (lower-byte r2)
                  (higher-byte r2))
     :arg1 16-register 
     :arg2 var-indirect)     

(soperand ld 4
     "LD IX, (nn)"
     :pass3 (list #b11011101 #b00101010 
                  (lower-byte r2)
                  (higher-byte r2))
     :arg1 ix-register 
     :arg2 var-indirect)     

(soperand ld 4
     "LD IY, (nn)"
     :pass3 (list #b11111101 #b00101010 
                  (lower-byte r2)
                  (higher-byte r2))
     :arg1 iy-register
     :arg2 var-indirect)

(soperand ld 3
     "LD (nn), HL"
     :pass3 (list #b00100010 (lower-byte r1) (higher-byte r1))
     :arg1 var-indirect)

(soperand ld 4
     "LD (nn), dd"
     :pass3 (list #b11101101
                  (logior #b01000011 (lshift (reg-code r2) 4))
                  (lower-byte r1)
                  (higher-byte r1))
     :arg1 var-indirect
     :arg2 16-register)

(soperand ld 4
     "LD (nn), IX"
     :pass3 (list #b11011101 #b00100010
                  (lower-byte r1) (higher-byte r1))
     :arg1 var-indirect
     :arg2 ix-register)

(soperand ld 4
     "LD (nn), IY"
     :pass3 (list #b11111101 #b00100010
                  (lower-byte r1) (higher-byte r1))
     :arg1 var-indirect
     :arg2 iy-register)

(operand ld 1
     "LD SP, HL"
     :pass3 (list #b11111001)
     :arg1 sp-register
     :arg2 hl-register)

(operand ld 2
     "LD SP, IX"
     :pass3 (list #b11011101 #b11111001)
     :arg1 sp-register
     :arg2 ix-register)

(operand ld 2
     "LD SP, IY"
     :pass3 (list #b11111101 #b11111001)
     :arg1 sp-register
     :arg2 iy-register)

(operand zpush 1
     "PUSH qq"
     :pass3 (list #b11000101 (logior (reg-code r1) 4))
     :arg1 16-register)

(operand zpush 2
     "PUSH IX"
     :pass3 (list #b11011101 #b11100101)
     :arg1 ix-register)

(operand zpush 2
     "PUSH IY"
     :pass3 (list #b11111101 #b11100101)
     :arg1 iy-register)

(operand zpop 1
     "POP qq"
     :pass3 (list #b11000001 (logior (reg-code r1) 4))
     :arg1 16-register)

(operand zpop 2
     "POP IX"
     :pass3 (list #b11011101 #b11100001)
     :arg1 ix-register)

(operand zpop 2
     "POP IY"
     :pass3 (list #b11111101 #b11100001)
     :arg1 iy-register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Exchange, Block Transfer, and Search Group ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(operand ex 1
     "EX DE, HL"
     :pass3 (if (and (eq r1 'de) (eq r2 'hl))
                (list #b11101011))
     :arg1 16-register
     :arg2 16-register)

(operand ex 1
     "EX AF, AF'"
     :pass3 (if (and (eq r1 'af) (eq r2 'af2))
                (list #b00001000))
     :arg1 16-register
     :arg2 16-register2)

(operand exx 1
     "EXX"
     :pass3 (list #b11011001))

(operand ex 1
     "EX (SP), HL"
     :pass3 (if (eq r2 'hl) (list #b11100011))
     :arg1 sp-indirect
     :arg2 16-register)

(operand ex 2
     "EX (SP), IX"
     :pass3 (list #b11011101 11100011)
     :arg1 sp-indirect
     :arg2 ix-register)

(operand ex 2
     "EX (SP), IY"
     :pass3 (list #b11111101 11100011)
     :arg1 sp-indirect
     :arg2 iy-register)

(operand ldi 2
     "LDI"
     :pass3 (list #b11101101 #b10100000))

(operand ldir 2
     "LDIR"
     :pass3 (list #b11101101 #b10110000))

(operand ldd 2
     "LDD"
     :pass3 (list #b11101101 #b10101000))

(operand lddr 2
     "LDDR"
     :pass3 (list #b11101101 #b10111000))

(operand cpi 2
     "CPI"
     :pass3 (list #b11101101 #b10100001))

(operand cpir 2
     "CPIR"
     :pass3 (list #b11101101 #b10110001))

(operand cpd 2
     "CPD"
     :pass3 (list #b11101101 #b10101001))

(operand cpdr 2
     "CPDR"
     :pass3 (list #b11101101 #b10111001))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; 8-bit Arithmetic Group ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(operand add 1
     "ADD A, r"
     :pass3 (if (eq r1 'a)
                (list (logior #b10000000 (reg-code r2))))
     :arg1 8-register
     :arg2 8-register)

(operand add 2
     "ADD A, n"
     :pass3 (if (eq r1 'a)
                (list #b11000110 r2))
     :arg1 8-register
     :arg2 number)

(operand add 1
     "ADD A, (HL)"
     :pass3 (if (eq r1 'a)
                (list #b10000110))
     :arg1 8-register
     :arg2 hl-indirect)

(operand add 3
     "ADD A, (IX + d)"
     :pass3 (if (eq r1 'a)
                (list #b11011101 #b10000110 r2))
     :arg1 8-register
     :arg2 ix-indirect)

(operand add 3
     "ADD A, (IY + d)"
     :pass3 (if (eq r1 'a)
                (list #b11111101 #b10000110 r2))
     :arg1 8-register
     :arg2 iy-indirect)

(operand adc 1
     "ADC A, r"
     :pass3 (if (eq r1 'a)
                (list (logior #b10001000 (reg-code r2))))
     :arg1 8-register
     :arg2 8-register)

(operand adc 2
     "ADC A, n"
     :pass3 (if (eq r1 'a)
                (list #b11001110 r2))
     :arg1 8-register
     :arg2 number)

(operand adc 1
     "ADC A, (HL)"
     :pass3 (if (eq r1 'a)
                (list #b10001110))
     :arg1 8-register
     :arg2 hl-indirect)

(operand adc 3
     "ADC A, (IX + d)"
     :pass3 (if (eq r1 'a)
                (list #b11011101 #b10001110 r2))
     :arg1 8-register
     :arg2 ix-indirect)

(operand adc 3
     "ADC A, (IY + d)"
     :pass3 (if (eq r1 'a)
                (list #b11111101 #b10001110 r2))
     :arg1 8-register
     :arg2 iy-indirect)

(operand sub 1
     "SUB r"
     :pass3 (if (eq r1 'a)
                (list (logior #b10010000 (reg-code r2))))
     :arg1 8-register)

(operand sub 2
     "SUB n"
     :pass3 (if (eq r1 'a)
                (list #b11010110 r2))
     :arg1 number)

(operand sub 1
     "SUB (HL)"
     :pass3 (if (eq r1 'a)
                (list #b10010110))
     :arg1 hl-indirect)

(operand sub 3
     "SUB (IX + d)"
     :pass3 (if (eq r1 'a)
                (list #b11011101 #b10010110 r2))
     :arg1 ix-indirect)

(operand sub 3
     "SUB (IY + d)"
     :pass3 (if (eq r1 'a)
                (list #b11111101 #b10010110 r2))
     :arg1 iy-indirect)

(operand sbc 1
     "SBC A, r"
     :pass3 (if (eq r1 'a)
                (list (logior #b10011000 (reg-code r2))))
     :arg1 8-register
     :arg2 8-register)

(operand sbc 2
     "SBC A, n"
     :pass3 (if (eq r1 'a)
                (list #b11011110 r2))
     :arg1 8-register
     :arg2 number)

(operand sbc 1
     "SBC A, (HL)"
     :pass3 (if (eq r1 'a)
                (list #b10011110))
     :arg1 8-register
     :arg2 hl-indirect)

(operand sbc 3
     "SBC A, (IX + d)"
     :pass3 (if (eq r1 'a)
                (list #b11011101 #b10011110 r2))
     :arg1 8-register
     :arg2 ix-indirect)

(operand sbc 3
     "SBC A, (IY + d)"
     :pass3 (if (eq r1 'a)
                (list #b11111101 #b10011110 r2))
     :arg1 8-register
     :arg2 iy-indirect)

(operand zand 1
     "AND r"
     :pass3 (if (eq r1 'a)
                (list (logior #b10100000 (reg-code r2))))
     :arg1 8-register)

(operand zand 2
     "AND n"
     :pass3 (if (eq r1 'a)
                (list #b11100110 r2))
     :arg1 number)

(operand zand 1
     "AND (HL)"
     :pass3 (if (eq r1 'a)
                (list #b10100110))
     :arg1 hl-indirect)

(operand zand 3
     "AND (IX + d)"
     :pass3 (if (eq r1 'a)
                (list #b11011101 #b10100110 r2))
     :arg1 ix-indirect)

(operand zand 3
     "AND (IY + d)"
     :pass3 (if (eq r1 'a)
                (list #b11111101 #b10100110 r2))
     :arg1 iy-indirect)

(operand zor 1
     "OR r"
     :pass3 (if (eq r1 'a)
                (list (logior #b10110000 (reg-code r2))))
     :arg1 8-register)

(operand zor 2
     "OR n"
     :pass3 (if (eq r1 'a)
                (list #b11110110 r2))
     :arg1 number)

(operand zor 1
     "OR (HL)"
     :pass3 (if (eq r1 'a)
                (list #b10110110))
     :arg1 hl-indirect)

(operand zor 3
     "OR (IX + d)"
     :pass3 (if (eq r1 'a)
                (list #b11011101 #b10110110 r2))
     :arg1 ix-indirect)

(operand zor 3
     "OR (IY + d)"
     :pass3 (if (eq r1 'a)
                (list #b11111101 #b10110110 r2))
     :arg1 iy-indirect)

(operand xor 1
     "XOR r"
     :pass3 (if (eq r1 'a)
                (list (logior #b10101000 (reg-code r2))))
     :arg1 8-register)

(operand xor 2
     "XOR n"
     :pass3 (if (eq r1 'a)
                (list #b11101110 r2))
     :arg1 number)

(operand xor 1
     "XOR (HL)"
     :pass3 (if (eq r1 'a)
                (list #b10101110))
     :arg1 hl-indirect)

(operand xor 3
     "XOR (IX + d)"
     :pass3 (if (eq r1 'a)
                (list #b11011101 #b10101110 r2))
     :arg1 ix-indirect)

(operand xor 3
     "XOR (IY + d)"
     :pass3 (if (eq r1 'a)
                 (list #b11111101 #b10101110 r2))
     :arg1 iy-indirect)

; XXX
(operand cp 1
     "CP r"
     :pass3 (list (logior #b10111000 (reg-code r2)))
     :arg1 8-register)

(operand cp 2
     "CP n"
     :pass3 (list #b11111110 r1)
     :arg1 number)

(operand cp 1
     "CP (HL)"
     :pass3 (list #b10111110)
     :arg1 hl-indirect)

(operand cp 3
     "CP (IX + d)"
     :pass3 (list #b11011101 #b10111110 r2)
     :arg1 ix-indirect)

(operand cp 3
     "CP (IY + d)"
     :pass3 (list #b11111101 #b10111110 r2)
     :arg1 iy-indirect)

(operand inc 1
     "INC r"
     :pass3 (list (logior #b00000100 (lshift (reg-code r1) 3)))
     :arg1 8-register)

(operand inc 1
     "INC (HL)"
     :pass3 (list #b00110100)
     :arg1 hl-indirect)

(operand inc 3
     "INC (IX + d)"
     :pass3 (list #b11011101 #b00110100 r2)
     :arg1 ix-indirect)

(operand inc 3
     "INC (IY + d)"
     :pass3 (list #b11111101 #b00110100 r2)
     :arg1 iy-indirect)

(operand dec 1
     "DEC r"
     :pass3 (list (logior #b00000101 (lshift (reg-code r1) 3)))
     :arg1 8-register)

(operand dec 1
     "DEC (HL)"
     :pass3 (list #b00110101)
     :arg1 hl-indirect)

(operand dec 3
     "DEC (IX + d)"
     :pass3 (list #b11011101 #b00110101 r2)
     :arg1 ix-indirect)

(operand dec 3
     "DEC (IY + d)"
     :pass3 (list #b11111101 #b00110101 r2)
     :arg1 iy-indirect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General-purpose Arithmetic and CPU Control Group ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(operand daa 1
     "DAA"
     :pass3 (list #b00100111))

(operand cpl 1
     "CPL"
     :pass3 (list #b00101111))

(operand neg 2
     "NEG"
     :pass3 (list #b11101101 #b01000100))

(operand ccf 1
     "CCF"
     :pass3 (list #b00111111))

(operand scf 1
     "SCF"
     :pass3 (list #b00110111))

(operand nop 1
     "NOP"
     :pass3 (list #b00000000))

(operand halt 1
     "HALT"
     :pass3 (list #b01110110))

(operand di 1
     "DI"
     :pass3 (list #b11110011))

(operand ei 1
     "EI"
     :pass3 (list #b11111011))

; XXX
(operand im 2
     "IM 0, IM 1, IM 2"
     :pass3 (list #b11101101
                  (case r1
                     (0 #b01000110)
                     (1 #b01010110)
                     (2 #b01011110)))
     :arg1 number)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; 16-bit Arithmetic Group ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(operand add 1
     "ADD (HL), ss"
     :pass3 (list (logior #b00001001 (lshift (reg-code r2) 4)))
     :arg1 hl-indirect
     :arg2 16-register)

(operand adc 2
     "ADC (HL), ss"
     :pass3 (list #b11101101 
                  (logior #b01001010 (lshift (reg-code r2) 4)))
     :arg1 hl-indirect
     :arg2 16-register)

(operand sbc 2
     "SBC (HL), ss"
     :pass3 (list #b11101101 
                  (logior #b01000010 (lshift (reg-code r2) 4)))
     :arg1 hl-indirect
     :arg2 16-register)

(operand add 2
     "ADD IX, pp"
     :pass3 (list #b11011101
                  (logior #b00001001 (lshift (reg-code r2) 4)))
     :arg1 ix-register
     :arg2 16-register)

(operand add 2
     "ADD IY, pp"
     :pass3 (list #b11111101
                  (logior #b00001001 (lshift (reg-code r2) 4)))
     :arg1 iy-register
     :arg2 16-register)

(operand inc 1
     "INC ss"
     :pass3 (list (logior #b00000011 (lshift (reg-code r2) r)))
     :arg1 16-register)

(operand inc 2
     "INC IX"
     :pass3 (list #b11011101 #b00100011)
     :arg1 ix-register)

(operand inc 2
     "INC IY"
     :pass3 (list #b11111101 #b00100011)
     :arg1 iy-register)

(operand dec 1
     "DEC ss"
     :pass3 (list (logior #b00001011 (lshift (reg-code r2) r)))
     :arg1 16-register)

(operand dec 2
     "DEC IX"
     :pass3 (list #b11011101 #b00101011)
     :arg1 ix-register)

(operand dec 2
     "DEC IY"
     :pass3 (list #b11111101 #b00101011)
     :arg1 iy-register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Rotate and shift group ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(operand rlca 1
     "RLCA"
     :pass3 (list #b00000111))

(operand rla 1
     "RLA"
     :pass3 (list #b00010111))

(operand rrca 1
     "RRCA"
     :pass3 (list #b00001111))

(operand rra 1
     "RRA"
     :pass3 (list #b00011111))

(operand rlc 2
     "RLC r"
     :pass3 (list #b11001011
                  (logior #b00000000 (reg-code r1)))
     :arg1 8-register)

(operand rlc 2
     "RLC (HL)"
     :pass3 (list #b11001011 #b00000110)
     :arg1 hl-indexed)

(operand rlc 4
     "RLC (IX + d)"
     :pass3 (list #b11011101 #b11001011 r1 #b00000110)
     :arg1 ix-indexed)

(operand rlc 4
     "RLC (IY + d)"
     :pass3 (list #b11111101 #b11001011 r1 #b00000110)
     :arg1 iy-indexed)

(operand rl 2
     "RL r"
     :pass3 (list #b11001011
                  (logior #b00010000 (reg-code r1)))
     :arg1 8-register)

(operand rl 2
     "RL (HL)"
     :pass3 (list #b11001011 #b00010110)
     :arg1 hl-indexed)

(operand rl 4
     "RL (IX + d)"
     :pass3 (list #b11011101 #b11001011 r1 #b00010110)
     :arg1 ix-indexed)

(operand rl 4
     "RL (IY + d)"
     :pass3 (list #b11111101 #b11001011 r1 #b00010110)
     :arg1 iy-indexed)

(operand rrc 2
     "RRC r"
     :pass3 (list #b11001011
                  (logior #b00001000 (reg-code r1)))
     :arg1 8-register)

(operand rrc 2
     "RRC (HL)"
     :pass3 (list #b11001011 #b00001110)
     :arg1 hl-indexed)

(operand rrc 4
     "RRC (IX + d)"
     :pass3 (list #b11011101 #b11001011 r1 #b00001110)
     :arg1 ix-indexed)

(operand rrc 4
     "RRC (IY + d)"
     :pass3 (list #b11111101 #b11001011 r1 #b00001110)
     :arg1 iy-indexed)

; XXX
(operand rr 2
     "RR r"
     :pass3 (list #b11001011
                  (logior #b00011000 (reg-code r1)))
     :arg1 8-register)

(operand rr 2
     "RR (HL)"
     :pass3 (list #b11001011 #b00011110)
     :arg1 hl-indexed)

(operand rr 4
     "RR (IX + d)"
     :pass3 (list #b11011101 #b11001011 r1 #b00011110)
     :arg1 ix-indexed)

(operand rr 4
     "RR (IY + d)"
     :pass3 (list #b11111101 #b11001011 r1 #b00011110)
     :arg1 iy-indexed)

(operand sla 2
     "SLA r"
     :pass3 (list #b11001011
                  (logior #b00100000 (reg-code r1)))
     :arg1 8-register)

(operand sla 2
     "SLA (HL)"
     :pass3 (list #b11001011 #b00100110)
     :arg1 hl-indexed)

(operand sla 4
     "SLA (IX + d)"
     :pass3 (list #b11011101 #b11001011 r1 #b00100110)
     :arg1 ix-indexed)

(operand sla 4
     "SLA (IY + d)"
     :pass3 (list #b11111101 #b11001011 r1 #b00100110)
     :arg1 iy-indexed)

; XXX
(operand sra 2
     "SRA r"
     :pass3 (list #b11001011
                  (logior #b00101000 (reg-code r1)))
     :arg1 8-register)

(operand sra 2
     "SRA (HL)"
     :pass3 (list #b11001011 #b00101110)
     :arg1 hl-indexed)

(operand sra 4
     "SRA (IX + d)"
     :pass3 (list #b11011101 #b11001011 r1 #b00101110)
     :arg1 ix-indexed)

(operand sra 4
     "SRA (IY + d)"
     :pass3 (list #b11111101 #b11001011 r1 #b00101110)
     :arg1 iy-indexed)

(operand srl 2
     "SRL r"
     :pass3 (list #b11001011
                  (logior #b00111000 (reg-code r1)))
     :arg1 8-register)

(operand srl 2
     "SRL (HL)"
     :pass3 (list #b11001011 #b00111110)
     :arg1 hl-indexed)

(operand srl 4
     "SRL (IX + d)"
     :pass3 (list #b11011101 #b11001011 r1 #b00111110)
     :arg1 ix-indexed)

(operand srl 4
     "SRL (IY + d)"
     :pass3 (list #b11111101 #b11001011 r1 #b00111110)
     :arg1 iy-indexed)

(operand rld 2
     "RLD"
     :pass3 (list #b11101101 #b01101111))

(operand rrd 2
     "RRD"
     :pass3 (list #b11101101 #b01100111))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Bit Set, Reset and Test Group ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(operand zbit 2
     "BIT b, r"
     :pass3 (progn (print "bla")(list #b11001011 
                  (logior #b01000000 (lshift (logand r1 7) 3)
                                     (reg-code r2))))
     :arg1 number
     :arg2 8-register)

(operand zbit 2
     "BIT b, (HL)"
     :pass3 (list #b11001011 
                  (logior #b01000110 (lshift (logand r1 7) 3)))
     :arg1 number
     :arg2 hl-register)

(operand zbit 4
     "BIT b, (IX + d)"
     :pass3 (list #b11011101
                  #b11001011
                  r2
                  (logior #b01000110 (lshift (logand r1 7) 3)))
     :arg1 number
     :arg2 ix-indirect)

(operand zbit 4
     "BIT b, (IY + d)"
     :pass3 (list #b11111101
                  #b11001011
                  r2
                  (logior #b01000110 (lshift (logand r1 7) 3)))
     :arg1 number
     :arg2 iy-indirect)

(operand zset 2
     "SET b, r"
     :pass3 (list #b11001011 
                  (logior #b11000000 (lshift (logand r1 7) 3)
                                     (reg-code r2)))
     :arg1 number
     :arg2 8-register)

(operand zset 2
     "SET b, (HL)"
     :pass3 (list #b11001011 
                  (logior #b11000110 (lshift (logand r1 7) 3)))
     :arg1 number
     :arg2 hl-register)

(operand zset 4
     "SET b, (IX + d)"
     :pass3 (list #b11011101
                  #b11001011
                  r2
                  (logior #b11000110 (lshift (logand r1 7) 3)))
     :arg1 number
     :arg2 ix-indirect)

(operand zset 4
     "SET b, (IY + d)"
     :pass3 (list #b11111101
                  #b11001011
                  r2
                  (logior #b11000110 (lshift (logand r1 7) 3)))
     :arg1 number
     :arg2 iy-indirect)

(operand res 2
     "RES b, r"
     :pass3 (list #b11001011 
                  (logior #b10000000 (lshift (logand r1 7) 3)
                                     (reg-code r2)))
     :arg1 number
     :arg2 8-register)

(operand res 2
     "RES b, (HL)"
     :pass3 (list #b11001011 
                  (logior #b10000110 (lshift (logand r1 7) 3)))
     :arg1 number
     :arg2 hl-register)

(operand res 4
     "RES b, (IX + d)"
     :pass3 (list #b11011101
                  #b11001011
                  r2
                  (logior #b10000110 (lshift (logand r1 7) 3)))
     :arg1 number
     :arg2 ix-indirect)

(operand res 4
     "RES b, (IY + d)"
     :pass3 (list #b11111101
                  #b11001011
                  r2
                  (logior #b10000110 (lshift (logand r1 7) 3)))
     :arg1 number
     :arg2 iy-indirect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Jump group ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Im ersten Pass wissen wir noch nicht wie gross und wie weit wir
; springen, im zweiten Pass, wo schon alle Symbole bekannt sind,
; koennen wir einigermassen gut wissen, wie weit wir springen.
; Dementsprechend wir der Befehl "mutiert", so dass er in der dritten
; Phase das richtige Ergebniss bringt. Falls wir zu JR mutieren (JR ist
; kuerzer als JP), werden alle nachfolgende Labels um 1 byte geschiftet.
; Fruehere umgeschriebene Jumps kuemmert es nicht, da sie in der dritten
; Phase erst diesen Wert kriegen werden. Es kann allerdings durchaus
; sein, das ein JMP der als longjump erkannt wurde, durch intensives
; Shiften besser haette zu einem shortjump gemacht werden sollen, sowas
; ist allerdings sehr haarig rauszufinden

(operand jmp 3
     "Virtual JMP operand, gets rewritten to JR or JP"
     :pass2 (let* ((r1v (get-label r1 prog))
                   (jl (- r1v (asm-prog-ilc prog))))
                  (if (and (< jl 129)
                           (> jl -126))
                      (progn 
                         (format t "Shifting~%")
                         (shift-following-labels -1 prog)
                         (setf (inst-op inst) 'jr)
                         3)
                      (progn 
                         (setf (inst-op inst) 'jp)
                         4)))
     :arg1 asmsym)

(soperand jp 3
     "JP nn"
     :pass3 (list #b11000011
                  (lower-byte r1)
                  (higher-byte r1))
     :arg1 variable)

(soperand jp 3
     "JP c, nn"
     :pass3 (list (logior #b11000010 (lshift (con-code r1) 3))
                  (lower-byte r2)
                  (higher-byte r2))
     :arg1 asmsym
     :arg2 variable)

(soperand jr 2
     "JR e"
     :pass3 (list #b00011000
                  (- r1 (asm-prog-ilc prog) 2))
     :arg1 variable)

(soperand jr 2
     "JR c, e"
     :pass3 (case r1
                ('c (list #b00111000 (- r2 (asm-prog-ilc prog) 2)))
                ('nc (list #b00110000 (- r2 (asm-prog-ilc prog) 2)))
                ('z (list #b00101000 (- r2 (asm-prog-ilc prog) 2)))
                ('nz (list #b00100000 (- r2 (asm-prog-ilc prog) 2))))
     :arg1 asmsym
     :arg2 variable)

(soperand jp 1
     "JP (HL)"
     :pass3 (list #b11101001)
     :arg1 hl-indirect)

(soperand jp 2
     "JP (IX)"
     :pass3 (list #b11011101 #b11101001)
     :arg1 ix-indirect)

(soperand jp 2
     "JP (IY)"
     :pass3 (list #b11111101 #b11101001)
     :arg1 iy-indirect)

(soperand djnz 2
     "DJNZ e"
     :pass3 (list #b00010000 (- r1 (asm-prog-ilc prog) 2))
     :arg1 variable)

(soperand call 3
     "CALL nn"
     :pass3 (list #b11001101 (lower-byte r1) (higher-byte r1))
     :arg1 variable)

(soperand call 3
     "CALL c, nn"
     :pass3 (list (logior #b11000100 (lshift (con-code r1) 3))
                  (lower-byte r2) (higher-byte r2))
     :arg1 asmsym
     :arg2 variable)

(operand ret 1
     "RET"
     :pass3 (list #b11001001))

(operand ret 1
     "RET c"
     :pass3 (list (logior #b11000000 (lshift (con-code r1) 3)))
     :arg1 asmsym)

(operand reti 2
     "RETI"
     :pass3 (list #b11101101 #b01001101))

(operand retn 2
     "RETN"
     :pass3 (list #b11101101 #b01000101))

(operand rst 1
     "RST p"
     :pass3 (list (logior #b11000111 (lshift (/ r1 8) 3)))
     :arg1 number)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Input/Output group ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(operand in 2
     "IN A, (n)"
     :pass3 (if (eq r1 'a) (list #b11011011 r2))
     :arg1 8-register
     :arg2 number)

(operand in 2
     "IN r, (C)"
     :pass3 (if (eq r2 'c)
                (list #b11101101 (logior #b01000000 
                                     (lshift (reg-code r1) 3))))
     :arg1 8-register
     :arg2 8-register)

(operand ini 2
     "INI"
     :pass3 (list #b11101101 #b10100010))

(operand inir 2
     "INIR"
     :pass3 (list #b11101101 #b10110010))

(operand ind 2
     "IND"
     :pass3 (list #b11101101 #b10101010))

(operand indr 2
     "INDR"
     :pass3 (list #b11101101 #b10111010))

(operand out 2
     "OUT (n), A"
     :pass3 (if (eq r2 'a) (list #b11010011 r1))
     :arg1 number
     :arg2 8-register)

(operand out 2
     "OUT (C), r"
     :pass3 (if (eq r1 'c)
                (list #b11101101 (logior #b01000001
                                     (lshift (reg-code r2) 3))))
     :arg1 8-register
     :arg2 8-register)

(operand outi 2
     "OUTI"
     :pass3 (list #b11101101 #b10100011))

(operand otir 2
     "OTIR"
     :pass3 (list #b11101101 #b10110011))

(operand outd 2
     "OUTD"
     :pass3 (list #b11101101 #b10101011))

(operand otdr 2
     "OTDR"
     :pass3 (list #b11101101 #b10111011))


