(defpackage lzasm
  (:use common-lisp))

; Helpers
(defun lshift (val shift)
   (* (expt 2 shift) val))

(defun flatten-list (lis)
   (cond
      ((null lis) lis)
      ((consp (car lis))
       (append (flatten-list (car lis))
               (flatten-list (cdr lis))))
      (t (cons (car lis)
                  (flatten-list (cdr lis))))))

(defun lower-byte (bla)
   "Get the lower byte of the integer"
   (logand bla #b11111111))

(defun higher-byte (bla)
   "Get the higher byte of the integer"
   (floor bla 256))

(defun recsubstitute (dest source lis)
   (cond
      ((equal lis source)
         dest)
      ((listp lis)
         (cond
            ((null lis) lis)
            (T (mapcar (lambda (lis) (recsubstitute dest source lis))
                       lis))))
      (T lis)))

; Lexical token

(defstruct lextoken
  "Lexical token"
  (type 'null)
  (value nil))

(defparameter 
  *optable* (make-hash-table :size 200 :test #'equal)
  "Opcode table for the assembler")

(defstruct opcode
   "Assembler opcode"
   (documentation "")
   (op nil)
   (arg1 'null)
   (arg2 'null)
   (arg3 'null)
   (size 0)
   (pass1 nil)
   (pass2 nil)
   (pass3 nil))

(defun hash-opcode (opc)
  "Enter an opcode into the assembler opcode table"
  (setf (gethash (list (opcode-op opc)
                       (opcode-arg1 opc)
                       (opcode-arg2 opc)
                       (opcode-arg3 opc))
                 *optable*)
        opc))

(defun get-opc (opc &optional (arg1 'null) (arg2 'null) (arg3 'null))
  "Get the hash-table entry for the corresponding opcode"
  (gethash (list opc arg1 arg2 arg3) *optable*))

(defstruct inst
  "Assembler instruction"
  (op nil)
  (arg1 (make-lextoken :type 'null))
  (arg2 (make-lextoken :type 'null))
  (arg3 (make-lextoken :type 'null)))

(defun get-inst (inst)
  "Get the hash-table entry for the corresponding instruction"
  (get-opc (inst-op inst) 
           (lextoken-type (inst-arg1 inst))
           (lextoken-type (inst-arg2 inst))
           (lextoken-type (inst-arg3 inst))))

(defstruct asm-prog
  "Assembler program, made of a list of instructions, a list of
   functions created by the instructions a symbol table, and an ilc"
  (insts nil)
  ;(ifuncs nil)
  (labs nil)
  (ltable (make-hash-table :size 30 :test #'eq))
  (ilc 0))

(defun get-label (label prog)
  "Get the value of a label from the program label hashtable"
  (gethash label (asm-prog-ltable prog)))

(defun shift-label (label value prog)
  "Shift a label from program prog by the specified value"
  (let ((bla (get-label label prog)))
       (setf (gethash label (asm-prog-ltable prog))
             (+ bla value))))

(defun shift-following-labels (value prog)
  "Shift all the following labels stored in the label list of the
   specified program"
  (mapcar (lambda (label)
             (shift-label label value prog))
          (asm-prog-labs prog)))

(defun assemble (prog &optional
                      (func (lambda (byte) 
                               (format t "~D: Word: ~b~%"
                                         (asm-prog-ilc prog) byte))))
  "Assemble an assembler program, go through pass 1, pass2 and then call
   all the functions returned by the instruction."
  (progn

    ; Pass 1
    (setf (asm-prog-ilc prog) 0)
    (mapcar #'(lambda (x) 
                 (let ((opc (get-inst x)))
             ;          (print "pass1")
             ;          (print x)
                      (setf (asm-prog-ilc prog)
                            (+ (asm-prog-ilc prog)
                               (funcall (opcode-pass1 opc) 
                                        x prog)))))
            (asm-prog-insts prog))
    (setf (asm-prog-labs prog) (nreverse (asm-prog-labs prog)))

    ; Pass 2
    (setf (asm-prog-ilc prog) 0)
    (mapcar #'(lambda (x) 
                 (let ((opc (get-inst x)))
             ;          (print "pass2")
             ;          (print x)
                      (setf (asm-prog-ilc prog)
                            (+ (asm-prog-ilc prog)
                               (funcall (opcode-pass2 opc) 
                                        x prog)))))
            (asm-prog-insts prog))

    ; Pass 3
    ; Call the whole program instruction function list
    (setf (asm-prog-ilc prog) 0)
    (mapcar #'(lambda (x)
                 (let* ((opc (get-inst x))
                        (lis (funcall (opcode-pass3 opc) x prog)))
                       (print "pass3")
                       (print x)
                       (mapcar func lis)
                (setf (asm-prog-ilc prog)
                      (+ (asm-prog-ilc prog) (list-length lis)))))
            (asm-prog-insts prog))))

(defun assemble-raw-byte (prog file)
  (let ((bos (open file :direction :output
                        :if-does-not-exist :create
                        :element-type 'unsigned-byte
                        :if-exists :overwrite)))
       (if bos
           (progn 
             (assemble prog (lambda (byte) (write-byte byte bos)))
             (close bos)
             (format t "Program written to file ~S~%" file))
           (format t "Program could not be written to file ~S~%" file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Parse function for the appropriate assembler type
(defparameter *parse* nil 
   "Parse function for the appropriate assembler type")

(defmacro operand (sym size documentation
                   &key (pass1 size) 
                        (pass2 size)
                        (pass3 nil) 
                        (arg1 'null)
                        (arg2 'null)
                        (arg3 'null))

  "Define an operand, enter it into the assembler hash table, and call
   the appropriate macro to define the function (op0, op1, op2, op3)"

  (let ((opc (make-opcode 
                :documentation documentation
                :op sym 
                :size size 
                :pass1 (eval 
                   `(lambda (inst prog) 
                      (let ((r1 (lextoken-value (inst-arg1 inst))) 
                            (r2 (lextoken-value (inst-arg2 inst)))
                            (r3 (lextoken-value (inst-arg3 inst))))
                           ,pass1)))
                :pass2 (eval 
                   `(lambda (inst prog) 
                      (let ((r1 (lextoken-value (inst-arg1 inst))) 
                            (r2 (lextoken-value (inst-arg2 inst)))
                            (r3 (lextoken-value (inst-arg3 inst))))
                           ,pass2)))
                :pass3 (eval 
                   `(lambda (inst prog) 
                      (let ((r1 (lextoken-value (inst-arg1 inst))) 
                            (r2 (lextoken-value (inst-arg2 inst)))
                            (r3 (lextoken-value (inst-arg3 inst))))
                           ,pass3)))
                :arg1 arg1
                :arg2 arg2
                :arg3 arg3)))

       (hash-opcode opc))

  `(defun ,sym (&optional (arg1 (make-lextoken :type 'null))
                          (arg2 (make-lextoken :type 'null))
                          (arg3 (make-lextoken :type 'null)))
      (let ((bla1 (funcall *parse* arg1))
            (bla2 (funcall *parse* arg2))
            (bla3 (funcall *parse* arg3)))
           (make-inst :op ',sym :arg1 bla1
                                :arg2 bla2
                                :arg3 bla3))))

; Es ist immer ein bisschen laestig, alles doppelt hinzuschreiben, wenn
; ein Opcode eine Adresse als Argument annimmt (symbol oder numerischer
; Wert). Die Soperand Macro ersetzt sowas automatisch.
(defmacro soperand (sym size documentation
                    &key (pass1 size) 
                         (pass2 size)
                         (pass3 nil) 
                         (arg1 'null)
                         (arg2 'null)
                         (arg3 'null))

  "Define an operand, enter it into the assembler hash table, and call
   the appropriate macro to define the function (op0, op1, op2, op3).
   the operand takes one or more arguments of type 'variable or
   'var-indirect, and 2 operands are generated. One where the variable
   arguments are used as immediate values, one where there are symbols."

   (let ((rplis nil)
         (arglis (nsubstitute 'asmsym-indirect 'var-indirect
                 (nsubstitute 'asmsym 'variable 
                              (list arg1 arg2 arg3))))
         (arglis1 (nsubstitute 'imm-indirect 'var-indirect
                 (nsubstitute 'number 'variable 
                              (list arg1 arg2 arg3))))
         (pass1b pass1)
         (pass2b pass2)
         (pass3b pass3))
        
        (mapcar 

          #'(lambda (blo blob bloc)
             (if (or (eq blo 'variable)
                     (eq blo 'var-indirect))
                 (progn 
                    (setf pass1b (recsubstitute bloc blob pass1b))
                    (setf pass2b (recsubstitute bloc blob pass2b))
                    (setf pass3b (recsubstitute bloc blob pass3b))
                    (push `(,bloc (get-label ,blob prog)) rplis))))

          (list arg1 arg2 arg3)
          (list 'r1 'r2 'r3)
          (list 'r1b 'r2b 'r3b))
               
        `(progn 
            (operand ,sym ,size ,documentation
                     :pass1 ,pass1
                     :pass2 ,pass2
                     :pass3 (let ,rplis
                                 ,pass3b)
                     :arg1 ,(car arglis)
                     :arg2 ,(cadr arglis)
                     :arg3 ,(caddr arglis))
            (operand ,sym ,size ,documentation
                     :pass1 ,pass1
                     :pass2 ,pass2
                     :pass3 ,pass3
                     :arg1 ,(car arglis1)
                     :arg2 ,(cadr arglis1)
                     :arg3 ,(caddr arglis1)))))

(operand lab 0
     "Label in the assembler program"
     :pass1 
        (progn
           (push r1 (asm-prog-labs prog))
           (setf (gethash r1 (asm-prog-ltable prog))
                 (asm-prog-ilc prog))
        0)
     :pass3
        (progn 
           (format t "Label ~S at ILC ~D.~%" r1 (asm-prog-ilc prog))
           (get-label r1 prog)
           '())
     :arg1 asmsym)
