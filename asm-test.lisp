(load "asm-program.lisp")

(load "asm-z80.lisp")

(defmacro subroutine (name &rest code)
   `((label ,name) ,@code))

(defparameter 
   test 
   (lzasm-z80 

§bla
      (ld @a @b) 
§bli
      (jmp 'blo)
      (ld @c (+ 1 3)) 
§ble
      (ld @c (+ 1 3)) 
      (ld @c (+ 1 3)) 
§blu
      (ld @a @c)
§blo

(subroutine blubber
      (ld @a @c))
))

(assemble test)
