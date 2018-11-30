#lang racket

;; 'todo' syntax implementation. You do not need to modify, although
;; you can remove once everything has been implemented, if you wish.
(define-struct (exn:fail:not-implemented exn:fail) ())

(define-syntax-rule (todo)
  (raise (make-exn:fail:not-implemented
           (string-append
             "Not implemented: "
             (symbol->string (caar
                               (continuation-mark-set->context
                                 (current-continuation-marks)))))
           (current-continuation-marks))))

;; Convert an input string in the lambda calculus to our internal
;; representation using lists. Note that either λ, #, or \ can be
;; used to input a lambda.
;;
;; Example: (parse "λx.x(λy.x)")
;;    ==>   '(abstraction
;;             x
;;             (application
;;               x
;;               (abstraction y x)))
(define (parse input-string)
  (define (variable? t)
    (cond
      [(member t '(lambda dot lparen rparen end)) #f]
      [(symbol? t) #t]
      [else #f]))

  (define (term? t)
    (or (variable? t) (list? t)))

  (define (parse-iter parse-stack tokens)
    (define (shift-tok tok)
      (parse-iter (cons tok parse-stack) (cdr tokens)))
    (match* (parse-stack (car tokens))
      [((list-rest 'rparen (? term? term) 'lparen stack) _)
       ;; Reduce by Term -> ( Term )
       (parse-iter (cons term stack) tokens)]
      [((list-rest (? term? n) (? term? m) stack) _)
       ;; Reduce by Application -> Term Term
       (parse-iter (cons `(application ,m ,n) stack) tokens)]
      [((list-rest (? term? term) 'dot (? variable? var) 'lambda stack)
        (or #\) 'end))
       ;; Reduce by Abstraction -> λ Variable . Term
       (parse-iter (cons `(abstraction ,var ,term) stack) tokens)]
      [((list (? term? t)) 'end)
       ;; Done, valid parse
       t]
      [(_ 'end) (error "Incomplete parse")]
      [(_ (? char-whitespace?))
       ;; Ignore whitespace
       (parse-iter parse-stack (cdr tokens))]
      ;; Shift
      [(_ (or #\λ #\\ #\#)) (shift-tok 'lambda)]
      [(_ #\.) (shift-tok 'dot)]
      [(_ #\() (shift-tok 'lparen)]
      [(_ #\)) (shift-tok 'rparen)]
      [(_ chr) (shift-tok (string->symbol (string chr)))]))

  (parse-iter '() (append (string->list input-string) '(end))))

;; Convert a list representation of a term to a string (the opposite
;; of the parse function)
;;
;; Example: (deparse '(abstraction x x))
;;    ==>   "λx.x"
(define (deparse term)
  (match term
    [(? symbol? var) (symbol->string var)]
    [`(abstraction ,var ,term)
      (format "λ~a.~a" (deparse var) (deparse term))]
    [`(application ,m ,n)
      (format (string-append
                (match m
                  [`(abstraction ,_ ,_) "(~a)"]
                  [_ "~a"])
                (if (symbol? n)
                  "~a"
                  "(~a)"))
              (deparse m) (deparse n))]))

;; show-alpha-rename is the default printer for showing that an alpha
;; rename has occured. Your code should *not* call this code directly,
;; but instead use the notify-rename parameter, like this example:
;;
;;  Suppose you alpha-rename a to b in the term '(abstraction a a). Then,
;;  your code should make this call to notify the grader that you have
;;  made an alpha rename:
;;
;;      ((notify-rename) 'a 'b '(abstraction a a))
;;
;;  The notify-rename is in parenthesis there since it's a dynamically
;;  bound variable. In Racket, this is implemented as a function which
;;  returns that variable's value.
(define (show-alpha-rename orig-var new-var term)
  (printf "α ==> ~a for ~a in ~a~%" orig-var new-var (deparse term)))

(define notify-rename (make-parameter show-alpha-rename))

;; The helper below will help you generate generate a new variable which does
;; not cause a naming conflict with any of the conflicting terms. You'll want
;; to used this when you need to alpha-rename a variable to find a new
;; variable name.
;;
;; Example: (new-variable '(a (abstraction a b) (abstraction c c)))
;;    ==>   'c
;;
;; Note: this function depends on your contains-free-var? implementation in
;; order to work properly.
(define alphabet (map (λ (c) (string->symbol (string c)))
                      (string->list "abcdefghijklmnopqrstuvwxyz")))

(define (new-variable conflicting-terms [alphabet alphabet])
  (cond
    [(null? alphabet) (gensym "var-")]
    [(not (ormap
            (λ (t) (contains-free-var? (car alphabet) t))
            conflicting-terms))
     (car alphabet)]
    [else (new-variable conflicting-terms (cdr alphabet))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    COMPLETE THE FUNCTIONS BELOW                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The function contains-free-var? should return #t if the term contains the
;; variable `var` as a free variable, or #f otherwise.
;;
;; Example: (contains-free-var? 'a '(abstraction a b))
;;    ==>   #f
;; Example: (contains-free-var? 'b '(abstraction a b))
;;    ==>   #t
;; Example: (contains-free-var? 'a 'a)
;;    ==>   #t
;; Example: (contains-free-var? 'b '(abstraction a (abstraction b b)))
;;    ==>   #f
;; Example: (contains-free-var? 'c '(abstraction a (abstraction a b)))
;;    ==>   #f
;; Example: (contains-free-var? 'c '(abstraction a (abstraction a c)))
;;    ==>   #t
;; Example: (contains-free-var? 'a '(application a (abstraction a c)))
;;    ==>   #t
;; Example: (contains-free-var? 'c '(application a (abstraction a c)))
;;    ==>   #t
(define (contains-free-var? var term)
  (todo))

;; The function substitute-var takes parameters:
;;    - var: a variable to replace
;;    - term: the term to replace that variable in
;;    - replacement-term: the term to replace that variable with
;;
;; Remember that the variable may exist under multiple bindings in the term,
;; so you must not replace the variables inside of an abstraction with the
;; same variable name.
;;
;; In particular, remember that the substitution operation is not safe. If
;; the conditions are met that an alpha-rename is needed (see the slides),
;; then your code must make that alpha-rename. In addition, your code MUST
;; use the notify-rename parameter (see above) to notify the grader that
;; you have made an alpha-rename.
;;
;; **Your code must not alpha-rename unless absolutely necessary!!**
;;
;; Hint: you will need to use your contains-free-var? as one of the conditions
;; to test if an alpha-rename is needed.
;;
;; Example: (substitute-var 'x '(application x x) 'y)
;;    ==>   '(application y y)
;; Example: (substitute-var 'x '(abstraction z x) 'y)
;;    ==>   '(abstraction z y)
;;
;; In this example, the inner x is bound by the abstaction, and should not be
;; substituted for y.
;;
;; Example: (substitute-var 'x '(abstraction x x) 'y)
;;    ==>   '(abstraction x x)
;;
;; In this example, we have met the conditions necessary to do an alpha
;; rename, so we use the notify-rename parameter and rename the variable
;; in the abstraction:
;;
;; Example: (substitute-var 'x '(abstraction y x) 'y)
;;          α ==> y for a in λy.x
;;    ==>   '(abstraction a y)
;; Example: (substitute-var 'x '(abstraction y (application y x)) 'y)
;;          α ==> y for a in λy.yx
;;    ==>   '(abstraction a (application a y))
;;
;; More examples, for your enjoyment:
;;
;; Example: (deparse (substitute-var 'a (parse "#b.abcdefghijklmnopqrstuvwx") '(application b b)))
;;          α ==> b for y in λb.abcdefghijklmnopqrstuvwx
;;    ==>   "λy.bbycdefghijklmnopqrstuvwx"
;; Example: (deparse (substitute-var 'a (parse "#b.abcdefghijklmnopqrstuvwx") '(abstraction b b)))
;;    ==>   "λb.(λb.b)bcdefghijklmnopqrstuvwx"
;;
;; NOTE: Please create some of your own test cases for this funciton. The
;; above test cases are not enough to test this function's non-trivial
;; functionality. We *will* be running many more than the above tests.
(define (substitute-var var term replacement-term)
  (todo))

;; Preform a single reduction on a term. This should find the left-most,
;; outer-most redex and preform the reduction (using substitute-var).
;;
;; Return #f when there is no left-most, outer-most redex.
;;
;; When applied iteratively (as the main function does for you), this
;; becomes a lazy beta-reducer.
;;
;; Example: (single-reduction '(application (abstraction x x) (abstraction y y)))
;;    ==>   '(abstraction y y)
;; Example: (single-reduction '(application (abstraction x (abstraction z (application x z))) (abstraction y y)))
;;    ==>   '(abstraction z (application (abstraction y y) z))
;; Example: (single-reduction 'x)
;;    ==>   #f
;; Example: (single-reduction '(application (application a a) (application (abstraction x x) a)))
;;    ==>   '(application (application a a) a)
;; Example: (single-reduction '(application (application a a) (application (application x x) a)))
;;    ==>   #f
;;
;; Once again, you should make some of your own test cases here, and use the
;; main function to test as well.
(define (single-reduction term)
  (todo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This main function was written for you...                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main-loop)
  (display "λ> ")
  (let ([user-input (read-line)])
    (when (not (eq? user-input eof))
      (let ([parsed-input (with-handlers
                            ([exn:fail?
                               (λ (e)
                                 (eprintf "parse error: ~a~%" (exn-message e))
                                 #f)])
                            (parse user-input))])
        (when parsed-input
          (define (reduce term)
            (let ([reduction (single-reduction term)])
              (when reduction
                (printf "β ==> ~a~%" (deparse reduction))
                (reduce reduction))))
          (printf "INPUT ~a~%" (deparse parsed-input))
          (reduce parsed-input)))
      (main-loop))))

(module* main #f
  (main-loop))
