#lang racket/base


(provide default-formatting-rules)


(require guard
         racket/format
         racket/list
         syntax/parse
         syntax/parse/lib/function-header
         syntax-format/base)


(module+ test
  (require racket/syntax
           rackunit))


(define-formatting-rule format-atom
  [(~or atom:id atom:number atom:string atom:keyword)
   (partially-formatted-syntax (list (~s (syntax-e #'atom))))])


(module+ test
  (test-case "format-atom"
    (check-equal? (formatting-rule-apply format-atom #'hello)
                  (partially-formatted-syntax (list "hello")))
    (check-equal? (formatting-rule-apply format-atom #'"hello")
                  (partially-formatted-syntax (list "\"hello\"")))
    (check-equal? (formatting-rule-apply format-atom #'42)
                  (partially-formatted-syntax (list "42")))
    (check-equal? (formatting-rule-apply format-atom #'#:hello)
                  (partially-formatted-syntax (list "#:hello")))))


(define-formatting-rule format-one-line-s-exp
  #:fallback-rule
  [(form ...)
   (define forms (attribute form))
   (define unformatted
     (for/list ([form (in-list forms)])
       (unformatted-syntax form #:flatten? #true)))
   (partially-formatted-syntax
    (add-between unformatted (list " ")
                 #:before-first (list "(")
                 #:after-last (list ")")
                 #:splice? #true))])


(module+ test
  (test-case "format-one-line-s-exp"
    (define stx #'(+ 1 2 3))
    (define/with-syntax (plus one two three) stx)
    (check-equal? (formatting-rule-apply format-one-line-s-exp stx)
                  (partially-formatted-syntax
                   (list "("
                         (unformatted-syntax #'plus #:flatten? #true)
                         " "
                         (unformatted-syntax #'one #:flatten? #true)
                         " "
                         (unformatted-syntax #'two #:flatten? #true)
                         " "
                         (unformatted-syntax #'three #:flatten? #true)
                         ")")))))


(define-formatting-rule format-multi-line-s-exp
  #:fallback-rule
  [(form ...)
   (guarded-block
     (define forms (attribute form))
     (guard-match (cons head-form arg-forms) forms #:else (partially-formatted-syntax (list "()")))
     (define unformatted-head (unformatted-syntax head-form))
     (define arg-count (length arg-forms))
     (guard (positive? arg-count) #:else (partially-formatted-syntax (list "(" unformatted-head ")")))
     (define unformatted-args
       (for/list ([arg (in-list arg-forms)]
                  [i (in-naturals 1)])
         (define arg-suffix (if (equal? i arg-count) "" "\n"))
         (unformatted-syntax arg #:suffix arg-suffix #:indentation 'align)))
     (partially-formatted-syntax
      (append (list "(" unformatted-head " ") unformatted-args (list ")"))))])


(module+ test
  (test-case "format-multi-line-s-exp"
    (define stx #'(+ 1 2 3))
    (define/with-syntax (plus one two three) stx)
    (check-equal? (formatting-rule-apply format-multi-line-s-exp stx)
                  (partially-formatted-syntax
                   (list "("
                         (unformatted-syntax #'plus)
                         " "
                         (unformatted-syntax #'one #:suffix "\n" #:indentation 'align)
                         (unformatted-syntax #'two #:suffix "\n" #:indentation 'align)
                         (unformatted-syntax #'three #:indentation 'align)
                         ")")))))


(define-formatting-rule format-one-line-variable-definition
  #:datum-literals (define)
  [((~and def define) id:id expr:expr)
   (partially-formatted-syntax
    (list "("
          (unformatted-syntax #'def #:flatten? #true)
          " "
          (unformatted-syntax #'id #:flatten? #true)
          " "
          (unformatted-syntax #'expr #:flatten? #true)
          ")"))])

(define-formatting-rule format-multi-line-variable-definition
  #:datum-literals (define)
  [((~and def define) id:id expr:expr)
   (partially-formatted-syntax
    (list "("
          (unformatted-syntax #'def #:flatten? #true)
          " "
          (unformatted-syntax #'id)
          (unformatted-syntax #'expr #:prefix "\n" #:indentation 2)
          ")"))])

(define-formatting-rule format-function-definition
  #:datum-literals (define)
  [((~and def define) header:function-header body ...)
   (partially-formatted-syntax
    (list* "("
           (unformatted-syntax #'def)
           " "
           (unformatted-syntax #'header #:indentation 'align)
           (for/list ([body-stx (in-list (attribute body))])
             (unformatted-syntax body-stx #:prefix "\n" #:indentation 2))))])

(define default-formatting-rules
  (list format-atom
        format-one-line-s-exp
        format-multi-line-s-exp
        format-one-line-variable-definition
        format-multi-line-variable-definition
        format-function-definition))
