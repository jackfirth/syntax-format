#lang racket/base


;; This module defines a Racket syntax formatter that uses pretty-expressive as
;; the underlying formatting engine. The formatter takes *syntax objects* as
;; input and produces a pretty-expressie document as output. Formatting occurs
;; in steps, similar to macro expansion: each step analyzes a syntax object and
;; returns a *partially formatted* syntax object, which consists of strings
;; interleaved with unformatted syntax objects representing the subforms of the
;; input object. Formatting begins with the outermost syntax object and
;; recursively traverses into unformatted subforms.
;;
;; This doesn't have anywhere close to all of fmt's features. It's just a proof
;; of concept that explores an alternative formatting API that's driven by
;; syntax objects instead of a custom tokenizer and parser. Open questions:
;;
;; - How hard is it to support a wider variety of forms?
;; - What about fixed-width indentation of subforms, like in a function's body?
;; - Rhombus attaches "raw source" to syntax objects via a property, can we use
;;   that in order to recover the exact text of forms? See the "raw source"
;;   property in https://docs.racket-lang.org/rhombus@rhombus/stxobj.html for
;;   details. Matthew mentioned being open to adding this property to objects
;;   produced by the regular Racket reader as well.
;; - What about comments? Pretty important detail, that one.
;; - Memoization.


(require racket/contract)


(provide
 (contract-out
  [syntax-format (-> syntax? doc?)]))


(require guard
         pretty-expressive
         racket/format
         (only-in racket/list add-between)
         racket/match
         racket/string
         syntax/parse
         syntax/parse/lib/function-header)


(module+ test
  (require racket/syntax
           rackunit))


;; An unformatted syntax object, to be formatted later by a subsequent pass.
(struct unformatted-syntax

  (;; A string to concatenate before the syntax object once it's formatted.
   prefix

   ;; The syntax object to format later.
   subform

   ;; A string to concatenate after the syntax object once it's formatted.
   suffix

   ;; Either 'align, indicating this object needs to be wrapped with (align ...)
   ;; after it's formatted (and concatenated with the prefix and suffix), or
   ;; a nonnegative integer indicating how many spaces of indentation to add with (nest ...)
   indentation

   ;; Whether or not to flattin the syntax object (before concatenation and alignment) in order to
   ;; guarantee it has no newlines.
   flatten?)

  #:guard (struct-guard/c string? syntax? string? (or/c 'align exact-nonnegative-integer?) boolean?)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:unformatted-syntax)


(define (unformatted-syntax subform
                            #:prefix [prefix ""]
                            #:suffix [suffix ""]
                            #:indentation [indentation 0]
                            #:flatten? [flatten? #false])
  (constructor:unformatted-syntax prefix subform suffix indentation flatten?))


;; A partially formatted syntax object, consisting of a series of strings and
;; unformatted subforms. We use strings instead of doc? objects so that equals
;; works correctly on instances of this struct.
(struct partially-formatted-syntax (pieces)
  #:transparent
  #:guard (struct-guard/c (listof (or/c string? unformatted-syntax?))))


;; Fully formats a syntax object into a pretty printable document.
(define/contract (syntax-format stx)
  (-> syntax? doc?)
  (define choices (syntax-format-partially stx))
  (define formatted-choices
    (for/list ([choice (in-list choices)])
      (define doc-pieces
        (for/list ([piece (in-list (partially-formatted-syntax-pieces choice))])
          (match piece
            [(? string?) (string->doc piece)]
            [(? unformatted-syntax?)
             (define subform-doc
               (<> (string->doc (unformatted-syntax-prefix piece))
                   (syntax-format (unformatted-syntax-subform piece))
                   (string->doc (unformatted-syntax-suffix piece))))
             (match (unformatted-syntax-indentation piece)
               ['align (align subform-doc)]
               [(? exact-nonnegative-integer? n) (nest n subform-doc)])])))
      (apply <> doc-pieces)))
  (apply alt formatted-choices))


;; Utility for turning a string that may contain newlines into a doc? consisting
;; of concatenated text and hard newlines.
(define/contract (string->doc s)
  (-> string? doc?)
  (v-concat (map text (string-split s "\n" #:trim? #false))))
             

;; The main dispatch table, which chooses what formatters to use based on the shape
;; of the input syntax object. Returns a list of possible formats to choose from,
;; to be combined later with `alt`.
(define/contract (syntax-format-partially stx)
  (-> syntax? (listof partially-formatted-syntax?))
  (syntax-parse stx
    #:datum-literals (define)
    [(~or atom:id atom:number atom:string atom:keyword) (list (format-atom stx))]
    [(define header:function-header body ...) (list (format-function-definition stx))]
    [(form ...) (list (format-one-line-s-exp stx) (format-multi-line-s-exp stx))]
    [_ (list)]))


(define/contract (format-atom stx)
  (-> syntax? partially-formatted-syntax?)
  (syntax-parse stx
    [(~or atom:id atom:number atom:string atom:keyword)
     (partially-formatted-syntax (list (~s (syntax-e #'atom))))]))


(module+ test
  (test-case "format-atom"
    (check-equal? (format-atom #'hello) (partially-formatted-syntax (list "hello")))
    (check-equal? (format-atom #'"hello") (partially-formatted-syntax (list "\"hello\"")))
    (check-equal? (format-atom #'42) (partially-formatted-syntax (list "42")))
    (check-equal? (format-atom #'#:hello) (partially-formatted-syntax (list "#:hello")))))


(define/contract (format-one-line-s-exp stx)
  (-> syntax? partially-formatted-syntax?)
  (define forms (syntax-parse stx [(form ...) (attribute form)]))
  (define unformatted
    (for/list ([form (in-list forms)])
      (unformatted-syntax form #:flatten? #true)))
  (partially-formatted-syntax
   (add-between unformatted (list " ")
                #:before-first (list "(")
                #:after-last (list ")")
                #:splice? #true)))


(module+ test
  (test-case "format-one-line-s-exp"
    (define stx #'(+ 1 2 3))
    (define/with-syntax (plus one two three) stx)
    (check-equal? (format-one-line-s-exp stx)
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


(define/contract (format-multi-line-s-exp stx)
  (-> syntax? partially-formatted-syntax?)
  (guarded-block
    (define forms (syntax-parse stx [(form ...) (attribute form)]))
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
     (append (list "(" unformatted-head " ") unformatted-args (list ")")))))


(module+ test
  (test-case "format-multi-line-s-exp"
    (define stx #'(+ 1 2 3))
    (define/with-syntax (plus one two three) stx)
    (check-equal? (format-multi-line-s-exp stx)
                  (partially-formatted-syntax
                   (list "("
                         (unformatted-syntax #'plus)
                         " "
                         (unformatted-syntax #'one #:suffix "\n" #:indentation 'align)
                         (unformatted-syntax #'two #:suffix "\n" #:indentation 'align)
                         (unformatted-syntax #'three #:indentation 'align)
                         ")")))))


(define/contract (format-function-definition stx)
  (-> syntax? partially-formatted-syntax?)
  (syntax-parse stx
    #:datum-literals (define)
    [((~and def define) header:function-header body ...)
     (partially-formatted-syntax
      (list* "("
             (unformatted-syntax #'def)
             " "
             (unformatted-syntax #'header #:indentation 'align)
             (for/list ([body-stx (in-list (attribute body))])
               (unformatted-syntax body-stx #:prefix "\n" #:indentation 2))))]))


;; Basic integration test of the whole shebang.
(module+ test
  (test-case "syntax-format"
    (define stx #'(+ 1 (+ 2 3 4) 5))
    (check-equal? (pretty-format (syntax-format stx)) "(+ 1 (+ 2 3 4) 5)")
    (check-equal? (pretty-format (syntax-format stx) #:page-width 10)
                  #<<EOS
(+ 1
   (+ 2
      3
      4)
   5)
EOS
                  )
    (check-equal? (pretty-format (syntax-format stx) #:page-width 15)
                  #<<EOS
(+ 1
   (+ 2 3 4)
   5)
EOS
                  )))


(module+ main
  (pretty-print
   (syntax-format
    #'(define (double x)
        (displayln "every day I'm doublin")
        (define (double x) (double x)) (* x 2)))))
