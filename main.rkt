#lang racket/base


;; This library defines a Racket syntax formatter that uses pretty-expressive as
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
  [syntax-format (->* (syntax?) (#:rules (sequence/c formatting-rule?)) doc?)]))


(require guard
         pretty-expressive
         racket/format
         (only-in racket/list add-between empty? partition)
         racket/match
         racket/sequence
         racket/string
         syntax/parse
         syntax/parse/define
         syntax/parse/lib/function-header
         syntax-format/base
         syntax-format/default-formatting-rules)


(module+ test
  (require racket/syntax
           rackunit))


;; Fully formats a syntax object into a pretty printable document.
(define (syntax-format stx #:rules [rules default-formatting-rules])
  (define choices (syntax-format-partially stx #:rules rules))
  (define formatted-choices
    (for/list ([choice (in-list choices)])
      (define doc-pieces
        (for/list ([piece (in-vector (partially-formatted-syntax-pieces choice))])
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


(define (syntax-format-partially stx #:rules [rules default-formatting-rules])
  (define-values (fallback-rules normal-rules) (partition formatting-rule-fallback-rule? rules))
  (define normal-formatting-choices
    (for*/list ([rule normal-rules]
                #:do [(define formatted (formatting-rule-apply rule stx))]
                #:when formatted)
      formatted))
  (if (empty? normal-formatting-choices)
      (for*/list ([rule fallback-rules]
                  #:do [(define formatted (formatting-rule-apply rule stx))]
                  #:when formatted)
        formatted)
      normal-formatting-choices))


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
        (define message "every day I'm doublin")
        (displayln message)
        (define (double x) (double x)) (* x 2)))))
