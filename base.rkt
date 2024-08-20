#lang racket/base


(require racket/contract/base)


(provide
 define-formatting-rule
 (contract-out
  [unformatted-syntax? (-> any/c boolean?)]
  [unformatted-syntax
   (->* (syntax?)
        (#:prefix string?
         #:suffix string?
         #:indentation (or/c exact-nonnegative-integer? 'align)
         #:flatten? boolean?)
        unformatted-syntax?)]
  [unformatted-syntax-subform (-> unformatted-syntax? syntax?)]
  [unformatted-syntax-prefix (-> unformatted-syntax? (and/c string? immutable?))]
  [unformatted-syntax-suffix (-> unformatted-syntax? (and/c string? immutable?))]
  [unformatted-syntax-indentation (-> unformatted-syntax? (or/c exact-nonnegative-integer? 'align))]
  [unformatted-syntax-flatten? (-> unformatted-syntax? boolean?)]
  [partially-formatted-syntax? (-> any/c boolean?)]
  [partially-formatted-syntax
   (-> (sequence/c (or/c string? unformatted-syntax?)) partially-formatted-syntax?)]
  [partially-formatted-syntax-pieces
   (-> partially-formatted-syntax?
       (vectorof (or/c (and/c string? immutable?) unformatted-syntax?) #:immutable #true))]
  [formatting-rule? (-> any/c boolean?)]
  [formatting-rule-apply (-> formatting-rule? syntax? (or/c partially-formatted-syntax? #false))]
  [formatting-rule-fallback-rule? (-> formatting-rule? boolean?)]))


(require (for-syntax racket/base)
         racket/format
         racket/sequence
         syntax/parse
         syntax/parse/define)


;; An unformatted syntax object, to be formatted later by a subsequent pass.
(struct unformatted-syntax

  (;; The syntax object to format later.
   subform

   ;; A string to concatenate before the syntax object once it's formatted.
   prefix

   ;; A string to concatenate after the syntax object once it's formatted.
   suffix

   ;; Either 'align, indicating this object needs to be wrapped with (align ...)
   ;; after it's formatted (and concatenated with the prefix and suffix), or
   ;; a nonnegative integer indicating how many spaces of indentation to add with (nest ...)
   indentation

   ;; Whether or not to flattin the syntax object (before concatenation and alignment) in order to
   ;; guarantee it has no newlines.
   flatten?)

  #:transparent
  #:omit-define-syntaxes
  #:constructor-name constructor:unformatted-syntax)


(define (unformatted-syntax subform
                            #:prefix [prefix ""]
                            #:suffix [suffix ""]
                            #:indentation [indentation 0]
                            #:flatten? [flatten? #false])
  (constructor:unformatted-syntax subform prefix suffix indentation flatten?))


;; A partially formatted syntax object, consisting of a series of strings and
;; unformatted subforms. We use strings instead of doc? objects so that equals
;; works correctly on instances of this struct.
(struct partially-formatted-syntax (pieces)
  #:transparent
  #:constructor-name constructor:partially-formatted-syntax
  #:omit-define-syntaxes)


(define (partially-formatted-syntax pieces)
  (constructor:partially-formatted-syntax
   (vector->immutable-vector
    (for/vector ([piece pieces])
      (if (string? piece)
          (string->immutable-string piece)
          piece)))))


(struct formatting-rule (name formatting-function fallback-rule?)
  #:property prop:object-name (struct-field-index name)
  #:constructor-name constructor:formatting-rule)


(define (make-formatting-rule formatting-function
                              #:name [name #false]
                              #:fallback-rule? [fallback-rule? #false])
  (constructor:formatting-rule name formatting-function fallback-rule?))


(define (formatting-rule-apply rule stx)
  ((formatting-rule-formatting-function rule) stx))


(define-syntax-parse-rule
  (define-formatting-rule rule:id
    (~optional (~and #:fallback-rule (~bind [fallback? #'#true])))
    parse-option ...
    [pattern pattern-directive ... formatted-output])
  #:declare formatted-output (expr/c #'(or/c partially-formatted-syntax? #false))
  (define rule
    (make-formatting-rule
     #:name 'rule
     (~? (~@ #:fallback-rule? fallback?) (~@))
     (λ (stx)
       (syntax-parse stx parse-option ...
         [pattern pattern-directive ... formatted-output.c]
         [_ #false])))))
