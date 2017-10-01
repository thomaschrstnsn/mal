;;  Copyright (C) 2015
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(import (readline) (reader) (printer) (ice-9 match) (srfi srfi-43)
        (srfi srfi-1) (ice-9 receive) (env) (core) (types))

(define *toplevel*
  (receive (b e) (unzip2 core.ns) 
    (make-Env #:binds b #:exprs e)))

(define (READ)
  (read_str (_readline "user> ")))

(define (eval_ast ast env)
  (define (_eval x) (EVAL x env))
  (match ast
    ((? _nil? obj) obj)
    ((? symbol? sym) (env-has sym env))
    ((? list? lst) (map _eval lst))
    ((? vector? vec) (vector-map (lambda (i x) (_eval x)) vec))
    ((? hash-table? ht)
     (hash-for-each (lambda (k v) (hash-set! ht k (_eval v))) ht)
     ht)
    (else ast)))

(define (eval_func ast env)
  (define expr (eval_ast ast env))
  (match expr
    (((? procedure? proc) args ...)
     (apply proc args))
    (else (throw 'mal-error (format #f "'~a' not found" (car expr))))))

(define (eval_seq ast env)
  (cond
   ((null? ast) nil)
   ((null? (cdr ast)) (EVAL (car ast) env))
   (else
    (EVAL (car ast) env)
    (eval_seq (cdr ast) env))))

(define (EVAL ast env)
  (define (->list kvs) ((if (vector? kvs) vector->list identity) kvs))
  (define (%unzip2 kvs)
    (let lp((next kvs) (k '()) (v '()))
      (cond
       ;; NOTE: reverse is very important here!
       ((null? next) (values (reverse k) (reverse v)))
       ((null? (cdr next)) (throw 'mal-error "let*: Invalid binding form" kvs)) 
       (else (lp (cddr next) (cons (car next) k) (cons (cadr next) v))))))
  ;; NOTE: I wish I can use (while #t ...) for that, but this is not Lispy, which means
  ;;       it'll bring some trouble in control flow. We have to use continuations to return
  ;;       and use non-standard `break' feature. In a word, not elegant at all.
  ;;       The named let loop is natural for Scheme, but it looks a bit cheating. But NO!
  ;;       Such kind of loop is actually `while loop' in Scheme, I don't take advantage of
  ;;       TCO in Scheme to implement TCO, but it's the same principle with normal loop.
  ;;       If you're Lispy enough, there's no recursive at all while you saw named let loop.
  (let tco-loop((ast ast) (env env))
    (match ast
      (() ast)
      (('def! k v) ((env 'set) k (EVAL v env)))
      (('let* kvs body)
       (let* ((new-env (make-Env #:outer env))
              (setter (lambda (k v) ((new-env 'set) k (EVAL v new-env)))))
         (receive (keys vals) (%unzip2 (->list kvs))
                  (for-each setter keys vals))
         (tco-loop body new-env)))
       (('do rest ...)
        (cond
         ((null? rest) (throw 'mal-error "do: Invalid form!" rest))
         ((= 1 (length rest)) (tco-loop (car rest) env))
         (else
          (let ((mexpr (take rest (1- (length rest))))
                (tail-call (car (take-right rest 1))))
            (eval_seq mexpr env)
            (tco-loop tail-call env)))))
       (('if cnd thn els ...)
        (cond
         ((and (not (null? els)) (not (null? (cdr els))))
          ;; Invalid `if' form
          (throw 'mal-error "if: failed to match any pattern in form " ast))
         ((cond-true? (EVAL cnd env)) (tco-loop thn env))
         (else (if (null? els) nil (tco-loop (car els) env)))))
       (('fn* params body ...) ; function definition
        (lambda args
          (let ((nenv (make-Env #:outer env #:binds (->list params) #:exprs args)))
            (cond
             ((null? body) (throw 'mal-error "fn*: bad lambda in form " ast))
             ((= 1 (length body)) (tco-loop (car body) nenv))
             (else
              (let ((mexpr (take body (1- (length body))))
                    (tail-call (car (take-right body 1))))
                (eval_seq mexpr nenv)
                (tco-loop tail-call nenv)))))))
       ((? list?) (eval_func ast env)) ; function calling
       (else (eval_ast ast env)))))

(define (PRINT exp)
  (and (not (eof-object? exp))
       (format #t "~a~%" (pr_str exp #t))))

(define (LOOP continue?)
  (and continue? (REPL)))

(define (REPL)
  (LOOP
   (catch 'mal-error
          (lambda () (PRINT (EVAL (READ) *toplevel*)))
          (lambda (k . e)
            (if (string=? (car e) "blank line")
                (display "")
                (format #t "Error: ~a~%" (car e)))))))

;; NOTE: we have to reduce stack size to pass step5 test
((@ (system vm vm) call-with-stack-overflow-handler)
 1024
 (lambda () (REPL))
 (lambda k (throw 'mal-error "stack overflow")))

