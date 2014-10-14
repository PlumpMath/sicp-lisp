#! /usr/bin/env python3
import re
import pprint

def procedural_eval(exp, env):
    """
    (define (eval exp env)
      (cond ((self-evaluating? exp) 
             exp)
            ((variable? exp) 
             (lookup-variable-value exp env))
            ((quoted? exp) 
             (text-of-quotation exp))
            ((assignment? exp) 
             (eval-assignment exp env))
            ((definition? exp) 
             (eval-definition exp env))
            ((if? exp) 
             (eval-if exp env))
            ((lambda? exp)
             (make-procedure 
              (lambda-parameters exp)
              (lambda-body exp)
              env))
            ((begin? exp)
             (eval-sequence 
              (begin-actions exp) 
              env))
            ((cond? exp) 
             (eval (cond->if exp) env))
            ((application? exp)
             (apply (eval (operator exp) env)
                    (list-of-values 
                     (operands exp) 
                     env)))
            (else
             (error "Unknown expression 
                     type: EVAL" exp))))
    """
    if is_self_evaluating(exp):
        return exp
    if is_variable(exp):
        return lookup_variable_value(exp, env)
    if is_quoted(exp):
        return text_of_quotation(exp)
    if is_assignment(exp):
        return eval_assignment(exp, env)
    if is_definition(exp):
        return eval_definition(exp, env)
    if is_if(exp):
        return eval_if(exp, env)
    if is_lambda(exp):
        return make_procedure(lambda_parameters(exp), lambda_body(exp), env)
    if is_begin(exp):
        return eval_sequence(begin_actions(exp), env)
    if is_cond(exp):
        return eval(cond_to_if(exp), env)
    if is_application(exp):
        return apply(
                eval(operator(exp), env),
                list_of_values(operands(exp), env))
    return error("Unknown expression type: EVAL", exp)


def apply(procedure, arguments, env):
    """
    (define (apply procedure arguments env)
      (cond ((primitive-procedure? procedure)
             (apply-primitive-procedure 
              procedure 
              (list-of-arg-values 
               arguments 
               env)))  ; changed
            ((compound-procedure? procedure)
             (eval-sequence
               (procedure-body procedure)
               (extend-environment
                 (procedure-parameters 
                  procedure)
                 (list-of-delayed-args 
                  arguments 
                  env)   ; changed
                 (procedure-environment 
                  procedure))))
            (else
             (error "Unknown procedure 
                     type: APPLY" 
                    procedure))))
    """
    if is_primitive_procedure(procedure):
        return apply_primitive_procedure(
                    procedure,
                    list_of_arg_values(arguments, env))
    if is_compound_procedure(procedure):
        return eval_sequence(
                    procedure_body(procedure),
                    extend_environment(
                        procedure_parameters(procedure),
                        list_of_delayed_args(arguments, env),
                        procedure_environment(procedure)))
    return error("Unknown procedure type: APPLY", procedure)


def actual_value(exp, env):
    """
    (define (actual-value exp env)
      (force-it (eval exp env)))
    """
    return force_it(eval(exp, env))


def force_it(obj):
    """
    (define (force-it obj)
      (cond ((thunk? obj)
             (let ((result
                    (actual-value 
                     (thunk-exp obj)
                     (thunk-env obj))))
               (set-car! obj 'evaluated-thunk)
               ;; replace exp with its value:
               (set-car! (cdr obj) result) 
               ;; forget unneeded env:
               (set-cdr! (cdr obj) '()) 
               result))
            ((evaluated-thunk? obj)
             (thunk-value obj))
            (else obj)))
    """
    if is_thunk(obj):
        result = actual_value(thunk_exp(obj), thunk_env(obj))
        set_head(obj, "evaluated-thunk")
        set_tail(obj, list_(result))
        return result
    if is_evaluated_thunk(obj):
        return thunk_value(obj)
    return obj


def delay_it(exp, env):
    """
    (define (delay-it exp env)
      (list 'thunk exp env))
    """
    return list_("thunk", exp, env)


def is_thunk(value):
    """
    (define (thunk? obj) (tagged-list? obj 'thunk))
    """
    return is_tagged_list(value, "thunk")


def thunk_exp(thunk):
    """
    (define (thunk-exp thunk) (cadr thunk))
    """
    return head(tail(thunk))


def thunk_env(thunk):
    """
    (define (thunk-env thunk) (caddr thunk))
    """
    return head(tail(tail(thunk)))


def is_evaluated_thunk(obj):
    """
    (define (evaluated-thunk? obj)
      (tagged-list? obj 'evaluated-thunk))
    """
    return is_tagged_list(obj, "evaluated-thunk")


def thunk_value(evaluated_thunk):
    """
    (define (thunk-value evaluated-thunk) 
      (cadr evaluated-thunk))
    """
    return head(tail(evaluated_thunk))


def list_of_arg_values(exps, env):
    """
    (define (list-of-arg-values exps env)
      (if (no-operands? exps)
          '()
          (cons (actual-value 
                 (first-operand exps) 
                 env)
                (list-of-arg-values 
                 (rest-operands exps)
                 env))))
    """
    if has_no_operands(exps):
        return []
    return pair(
            actual_value(first_operand(exps), env),
            list_of_arg_values(rest_operands(exps), env))


def list_of_delayed_args(exps, env):
    """
    (define (list-of-delayed-args exps env)
      (if (no-operands? exps)
          '()
          (cons (delay-it 
                 (first-operand exps) 
                 env)
                (list-of-delayed-args 
                 (rest-operands exps)
                 env))))
    """
    if has_no_operands(exps):
        return []
    return pair(
            delay_it(first_operand(exps), env),
            list_of_delayed_args(rest_operands(exps), env))


def list_of_values(exps, env):
    """
    (define (list-of-values exps env)
      (if (no-operands? exps)
          '()
          (cons (eval (first-operand exps) env)
                (list-of-values 
                 (rest-operands exps) 
                 env))))
    """
    if has_no_operands(exps):
        return []
    return pair(
        eval(first_operand(exps), env),
        list_of_values(rest_operands(exps), env))


def eval_if(exp, env):
    """
    (define (eval-if exp env)
      (if (true? (eval (if-predicate exp) env))
          (eval (if-consequent exp) env)
          (eval (if-alternative exp) env)))

    """
    if is_true(actual_value(if_predicate(exp), env)):
        return eval(if_consequent(exp), env)
    return eval(if_alternative(exp), env)


def eval_sequence(exps, env):
    """
    (define (eval-sequence exps env)
      (cond ((last-exp? exps) 
             (eval (first-exp exps) env))
            (else 
             (eval (first-exp exps) env)
             (eval-sequence (rest-exps exps) 
                            env))))
    """
    if is_last_exp(exps):
        return eval(first_exp(exps), env)
    eval(first_exp(exps), env)
    return eval_sequence(rest_exps(exps), env)


def eval_assignment(exp, env):
    """
    (define (eval-assignment exp env)
      (set-variable-value! 
       (assignment-variable exp)
       (eval (assignment-value exp) env)
       env)
      'ok)
    """
    set_variable_value(
        assignment_variable(exp),
        eval(assignment_value(exp), env),
        env)


def eval_definition(exp, env):
    """
    (define (eval-definition exp env)
      (define-variable! 
        (definition-variable exp)
        (eval (definition-value exp) env)
        env)
      'ok)
    """
    define_variable(
        definition_variable(exp),
        eval(definition_value(exp), env),
        env)
    return "\n%s" % pprint.pformat(env)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# syntax

def is_self_evaluating(exp):
    """
    (define (self-evaluating? exp)
      (cond ((number? exp) true)
            ((string? exp) true)
            (else false)))
    """
    return is_number(exp) or is_string(exp)


def is_number(exp):
    return isinstance(exp, int)


def is_string(exp):
    return isinstance(exp, string)


def is_variable(exp):
    """
    (define (variable? exp) (symbol? exp))
    """
    return is_symbol(exp)


def is_quoted(exp):
    """
    (define (quoted? exp)
      (tagged-list? exp 'quote))
    """
    return is_tagged_list(exp, "quote")


def text_of_quotation(exp):
    """
    (define (text-of-quotation exp)
      (cadr exp))
    """
    return head(tail(exp))


def is_tagged_list(exp, tag):
    """
    (define (tagged-list? exp tag)
      (if (pair? exp)
          (eq? (car exp) tag)
          false))
    """
    return is_pair(exp) and head(exp) == tag


def is_assignment(exp):
    """
    (define (assignment? exp)
      (tagged-list? exp 'set!))
    """
    return is_tagged_list(exp, "set!")


def assignment_variable(exp):
    """
    (define (assignment-variable exp) 
      (cadr exp))
    """
    return head(tail(exp))


def assignment_value(exp):
    """
    (define (assignment-value exp) (caddr exp))
    """
    return head(tail(tail(exp)))


def is_definition(exp):
    """
    (define (definition? exp)
      (tagged-list? exp 'define))
    """
    return is_tagged_list(exp, "define")


def definition_variable(exp):
    """
    (define (definition-variable exp)
      (if (symbol? (cadr exp))
          (cadr exp)
          (caadr exp)))
    """
    if is_symbol(head(tail(exp))):
        return head(tail(exp))
    return head(head(tail(exp)))


def definition_value(exp):
    """
    (define (definition-value exp)
      (if (symbol? (cadr exp))
          (caddr exp)
          (make-lambda 
           (cdadr exp)   ; formal parameters
           (cddr exp)))) ; body
    """
    if is_symbol(head(tail(exp))):
        return head(tail(tail(exp)))
    return make_lambda(
            tail(head(tail(exp))),  # formal parameters
            tail(tail(exp)))        # body


def is_lambda(exp):
    """
    (define (lambda? exp) 
      (tagged-list? exp 'lambda))
    """
    return is_tagged_list(exp, "lambda")


def lambda_parameters(exp):
    """
    (define (lambda-parameters exp) (cadr exp))
    """
    return head(tail(exp))


def lambda_body(exp):
    """
    (define (lambda-body exp) (cddr exp))
    """
    return tail(tail(exp))


def make_lambda(parameters, body):
    """
    (define (make-lambda parameters body)
      (cons 'lambda (cons parameters body)))
    """
    return pair("lambda", pair(parameters, body))


def is_if(exp):
    """
    (define (if? exp) (tagged-list? exp 'if))
    """
    return is_tagged_list(exp, "if")


def if_predicate(exp):
    """
    (define (if-predicate exp) (cadr exp))
    """
    return head(tail(exp))


def if_consequent(exp):
    """
    (define (if-consequent exp) (caddr exp))
    """
    return head(tail(tail(exp)))


def if_alternative(exp):
    """
    (define (if-alternative exp)
      (if (not (null? (cdddr exp)))
          (cadddr exp)
          'false))
    """
    if not is_null(tail(tail(tail(exp)))):
        return head(tail(tail(tail(exp))))

def make_if(predicate, consequent, alternative):
    """
    (define (make-if predicate 
                     consequent 
                     alternative)
      (list 'if 
            predicate 
            consequent 
            alternative))
    """
    return list_("if", predicate, consequent, alternative)


def is_begin(exp):
    """
    (define (begin? exp) 
      (tagged-list? exp 'begin))
    """
    return is_tagged_list(exp, "begin")


def begin_actions(exp):
    """
    (define (begin-actions exp) (cdr exp))
    """
    return tail(exp)


def is_last_exp(seq):
    """
    (define (last-exp? seq) (null? (cdr seq)))
    """
    return not tail(seq)


def first_exp(seq):
    """
    (define (first-exp seq) (car seq))
    """
    return head(seq)


def rest_exps(seq):
    """
    (define (rest-exps seq) (cdr seq))
    """
    return tail(seq)


def sequence_to_exp(seq):
    """
    (define (sequence->exp seq)
      (cond ((null? seq) seq)
            ((last-exp? seq) (first-exp seq))
            (else (make-begin seq))))
    """
    if not seq:
        return seq
    if is_last_exp(seq):
        return first_exp(seq)
    return make_begin(seq)


def make_begin(seq):
    """
    (define (make-begin seq) (cons 'begin seq))
    """
    return pair("begin", seq)


def is_application(exp):
    """
    (define (application? exp) (pair? exp))
    """
    return is_pair(exp)


def operator(exp):
    """
    (define (operator exp) (car exp))
    """
    return head(exp)


def operands(exp):
    """
    (define (operands exp) (cdr exp))
    """
    return tail(exp)


def has_no_operands(ops):
    """
    (define (no-operands? ops) (null? ops))
    """
    return not ops


def first_operand(ops):
    """
    (define (first-operand ops) (car ops))
    """
    return head(ops)


def rest_operands(ops):
    """
    (define (rest-operands ops) (cdr ops))
    """
    return tail(ops)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# derived expressions

def is_cond(exp):
    """
    (define (cond? exp) 
      (tagged-list? exp 'cond))
    """
    return is_tagged_list(exp, "cond")


def cond_clauses(exp):
    """
    (define (cond-clauses exp) (cdr exp))
    """
    return tail(exp)


def is_cond_else_clause(clause):
    """
    (define (cond-else-clause? clause)
      (eq? (cond-predicate clause) 'else))
    """
    return cond_predicate(clause) == "else"


def cond_predicate(clause):
    """
    (define (cond-predicate clause) 
      (car clause))
    """
    return head(clause)


def cond_actions(clause):
    """
    (define (cond-actions clause) 
      (cdr clause))
    """
    return tail(clause)


def cond_to_if(exp):
    """
    (define (cond->if exp)
      (expand-clauses (cond-clauses exp)))
    """
    return expand_clauses(cond_clauses(exp))


def expand_clauses(clauses):
    """
    (define (expand-clauses clauses)
      (if (null? clauses)
          'false     ; no else clause
          (let ((first (car clauses))
                (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp 
                     (cond-actions first))
                    (error "ELSE clause isn't 
                            last: COND->IF"
                           clauses))
                (make-if (cond-predicate first)
                         (sequence->exp 
                          (cond-actions first))
                         (expand-clauses 
                          rest))))))
    """
    if not clauses:
        return # no else clause
    first = head(clauses)
    rest = tail(clauses)
    if is_cond_else_clause(first):
        if not rest:
            return sequence_to_exp(cond_actions(first))
        return error("ELSE clause isn't last: COND->IF", clauses)
    return make_if(
                cond_predicate(first),
                sequence_to_exp(cond_actions(first)),
                expand_clauses(rest))


def let_to_combination(exp):
    """
    (let ((<var-1> <exp-1>) ... (<var-n> <exp-n>)) <body>)

    ->

    ((lambda (<var-1> ... <var-n>)
        <body>)
     <exp-1>
     ...
     <exp-n>)
    """
    name = None
    if is_symbol(head(tail(exp))):
        # named let
        exp = tail(exp)
        name = head(exp)
    names = let_names(exp)
    body = let_body(exp)
    if name is not None:
        define = make_define(name, names, body)
        body = pair(define, body)
    return pair(make_lambda(names, body), let_values(exp))


def make_define(name, vars, body):
    return pair("define", pair(pair(name, vars), body))


def let_names(exp):
    def heads(assignments):
        if not assignments:
            return assignments
        # TODO error handling for malformed expressions
        return pair(
                head(head(assignments)),
                heads(tail(assignments)))
    return heads(head(tail(exp)))


def let_values(exp):
    def tails(assignments):
        if not assignments:
            return assignments
        # TODO error handling for malformed expressions
        return pair(
                head(tail(head(assignments))),
                tails(tail(assignments)))
    return tails(head(tail(exp)))


def let_body(exp):
    return tail(tail(exp))


def is_let(exp):
    return is_tagged_list(exp, "let")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# testing predicates

def is_true(x):
    """
    (define (true? x)
      (not (eq? x false)))
    """
    return x != False


def is_false(x):
    """
    (define (false? x)
      (eq? x false))
    """
    return x == False

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# representing procedures

def make_procedure(parameters, body, env):
    """
    (define (make-procedure parameters body env)
      (list 'procedure parameters body env))
    """
    return list_("procedure", parameters, body, Env(env))


def is_compound_procedure(proc):
    """
    (define (compound-procedure? p)
      (tagged-list? p 'procedure))
    """
    return is_tagged_list(proc, "procedure")


def procedure_parameters(proc):
    """
    (define (procedure-parameters p) (cadr p))
    """
    return head(tail(proc))


def procedure_body(proc):
    """
    (define (procedure-body p) (caddr p))
    """
    return head(tail(tail(proc)))


def procedure_environment(proc):
    """
    (define (procedure-environment p) (cadddr p))
    """
    return head(tail(tail(tail(proc)))).env


class Env(object):

    def __init__(self, env):
        self.env = env

    def __repr__(self):
        rep = repr(self.env)
        if len(rep) > 50:
            rep = rep[:50] + "... "
        return rep


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# operations on environments

def enclosing_environment(env):
    """
    (define (enclosing-environment env) (cdr env))
    """
    return tail(env)


def first_frame(env):
    """
    (define (first-frame env) (car env))
    """
    return head(env)


"""
(define the-empty-environment '())
"""
the_empty_environment = []


def make_frame(variables, values):
    """
    (define (make-frame variables values)
      (cons variables values))
    """
    return pair(variables, values)


def frame_variables(frame):
    """
    (define (frame-variables frame) (car frame))
    """
    return head(frame)


def frame_values(frame):
    """
    (define (frame-values frame) (cdr frame))
    """
    return tail(frame)


def add_binding_to_frame(var, val, frame):
    """
    (define (add-binding-to-frame! var val frame)
      (set-car! frame (cons var (car frame)))
      (set-cdr! frame (cons val (cdr frame))))
    """
    set_head(frame, pair(var, head(frame)))
    set_tail(frame, pair(val, tail(frame)))


def extend_environment(vars, vals, base_env):
    """
    (define (extend-environment vars vals base-env)
      (if (= (length vars) (length vals))
          (cons (make-frame vars vals) base-env)
          (if (< (length vars) (length vals))
              (error "Too many arguments supplied" 
                     vars 
                     vals)
              (error "Too few arguments supplied" 
                     vars 
                     vals))))
    """
    if length(vars) == length(vals):
        return pair(make_frame(vars, vals), base_env)
    if length(vars) < length(vals):
        return error("Too many arguments supplied", vars, vals)
    return error("Too few arguments supplied", vars, vals)


def lookup_variable_value(var, env):
    """
    (define (lookup-variable-value var env)
      (define (env-loop env)
        (define (scan vars vals)
          (cond ((null? vars)
                 (env-loop 
                  (enclosing-environment env)))
                ((eq? var (car vars))
                 (car vals))
                (else (scan (cdr vars) 
                            (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
              (scan (frame-variables frame)
                    (frame-values frame)))))
      (env-loop env))
    """
    def env_loop(env):
        def scan(vars, vals):
            if not vars:
                return env_loop(enclosing_environment(env))
            if var == head(vars):
                return head(vals)
            return scan(tail(vars), tail(vals))
        if env == the_empty_environment:
            return error("Unbound variable", var)
        frame = first_frame(env)
        return scan(frame_variables(frame), frame_values(frame))
    return env_loop(env)


def set_variable_value(var, val, env):
    """
    (define (set-variable-value! var val env)
      (define (env-loop env)
        (define (scan vars vals)
          (cond ((null? vars)
                 (env-loop 
                  (enclosing-environment env)))
                ((eq? var (car vars))
                 (set-car! vals val))
                (else (scan (cdr vars) 
                            (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable: SET!" var)
            (let ((frame (first-frame env)))
              (scan (frame-variables frame)
                    (frame-values frame)))))
      (env-loop env))
    """
    def env_loop(env):
        def scan(vars, vals):
            if not vars:
                return env_loop(enclosing_environment(env))
            if var == head(vars):
                return set_head(vals, val)
            return scan(tail(vars), tail(vals))
        if env == the_empty_environment:
            return error("Unbound variable: SET!", var)
        frame = first_frame(env)
        return scan(frame_variables(frame), frame_values(frame))
    return env_loop(env)


def define_variable(var, val, env):
    """
    (define (define-variable! var val env)
      (let ((frame (first-frame env)))
        (define (scan vars vals)
          (cond ((null? vars)
                 (add-binding-to-frame! 
                  var val frame))
                ((eq? var (car vars))
                 (set-car! vals val))
                (else (scan (cdr vars) 
                            (cdr vals)))))
        (scan (frame-variables frame)
              (frame-values frame))))
    """
    def scan(vars, vals):
        if not vars:
            return add_binding_to_frame(var, val, frame)
        if var == head(vars):
            return set_head(vals, val)
        return scan(tail(vars), tail(vals))
    frame = first_frame(env)
    return scan(frame_variables(frame), frame_values(frame))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exercise 4.3 : data driven eval

EVAL_DATA = (
    (is_self_evaluating, lambda exp, env: exp),
    (is_variable, lookup_variable_value),
    (is_quoted, lambda exp, env: text_of_quotation(exp)),
    (is_assignment, eval_assignment),
    (is_definition, eval_definition),
    (is_if, eval_if),
    (is_lambda, lambda exp, env: make_procedure(
                                    lambda_parameters(exp),
                                    lambda_body(exp), env)),
    (is_begin, lambda exp, env: eval_sequence(begin_actions(exp), env)),
    (is_cond, lambda exp, env: eval(cond_to_if(exp), env)),
    (is_let, lambda exp, env: eval(let_to_combination(exp), env)),
    (is_application, lambda exp, env: apply(
                                        actual_value(operator(exp), env),
                                        operands(exp), env)),
)

def data_driven_eval(exp, env):
    for predicate, process in EVAL_DATA:
        try:
            matched = predicate(exp)
        except Exception:
            return error("Cannot check", repr(exp), " with ", predicate.__name__)
        if matched:
            try:
                return process(exp, env)
            except Exception:
                return error("Cannot process {}".format(predicate.__name__[3:]),
                             repr(exp), '\nenv: ', repr(env))
    return error("Unknown expression type: EVAL", exp)


eval = data_driven_eval

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lisp primitives

def head(obj):
    """
    car
    """
    if not obj:
        error("Empty list has no head", obj)
    try:
        return obj[0]
    except Exception:
        return error("Cannot get CAR:", obj)


def tail(obj):
    """
    cdr
    """
    if not obj:
        error("Empty list has no tail", obj)
    try:
        return obj[1:]
    except Exception:
        return error("Cannot get CDR:", obj)


def set_head(pair_, value):
    pair_[0] = value


def set_tail(pair_, value):
    pair_[1:] = value


def is_pair(obj):
    return isinstance(obj, list)


def pair(a, b):
    if isinstance(b, list):
        return [a] + b
    return [a, b]


def list_(*args):
    return list(args)


def iter_list(lst):
    return iter(lst)


def length(lst):
    return len(lst) #sum(1 for v in iter_list(lst))


def is_null(exp):
    """
    null?
    """
    return exp is None or exp == []


SYMBOL_RE = re.compile("^[^\s()]+$")
def is_symbol(exp):
    return isinstance(exp, str) and SYMBOL_RE.match(exp)


class string(str):
    """Marker class for string values"""


def is_equal(a, b):
    return a == b


def error(msg, *args):
    fargs = (a if isinstance(a, str) else repr(a) for a in args)
    raise Error("{} {}".format(msg, " ".join(fargs)))


class Error(Exception): pass

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# running the evaluator as a program

def setup_environment():
    """
    (define (setup-environment)
      (let ((initial-env
             (extend-environment 
              (primitive-procedure-names)
              (primitive-procedure-objects)
              the-empty-environment)))
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))
    """
    initial_env = extend_environment(
                    primitive_procedure_names,
                    primitive_procedure_objects,
                    the_empty_environment)
    define_variable("true", True, initial_env)
    define_variable("false", False, initial_env)
    return initial_env

def is_primitive_procedure(proc):
    """
    (define (primitive-procedure? proc)
      (tagged-list? proc 'primitive))
    """
    return is_tagged_list(proc, "primitive")


def primitive_implementation(proc):
    """
    (define (primitive-implementation proc) 
      (cadr proc))
    """
    return head(tail(proc))


def make_primitive(obj):
    return list_("primitive", head(tail(obj)))


def add(*a):
    return sum(a)

def sub(a, b):
    return a - b

def mul(a, b):
    return a * b

def div(a, b):
    return a / b

primitive_procedures = [
    list_("car", head),
    list_("cdr", tail),
    list_("cons", pair),
    list_("null?", is_null),
    list_("=", is_equal),
    list_("+", add),
    list_("-", sub),
    list_("*", mul),
    list_("/", div),
]
"""
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ⟨more primitives⟩ ))
"""


primitive_procedure_names = list_(*map(head, primitive_procedures))
"""
(define (primitive-procedure-names)
  (map car primitive-procedures))
"""


primitive_procedure_objects = list_(*map(make_primitive, primitive_procedures))
"""
(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))
"""


def apply_primitive_procedure(proc, args):
    """
    (define (apply-primitive-procedure proc args)
      (apply-in-underlying-scheme
       (primitive-implementation proc) args))
    """
    return primitive_implementation(proc)(*iter_list(args))


input_prompt  = ";;; M-Eval input: "
output_prompt = ";;; M-Eval value: "
"""
(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
"""


the_global_environment = setup_environment()
"""
(define the-global-environment 
  (setup-environment))
"""


def driver_loop():
    """
    (define (driver-loop)
      (prompt-for-input input-prompt)
      (let ((input (read)))
        (let ((output 
               (eval input 
                     the-global-environment)))
          (announce-output output-prompt)
          (user-print output)))
      (driver-loop))

    (define (prompt-for-input string)
      (newline) (newline) 
      (display string) (newline))

    (define (announce-output string)
      (newline) (display string) (newline))
    """
    print("python powered lisp")
    input_ = input(input_prompt)
    for exp in tokenize(input_):
        output = actual_value(exp, the_global_environment)
        print("{}{}".format(output_prompt, user_repr(output)))


def tokenize(program):
    for exp in iter_expressions(PushBackIterator(program.strip())):
        yield exp


def iter_expressions(chars):
    token = []
    tokens = []
    balanced = False
    for char in chars:
        print(char, end="")
        if char == ")":
            if token:
                yield "".join(token)
            return
        elif char == "(":
            if token:
                tokens.append("".join(token))
                yield tokens[-1]
                token = []
            yield list(iter_expressions(chars))
        elif char in " \n\t\r":
            if token:
                tokens.append("".join(token))
                yield tokens[-1]
                token = []
        elif char in '"':
            if token:
                tokens.append("".join(token))
                yield tokens[-1]
                token = []
            tokens.append(consume_string(chars))
            yield tokens[-1]
        elif char in "1234567890":
            if token:
                tokens.append("".join(token))
                yield tokens[-1]
                token = []
            tokens.append(consume_number(char, chars))
            yield tokens[-1]
        else:
            token.append(char)
    if token:
        tokens.append("".join(token))
        yield "".join(token)
    if tokens:
        error("missing closing paren", tokens)


def consume_number(first, chars):
    prev = None
    value = [first]
    for char in chars:
        if char == ")":
            chars.push(char)
            break
        print(char, end="")
        if char in " \n\t\r":
            break
        value.append(char)
        if char not in "1234567890":
            return error("Malformed token:", "".join(value))
    return int("".join(value))


def consume_string(chars):
    prev = None
    value = []
    for char in chars:
        print(char, end="")
        if char == '"' and prev != '\\':
            break
        value.append(char)
        prev = char
    else:
        return error("Unterminated string:", "".join(value))
    return string("".join(value))


def user_repr(object):
    """
    (define (user-print object)
      (if (compound-procedure? object)
          (display 
           (list 'compound-procedure
                 (procedure-parameters object)
                 (procedure-body object)
                 '<procedure-env>))
          (display object)))
    """
    if is_compound_procedure(object):
        return repr(object)
    return object


class PushBackIterator(object):

    def __init__(self, iterable):
        self.itr = iter(iterable)
        self.pushed = []

    def __iter__(self):
        return self

    def __next__(self):
        if self.pushed:
            return self.pushed.pop()
        return next(self.itr)

    def push(self, value):
        self.pushed.append(value)


def run(program):
    for exp in tokenize(program):
        print("")
        output = actual_value(exp, the_global_environment)
        print(user_repr(output))    


if __name__ == "__main__":
    try:
        driver_loop()
    except KeyboardInterrupt:
        print("")
