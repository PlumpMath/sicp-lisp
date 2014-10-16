#! /usr/bin/env python3
import re
import pprint

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
    return value


def set_tail(pair_, value):
    pair_[1:] = value
    return value


def is_pair(obj):
    return isinstance(obj, list)


def pair(a, b):
    """
    cons
    """
    if isinstance(b, list):
        return [a] + b
    return [a, b]


length = len


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
    return ["if", predicate, consequent, alternative]


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


first_exp = head
rest_exps = tail
"""
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
"""


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
    return ["procedure", parameters, body, Env(env)]


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

enclosing_environment = tail
first_frame = head
the_empty_environment = []
"""
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
"""


def make_frame(variables, values):
    """
    (define (make-frame variables values)
      (cons variables values))
    """
    return dict(zip(variables, values))


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
        if not env:
            return error("Unbound variable", var)
        frame = first_frame(env)
        if var in frame:
            return frame[var]
        return env_loop(enclosing_environment(env))
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
        if not env:
            return error("Unbound variable: SET!", var)
        frame = first_frame(env)
        if var in frame:
            frame[var] = val
        return env_loop(enclosing_environment(env))
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
    first_frame(env)[var] = val
    return val

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# analyzers

def analyze_self_evaluating(exp):
    """
    (define (analyze-self-evaluating exp)
      (lambda (env) exp))
    """
    return lambda env: exp


def analyze_quoted(exp):
    """
    (define (analyze-quoted exp)
      (let ((qval (text-of-quotation exp)))
        (lambda (env) qval)))
    """
    qval = text_of_quotation(exp)
    return lambda env: qval


def analyze_variable(exp):
    """
    (define (analyze-variable exp)
      (lambda (env) 
        (lookup-variable-value exp env)))
    """
    return lambda env: lookup_variable_value(exp, env)


def analyze_assignment(exp):
    """
    (define (analyze-assignment exp)
      (let ((var (assignment-variable exp))
            (vproc (analyze 
                    (assignment-value exp))))
        (lambda (env)
          (set-variable-value! 
           var (vproc env) env)
          'ok)))
    """
    var = assignment_variable(exp)
    vproc = analyze(assignment_value(exp))
    return lambda env: set_variable_value(var, vproc(env), env)


def analyze_definition(exp):
    """
    (define (analyze-definition exp)
      (let ((var (definition-variable exp))
            (vproc (analyze 
                    (definition-value exp))))
        (lambda (env)
          (define-variable! var (vproc env) env)
          'ok)))
    """
    var = definition_variable(exp)
    vproc = analyze(definition_value(exp))
    def define_in(env):
        define_variable(var, vproc(env), env)
        return "\n%s" % pprint.pformat(env)
    return define_in


def analyze_if(exp):
    """
    (define (analyze-if exp)
      (let ((pproc (analyze (if-predicate exp)))
            (cproc (analyze (if-consequent exp)))
            (aproc (analyze (if-alternative exp))))
        (lambda (env)
          (if (true? (pproc env))
              (cproc env)
              (aproc env)))))
    """
    pre = analyze(if_predicate(exp))
    con = analyze(if_consequent(exp))
    alt = analyze(if_alternative(exp))
    return lambda env: con(env) if is_true(pre(env)) else alt(env)


def analyze_lambda(exp):
    """
    (define (analyze-lambda exp)
      (let ((vars (lambda-parameters exp))
            (bproc (analyze-sequence 
                    (lambda-body exp))))
        (lambda (env) 
          (make-procedure vars bproc env))))
    """
    params = lambda_parameters(exp)
    body = analyze_sequence(lambda_body(exp))
    return lambda env: make_procedure(params, body, env)


def analyze_sequence(exps):
    """
    (define (analyze-sequence exps)
      (define (sequentially proc1 proc2)
        (lambda (env) (proc1 env) (proc2 env)))
      (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc 
                                (car rest-procs))
                  (cdr rest-procs))))
      (let ((procs (map analyze exps)))
        (if (null? procs)
            (error "Empty sequence: ANALYZE"))
        (loop (car procs) (cdr procs))))
    """
    def sequentially(proc1, proc2):
        def combined(env):
            proc1(env)
            return proc2(env)
        return combined
    def loop(first_proc, rest_procs):
        if is_null(rest_procs):
            return first_proc
        return loop(sequentially(first_proc, head(rest_procs)), tail(rest_procs))
    procs = [analyze(e) for e in exps]
    if is_null(procs):
        return error("Empty sequence: ANALYZE")
    return loop(head(procs), tail(procs))


def analyze_application(exp):
    """
    (define (analyze-application exp)
      (let ((fproc (analyze (operator exp)))
            (aprocs (map analyze (operands exp))))
        (lambda (env)
          (execute-application 
           (fproc env)
           (map (lambda (aproc) (aproc env))
                aprocs)))))
    """
    fproc = analyze(operator(exp))
    aprocs = [analyze(op) for op in operands(exp)]
    return lambda env: execute_application(
                            fproc(env),
                            [p(env) for p in aprocs])


def execute_application(proc, args):
    """
    (define (execute-application proc args)
      (cond ((primitive-procedure? proc)
             (apply-primitive-procedure proc args))
            ((compound-procedure? proc)
             ((procedure-body proc)
              (extend-environment 
               (procedure-parameters proc)
               args
               (procedure-environment proc))))
            (else (error "Unknown procedure type: 
                          EXECUTE-APPLICATION"
                         proc))))
    """
    if is_primitive_procedure(proc):
        return apply_primitive_procedure(proc, args)
    if is_compound_procedure(proc):
        return procedure_body(proc)(
                    extend_environment(
                        procedure_parameters(proc),
                        args,
                        procedure_environment(proc)))
    return error("Unknown procedure type: EXECUTE-APPLICATION", proc)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.1.7+ : data driven analyze

ANALYZE_DATA = [
    (is_self_evaluating, analyze_self_evaluating),
    (is_quoted, analyze_quoted),
    (is_variable, analyze_variable),
    (is_assignment, analyze_assignment),
    (is_definition, analyze_definition),
    (is_if, analyze_if),
    (is_lambda, analyze_lambda),
    (is_begin, lambda exp: analyze_sequence(begin_actions(exp))),
    (is_cond, lambda exp: analyze(cond_to_if(exp))),
    (is_let, lambda exp: analyze(let_to_combination(exp))),
    (is_application, analyze_application),
]

def analyze(exp):
    """
    (define (analyze exp)
      (cond ((self-evaluating? exp)
             (analyze-self-evaluating exp))
            ((quoted? exp) 
             (analyze-quoted exp))
            ((variable? exp) 
             (analyze-variable exp))
            ((assignment? exp) 
             (analyze-assignment exp))
            ((definition? exp) 
             (analyze-definition exp))
            ((if? exp) 
             (analyze-if exp))
            ((lambda? exp) 
             (analyze-lambda exp))
            ((begin? exp) 
             (analyze-sequence 
              (begin-actions exp)))
            ((cond? exp) 
             (analyze (cond->if exp)))
            ((application? exp) 
             (analyze-application exp))
            (else
             (error "Unknown expression 
                     type: ANALYZE" 
                    exp))))
    """
    for predicate, analyze_ in ANALYZE_DATA:
        try:
            matched = predicate(exp)
        except Exception:
            return error("Cannot check", repr(exp), " with ", predicate.__name__)
        if matched:
            try:
                return analyze_(exp)
            except Exception:
                return error("Cannot analyze {}".format(predicate.__name__[3:]),
                             repr(exp))
    return error("Unknown expression type: EVAL", exp)


def eval(exp, env):
    """
    (define (eval exp env) ((analyze exp) env))
    """
    return analyze(exp)(env)

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
    return ["primitive", head(tail(obj))]


def add(*a):
    return sum(a)

def sub(a, b):
    return a - b

def mul(a, b):
    return a * b

def div(a, b):
    return a / b


primitive_procedures = [
    ("car", head),
    ("cdr", tail),
    ("cons", pair),
    ("null?", is_null),
    ("=", is_equal),
    ("+", add),
    ("-", sub),
    ("*", mul),
    ("/", div),
]
"""
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ⟨more primitives⟩ ))
"""


primitive_procedure_names = list(map(head, primitive_procedures))
"""
(define (primitive-procedure-names)
  (map car primitive-procedures))
"""


primitive_procedure_objects = list(map(make_primitive, primitive_procedures))
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
    return primitive_implementation(proc)(*args)


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
        output = eval(exp, the_global_environment)
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
        return error("missing closing paren", tokens)


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
        output = eval(exp, the_global_environment)
        print(user_repr(output))    


if __name__ == "__main__":
    try:
        driver_loop()
    except KeyboardInterrupt:
        print("")
