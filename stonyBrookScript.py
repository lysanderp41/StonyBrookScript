class Node:
    def __init__(self):
        print("init node")

    def evaluate(self):
        return 0

    def execute(self):
        return 0

class BlockNode(Node):
    def __init__(self, s):
        self.statements = [s]

    def execute(self):
        for statement in self.statements:
            statement.execute()

class NumberNode(Node):
    def __init__(self, v):
        if('.' in v):
            self.value = float(v)
        else:
            self.value = int(v)

    def evaluate(self):
        return self.value

class StatementNode(Node):
    def execute(self):
        pass

class PrintNode(StatementNode):
    def __init__(self, v):
        self.v = v

    def execute(self):
        print(repr(self.v.evaluate()))

class AssignmentNode(StatementNode):
    def __init__(self, symbol, value):
        self.symbol = symbol
        self.value = value

    def execute(self):
        symbol_table[self.symbol.v] = self.value.evaluate()

class AssignToListNode(StatementNode):
    def __init__(self, symbol, index, value):
        self.symbol = symbol
        self.index = index
        self.value = value

    def execute(self):
        symbol_table[self.symbol.v][self.index.evaluate()] = self.value.evaluate()

class IfNode(StatementNode):
    def __init__(self, condition, block):
        self.condition = condition
        self.block = block

    def execute(self):
        if (self.condition.evaluate()):
            self.block.execute()

class IfElseNode(StatementNode):
    def __init__(self, condition, if_block, else_block):
        self.condition = condition
        self.if_block = if_block
        self.else_block = else_block

    def execute(self):
        if (self.condition.evaluate()):
            self.if_block.execute()
        else:
            self.else_block.execute()

class WhileNode(StatementNode):
    def __init__(self, condition, block):
        self.condition = condition
        self.block = block

    def execute(self):
        while(self.condition.evaluate()):
            self.block.execute()
    
class BooleanNode(Node):
    def __init__(self, v):
        if v=='true':
            self.v = True
        else:
            self.v = False

    def evaluate(self):
        return self.v

class ListNode(Node):
    def __init__(self, v):
        self.v = [v]

    def evaluate(self):
        l = []
        for v in self.v:
            l.append(v.evaluate())
        return l

class StringNode(Node):
    def __init__(self, v):
        self.v = v[1:-1]

    def evaluate(self):
        return self.v

class SymbolNode(Node):
    def __init__(self, v):
        self.v = v

    def evaluate(self):
        return symbol_table[self.v]


class BopNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op

    def evaluate(self):
        if (self.op == '+'):
            return self.v1.evaluate() + self.v2.evaluate()
        elif (self.op == '-'):
            return self.v1.evaluate() - self.v2.evaluate()
        elif (self.op == '*'):
            return self.v1.evaluate() * self.v2.evaluate()
        elif (self.op == '/'):
            return self.v1.evaluate() / self.v2.evaluate()
        elif (self.op == '**'):
            return self.v1.evaluate() ** self.v2.evaluate()
        elif (self.op == '//'):
            return self.v1.evaluate() // self.v2.evaluate()
        elif (self.op == '%'):
            return self.v1.evaluate() % self.v2.evaluate()
        elif (self.op == 'and'):
            return self.v1.evaluate() and self.v2.evaluate()
        elif (self.op == 'or'):
            return self.v1.evaluate() or self.v2.evaluate()
        elif (self.op == '<'):
            return self.v1.evaluate() < self.v2.evaluate()
        elif (self.op == '<='):
            return self.v1.evaluate() <= self.v2.evaluate()
        elif (self.op == '=='):
            return self.v1.evaluate() == self.v2.evaluate()
        elif (self.op == '<>'):
            return self.v1.evaluate() != self.v2.evaluate()
        elif (self.op == '>='):
            return self.v1.evaluate() >= self.v2.evaluate()
        elif (self.op == '>'):
            return self.v1.evaluate() > self.v2.evaluate()
        elif (self.op == 'in'):
            return self.v1.evaluate() in self.v2.evaluate()
        elif (self.op == '['):
            return self.v1.evaluate()[self.v2.evaluate()]

class UopNode(Node):
    def __init__(self, op, v):
        self.op = op
        self.v = v

    def evaluate(self):
        if (self.op == 'not'):
            return not self.v.evaluate()

symbol_table = dict()

reserved = {
    'and' : 'AND',
    'or'  : 'OR',
    'true': 'TRUE',
    'false': 'FALSE',
    'not' : 'NOT',
    'in'  : 'IN',
    'if'  : 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'print': 'PRINT'
}

tokens = [
    'LPAREN', 'RPAREN','SEMI',
    'NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE', 'ASSIGN',
    'POWER', 'FLOOR', 'MOD',
    'STRING', 'SYMBOL',
    'LBRACKET','RBRACKET','COMMA', 'LBRACE', 'RBRACE',
    'LT', 'LTE', 'EQ', 'GTE', 'GT', 'NEQ'
    ] + list(reserved.values())

# Tokens
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_SEMI    = r';'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_FLOOR   = r'//'
t_POWER   = r'\*\*'
t_MOD     = r'%'
t_ASSIGN  = r'='
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE  = r'\{'
t_RBRACE  = r'\}'
t_LT      = r'<'
t_LTE     = r'<='
t_EQ      = r'=='
t_NEQ     = r'<>'
t_GTE     = r'>='
t_GT      = r'>'
t_COMMA = r','

def t_TRUE(t):
    r'true'
    t.value = BooleanNode(t.value)
    return t

def t_FALSE(t):
    r'false'
    t.value = BooleanNode(t.value)
    return t

def t_NUMBER(t):
    r'-?\d*(\d\.|\.\d)\d* | \d+'
    try:
        t.value = NumberNode(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_STRING(t):
    r'((\"[^(\'|\")]*\")|(\'[^(\'|\")]*\'))'
    t.value = StringNode(t.value)
    return t

def t_SYMBOL(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'SYMBOL')
    if t.type=='SYMBOL':
        t.value = SymbolNode(t.value)
    return t

# Ignored characters
t_ignore = " \t\n"

def t_error(t):
    print("Syntax error at '%s'" % t.value)
    
# Build the lexer
import ply.lex as lex
lex.lex()

# Parsing rules
precedence = (
    ('right', 'ASSIGN'),
    ('left', 'PRINT'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'NOT'),
    ('left', 'LT', 'LTE', 'EQ', 'NEQ', 'GTE', 'GT'),
    ('left', 'IN'),
    ('left','PLUS','MINUS'),
    ('left', 'FLOOR'),
    ('left', 'MOD'),
    ('left','TIMES','DIVIDE'),
    ('right', 'POWER'),
    ('left', 'LBRACKET')
    )

def p_block(t):
    '''block : LBRACE in_block RBRACE'''
    t[0] = t[2]

def p_in_block(t):
    '''in_block : smt'''
    t[0] = BlockNode(t[1])

def p_in_block2(t):
    '''in_block : smt in_block'''
    t[2].statements.insert(0,t[1])
    t[0] = t[2]

def p_smt(t):
    '''smt : print_smt
           | assign_smt
           | list_assign_smt
           | if_smt
           | if_else_smt
           | while_smt
           '''
    t[0] = t[1]

def p_print_smt(t):
    '''print_smt : PRINT LPAREN expression RPAREN SEMI'''
    t[0] = PrintNode(t[3])

def p_list_assign_smt(t):
    '''list_assign_smt : SYMBOL LBRACKET expression RBRACKET ASSIGN expression SEMI'''
    t[0] = AssignToListNode(t[1], t[3], t[6])
    
def p_assign_smt(t):
    '''assign_smt : SYMBOL ASSIGN expression SEMI'''
    t[0] = AssignmentNode(t[1], t[3])

def p_if_smt(t):
    '''if_smt : IF LPAREN expression RPAREN block'''
    t[0] = IfNode(t[3], t[5])

def p_if_else_smt(t):
    '''if_else_smt : IF LPAREN expression RPAREN block ELSE block'''
    t[0] = IfElseNode(t[3], t[5], t[7])

def p_while_smt(t):
    '''while_smt : WHILE LPAREN expression RPAREN block'''
    t[0] = WhileNode(t[3], t[5])

def p_list(t):
    '''expression : LBRACKET in_list RBRACKET'''
    t[0] = t[2]

def p_in_list(t):
    '''in_list : expression'''
    t[0] = ListNode(t[1])

def p_in_list2(t):
    '''in_list : expression COMMA in_list'''
    t[3].v.insert(0,t[1])
    t[0] = t[3]


def p_expression_binop(t):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression FLOOR expression
                  | expression MOD expression
                  | expression POWER expression
                  | expression AND expression
                  | expression OR expression
                  | expression LT expression
                  | expression LTE expression
                  | expression EQ expression
                  | expression NEQ expression
                  | expression GTE expression
                  | expression GT expression
                  | expression IN expression'''
    t[0] = BopNode(t[2], t[1], t[3])

def p_expression_uop(t):
    '''expression : NOT expression'''
    t[0] = UopNode(t[1], t[2])

def p_expression_index(t):
    '''expression : expression LBRACKET expression RBRACKET'''
    t[0] = BopNode(t[2],t[1], t[3])

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = t[2]

def p_expression_number(t):
    'expression : NUMBER'
    t[0] = t[1]

def p_expression_true(t):
    'expression : TRUE'
    t[0] = t[1]

def p_expression_false(t):
    'expression : FALSE'
    t[0] = t[1]

def p_expression_string(t):
    'expression : STRING'
    t[0] = t[1]

def p_expression_symbol(t):
    'expression : SYMBOL'
    t[0] = t[1]

def p_error(t):
    #print("Syntax ERROR at '%s'" % t.value)
    raise Exception("Syntax Error")

import ply.yacc as yacc
yacc.yacc()

import sys

if (len(sys.argv) != 2):
    sys.exit("invalid arguments")
fd = open(sys.argv[1], 'r')

try:
    code = (fd.read())
    lex.input(code)
    while True:
        token = lex.token()
        if not token: break
        #print(token)
    ast = yacc.parse(code)
    ast.execute()
except SyntaxError:
    print("SYNTAX ERROR")
    
except Exception:
    print("SEMANTIC ERROR")
#print(symbol_table)



