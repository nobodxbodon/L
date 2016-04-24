# libinterp.py
import re
import os
from decimal import Decimal, getcontext, ROUND_05UP

getcontext().rounding = ROUND_05UP

meta_lambda = [
    'define',
    'update',

    'cond',
    'eq',
    'lambda',
    'progn',
    'if',
    'input',
    'output',
    'apply',
    'quote',
    'string',
    'import',
    'let',
    'eval',
    'type',
    'pass',
    'trap',
    'assert',
    'exit',

    'get',
    'set',
    'insert',
    'delete',
    'length',
    'copy',

    'and',
    'or',
    'not',

    'encode',
    'decode',

    'gt',
    'lt',
    'add',
    'sub',
    'mul',
    'div',
    'trunc',
]

class Node:
    def __init__(self, text=None, ln=None, col=None):
        self.text = text
        self.ln = ln
        self.col = col
        self.type = None
        self.value = None
        self.sub_nodes = []
        self.expr = None

    def __eq__(self, other):
        if self.type is not other.type:
            return False
        elif self.type is LambdaType:
            return self.get_expr() == other.get_expr()
        elif self.type is ListType:
            if self.get_length() != other.get_length():
                return False
            else:
                for a, b in zip(self.sub_nodes, other.sub_nodes):
                    if a != b:
                        return False
                return True
        else:
            return self.value == other.value

    def __str__(self):
        if self.type is StringType:
            return self.value
        else:
            return self.get_expr()

    def __repr__(self):
        return self.__str__()

    def __getitem__(self, i):
        return self.sub_nodes[i]

    def __setitem__(self, i, v):
        self.sub_nodes[i] = v

    def get_expr(self):
        if self.type is None:
            return self.text

        if self.expr is not None and self.type is not ListType:
            return self.expr

        if self.type is BoolType:
            if self.value:
                self.expr = '#true'
            else:
                self.expr = '#false'
        elif self.type is NumberType:
            self.expr = str(self.value)
        elif self.type is StringType:
            fragments = []
            fragments.append("'")
            for c in self.value:
                if c == '"':
                    fragments.append('\\0022')
                elif c == "'":
                    fragments.append('\\0027')
                elif c == '\\':
                    fragments.append('\\\\')
                elif c.isprintable():
                    fragments.append(c)
                else:
                    fragments.append('\\%s' % (hex(ord(c))[2:].zfill(4).upper(),))
            fragments.append("'")

            self.expr = ''.join(fragments)

        elif self.type is ErrorType:
            self.expr = str(self.value)
        elif self.type is NoneType:
            self.expr = '#none'
        elif self.type is LambdaType:
            if self.value == '__lambda__':
                self.expr = '(lambda %s %s)' % (self[0].get_expr(), self[1].get_expr())
            else:
                self.expr = self.value
        elif self.type is ExprType:
            fragments = []
            for sub_node in self:
                fragments.append(sub_node.get_expr())
            self.expr = '(%s)' % (' '.join(fragments), )
        elif self.type is ListType:
            fragments = []
            for sub_node in self:
                fragments.append(sub_node.get_expr())
            self.expr = '[%s]' % (' '.join(fragments), )
        elif self.type is SymbolType:
            self.expr = self.value
        elif self.type is TypeType:
            self.expr = self.value
        else:
            pass

        return self.expr

    def set_type(self, type):
        self.type = type
        return self

    def set_value(self, value):
        self.value = value
        return self

    def set_sub_nodes(self, v):
        self.sub_nodes = v
        return self

    def append_sub_node(self, sub_node):
        self.sub_nodes.append(sub_node)

    def get_op_node(self):
        return self[0]

    def get_param_nodes(self):
        return self[1:]

    def get_length(self):
        return len(self.sub_nodes)

class Stack:
    def __init__(self):
        self.container = []
    def push(self, a):
        self.container.insert(0, a)
    def pop(self):
        return self.container.pop(0)
    def length(self):
        return len(self.container)
    def not_empty(self):
        return len(self.container) != 0
    def top(self):
        return self.container[0]

class Env:
    def __init__(self, parent=None):
        if parent is None:
            self.parent = env_root
        else:
            self.parent = parent
        self.items = {}

    def lookup(self, name):
        retval = self.items.get(name)
        if retval is None and self.parent is not None:
            return self.parent.lookup(name)
        else:
            return retval

    def define(self, name, node):
        self.items[name] = node

    def update(self, name, node):
        if name in self.items.keys():
            if self.items[name].type is node.type:
                self.items[name] = node
                return True
            else:
                return False
        elif self.parent is not None:
            self.parent.update(name, node)
            return True
        else:
            return False

class EnvRoot(Env):
    def __init__(self):
        self.items = {}
        self.parent = None

TypeType = Node().set_value(':type')
TypeType.set_type(TypeType)

def CreateTypeNode(type_string):
    return Node().set_type(TypeType).set_value(type_string)

SymbolType = CreateTypeNode(':symbol')
ExprType = CreateTypeNode(':expr')
NumberType = CreateTypeNode(':number')
StringType = CreateTypeNode(':string')
BoolType = CreateTypeNode(':bool')
NoneType = CreateTypeNode(':none')
LambdaType = CreateTypeNode(':lambda')
ErrorType = CreateTypeNode(':error')
ListType = CreateTypeNode(':list')

NodeTrue = Node().set_type(BoolType).set_value(True)
NodeFalse = Node().set_type(BoolType).set_value(False)
NodeNone = Node().set_type(NoneType).set_value(None)

AtomicTypes = [NumberType, StringType, BoolType, NoneType, LambdaType, ErrorType, TypeType]

env_root = EnvRoot()
env_root.parent = None
for __lambda in meta_lambda:
    env_root.define(__lambda, Node().set_type(LambdaType).set_value(__lambda))

def CreateLambdaNode(params, expr, env):
    return Node().set_type(LambdaType).set_value('__lambda__').set_sub_nodes([params, expr, env])

def CreateNumberNode(x=0):
    return Node().set_type(NumberType).set_value(Decimal(x))

def CreateStringNode(s):
    return Node().set_type(StringType).set_value(s)

def CreateListNode(v):
    return Node().set_type(ListType).set_sub_nodes(v)

def CreateEvalNode(v):
    return Node().set_type(ExprType).set_sub_nodes(v)

def CreateErrorNode(s='0'):
    return Node().set_type(ErrorType).set_value(s)

def CreateBoolNode(b):
    if b:
        return NodeTrue
    else:
        return NodeFalse

def Lex(code_string):
    p = r"""\(|\)|\[|\]|(?<=\s||\(|\[|\]|\)|)[^\(\[\]\)'"\s]+?(?=\s|\)|\]||\(|\[|)|"(?:\\"|.)*?"|'(?:\\'|.)*?'|^.*$"""
    tokens = re.findall(p, code_string)
    container = []
    for token in tokens:
        container.append(Node(token))
    return container

def Lex(code_string):
    single_quote_mark = False
    double_quote_mark = False
    token_ln = 1
    token_col = 1
    current_ln = 1
    current_col = 1

    char_container = []
    node_container = []

    for c in code_string:
        if single_quote_mark is True:
            if c == "'":
                single_quote_mark = False
                char_container.append(c)
                node_container.append(Node(''.join(char_container), token_ln, token_col))
                char_container.clear()
            else:
                char_container.append(c)
        elif double_quote_mark is True:
            if c == '"':
                double_quote_mark = False
                char_container.append(c)
                node_container.append(Node(''.join(char_container), token_ln, token_col))
                char_container.clear()
            else:
                char_container.append(c)
        else:
            if c in ['(', '[', ']', ')', ' ', '"', "'", '\n']:
                if len(char_container) != 0:
                    node_container.append(Node(''.join(char_container), token_ln, token_col))
                    char_container.clear()

                if c in ['(', '[', ']', ')']:
                    node_container.append(Node(c, current_ln, current_col))
                elif c == "'":
                    single_quote_mark = True
                    char_container.append(c)
                    token_ln = current_ln
                    token_col = current_col
                elif c == '"':
                    double_quote_mark = True
                    char_container.append(c)
                    token_ln = current_ln
                    token_col = current_col
                elif c == '\n':
                    current_ln += 1
                    current_col = 1
                    continue
                else:
                    pass
            else:
                if len(char_container) == 0:
                    token_ln = current_ln
                    token_col = current_col

                char_container.append(c)

        current_col += 1

    if len(char_container) != 0:
        node_container.append(Node(''.join(char_container), token_ln, token_col))
        char_container.clear()

    return node_container

def Validate(nodes):
    stack = Stack()
    for node in nodes:
        if node.text in ['(', '[']:
            stack.push(node)
        elif node.text in [')', ']']:
            check(stack.not_empty(), 'unexpected bracket.' + '%d:%d' % (node.ln, node.col))
            stack_node = stack.pop()
            right_bracket = {'(':')', '[':']'}[stack_node.text]
            check(right_bracket == node.text, 'brackets not match.' + '%d:%d' % (node.ln, node.col))

    if stack.not_empty():
        node = stack.pop()
        check(False, "unexpected bracket." + '%d:%d' % (node.ln, node.col))

    return nodes

def Clean(nodes):
    return nodes

def Parse(nodes):
    Types = {
        ':number': NumberType,
        ':bool': BoolType,
        ':none': NoneType,
        ':lambda': LambdaType,
        ':error': ErrorType,
        ':list': ListType,
        ':string': StringType,
        ':type': TypeType
    }
    stack = Stack()
    while True:
        node = nodes.pop(0)

        if node.text == '(':
            stack.push(node.set_type(ExprType).set_value('()'))
            continue
        elif node.text == '[':
            stack.push(node.set_type(ListType).set_value('[]'))
            continue
        elif node.text in [')', ']']:
            node = stack.pop()
        else:
            if re.fullmatch(r'^(-|\+)?\d+(\.\d+(\^(-|\+)?\d+)?)?$', node.text, re.I) is not None:
                node.set_type(NumberType).set_value(Decimal(node.text))
            elif re.fullmatch(r'^0x[0-9A-F]+$', node.text, re.I) is not None:
                node.set_type(NumberType).set_value(Decimal(int(node.text, 16)))
            elif re.fullmatch(r'^0o[0-7]+$', node.text, re.I) is not None:
                node.set_type(NumberType).set_value(Decimal(int(node.text, 8)))
            elif re.fullmatch(r'^0b[10]+$', node.text, re.I) is not None:
                node.set_type(NumberType).set_value(Decimal(int(node.text, 2)))

            elif node.text.startswith('"') and node.text.endswith('"') or \
                 node.text.startswith("'") and node.text.endswith("'"):
                node.set_type(StringType).set_value(node.text[1:-1])
                node.value = node.value.replace('\\\\', '\\005c')
                def convert(s):
                    def _convert(o):
                        if re.fullmatch(r'^\\[0-9a-fA-F]{4}$', o.group(0)) is not None:
                            return chr(int(o.group(0)[1:], 16))
                        else:
                            check(False, 'wrong format, %s in %s' % (o.group(0), s.replace('\\005c', '\\')))

                    return _convert
                node.value = re.sub(r"\\[0-9a-zA-Z]{0,4}", convert(node.value), node.value)

            elif node.text.startswith('&'):
                node.set_type(ErrorType).set_value(node.text)
            elif node.text.startswith('#'):
                if node.text == '#true':
                    node = NodeTrue
                elif node.text == '#false':
                    node = NodeFalse
                elif node.text == '#none':
                    node = NodeNone
                elif node.text == '#args' or node.text == '#lambda':
                    node.set_type(SymbolType).set_value(node.text)
                else:
                    check(False, '%s is not a regular pre-defined value.' % (node.text,))
            elif node.text.startswith(':'):
                if node.text in Types.keys():
                    node = Types[node.text]
                else:
                    check(False, '%s is not a regular type.' % (node.text,))
            else:
                node.set_type(SymbolType).set_value(node.text)

        if stack.not_empty():
            stack.top().append_sub_node(node)
        else:
            return node

class InterpError(Exception):pass

def check(cond, msg):
    if not cond:
        raise InterpError(msg)
    else:
        pass
import_paths_stack = Stack()
import_paths_stack.push(__file__)
def Eval(node, env):
    retval = None
    node_type = node.type

    if node_type is SymbolType:
        retval = env.lookup(node.value)
        check(retval is not None, 'error, name %s not exist.' % (node.value, ))


    elif node_type in AtomicTypes:
        retval = node

    elif node_type is ListType:
        retval = CreateListNode([Eval(sub_node, env) for sub_node in node.sub_nodes])

    elif node_type is ExprType:
        check(node.get_length() >= 1, 'error, in () should not be empty.')

        op_node = node.get_op_node()
        # check(op_node.type is SymbolType, 'error, say (a b), a.type should be :symbol')

        op_node = Eval(op_node, env)
        check(op_node.type is LambdaType, 'error, in () need :lambda type.')

        op = op_node.value
        L = node.get_param_nodes()

        if op == '__lambda__':
            lambda_node = op_node

            lambda_param = lambda_node[0]
            lambda_expr = lambda_node[1]
            lambda_env = lambda_node[2]

            env0 = Env(lambda_env)
            lambda_param_num = lambda_param.get_length()
            check(lambda_param_num in (0, len(L)), '(__lambda__): 缺少參數\n%s\n需要 %s 個' % (node.get_expr(), lambda_param_num))
            params = []

            # 求調用參數求值
            for param in L:
                params.append(Eval(param, env))

            for i, p1 in enumerate(lambda_param):
                param_value_node = params[i]
                param_name = p1.value
                env0.define(param_name, param_value_node)

            env0.define('#args', CreateListNode(params))
            env0.define('#lambda', lambda_node)

            retval = Eval(lambda_expr, env0)

        elif op == 'define':
            check(len(L) == 2, '(define) need TWO params.')
            check(L[0].type is SymbolType, '(define) need :symbol type as first param.')
            retval = Eval(L[1], env)
            env.define(L[0].value, retval)

        elif op == 'update':
            check(len(L) == 2, '(update) need TWO params.')
            check(L[0].type is SymbolType, '(update) need :symbol type as first param.')
            retval = Eval(L[1], env)
            check(env.update(L[0].value, retval), '(update) name "%s" not exist or the new type not match.' % (L[0].value))

        elif op == 'cond':
            retval = NodeNone

            for node in L:
                condition_node, value_node = node.sub_nodes
                cond = Eval(condition_node, env)

                check(cond.type is BoolType, '(cond), lvalue should be :bool type.')

                if cond.value:
                    retval = Eval(value_node, env)
                    break

        elif op == 'eq':
            retval = CreateBoolNode(Eval(L[0], env) == Eval(L[1], env))

        elif op == 'lambda':
            retval = CreateLambdaNode(L[0], L[1], env)

        elif op == 'progn':
            for node in L:
                retval = Eval(node, env)

        elif op == 'if':
            bool_node = Eval(L[0], env)
            check(bool_node.type is BoolType, '(if): condition result should be :bool type.')

            if bool_node.value:
                retval = Eval(L[1], env)
            else:
                if len(L) == 3:
                    retval = Eval(L[2], env)
                else:
                    retval = NodeNone

        elif op == 'input':
            retval = CreateStringNode(input())

        elif op == 'output':
            p = Eval(L[0], env)
            print(p, end='')
            retval = p

        elif op == 'apply':
            _op = Eval(L[0], env)
            param = Eval(L[1], env)
            check(param.type is ListType, '')
            _L = [_op]
            _L.extend(param)
            retval = Eval(CreateEvalNode(_L), env)

        elif op == 'quote':
            def quote(node):
                # if node.type is SymbolType:
                #     _node = env.lookup(node.value)
                #     if _node is None:
                #         return node.get_expr()
                #     else:
                #         return quote(_node, env)
                # elif node.type is StringType:
                if node.type is StringType:
                    return '(decode %s)' % (CreateListNode([CreateNumberNode(ord(c)) for c in node.value]).get_expr(), )
                elif node.type is LambdaType:
                    if node.value in meta_lambda:
                        return node.get_expr()
                    else:
                        return '(lambda %s %s)' % (node[0].get_expr(), quote(node[1]))
                elif node.type is ExprType:
                    fragments = []
                    for sub_node in node.sub_nodes:
                        fragments.append(quote(sub_node))
                    return '(%s)' % (' '.join(fragments), )

                elif node.type is ListType:
                    fragments = []
                    for sub_node in node.sub_nodes:
                        fragments.append(quote(sub_node))
                    return '[%s]' % (' '.join(fragments), )
                else:
                    return node.get_expr()

            retval = CreateListNode([CreateStringNode(quote(sub_node)) for sub_node in L])

        elif op == 'string':
            retval = CreateStringNode(str(Eval(L[0], env)))

        elif op == 'import':
            path = os.path.join(os.path.dirname(import_paths_stack.top()), L[0].value)

            import_paths_stack.push(path)

            env0 = Env()
            try:
                code = open(path, encoding='utf-8').read()

                Eval(Parse(Clean(Validate(Lex('(progn %s)' % (code,))))), env0)
                if len(L) == 2:
                    for k in env0.items.keys():
                        env0.items[L[1].value+'-'+k] = env0.items[k]
                        del env0.items[k]

                env0.parent = env.parent
                env.parent = env0
                retval = NodeNone
            except FileNotFoundError as e:
                check(False, '(import) no such file.')
            except Exception as e:
                check(False, e)
            finally:
                import_paths_stack.pop()

        elif op == 'let':
            env0 = Env(env)
            for pair in L[0]:
                symbol, value = pair
                env0.define(symbol.value, Eval(value, env))
            retval = Eval(L[1], env0)

        elif op == 'eval':
            p = Eval(L[0], env)
            assert p.type is StringType, '(eval): 參數1 須為 :string 類型'
            try:
                retval = Eval(Parse(Clean(Validate(Lex(p.value)))), env)
            except InterpError:
                retval = CreateErrorNode()

        elif op == 'type':
            retval = Eval(L[0], env).type

        elif op == 'pass':
            retval = NodeNone

        elif op == 'trap':
            pass

        elif op == 'assert':
            cond_node = Eval(L[0], env)
            assert cond_node is NodeTrue, '%s' % (node.get_expr(), )
            retval = NodeNone

        elif op == 'exit':
            exit()

        ##########
        #
        elif op == 'get':
            L_node = Eval(L[0], env)
            I_node = Eval(L[1], env)
            assert L_node.type is ListType, '(get): 參數1 須為 :list 類型'
            assert I_node.type is NumberType, '(get): 參數索引 須為 :number 類型'

            i = int(I_node.value)
            if i < 0:
                i = i + L_node.get_length() + 1
            assert 1 <= i <= L_node.get_length(), '(get): 索引值須大於等於1且小於等於列表長度'

            retval = L_node[i - 1]

        elif op == 'set':
            L_node = Eval(L[0], env)
            I_node = Eval(L[1], env)
            V_node = Eval(L[2], env)
            assert L_node.type is ListType, '(set): 參數1 須為 :list 類型'
            assert I_node.type is NumberType, '(set): 參數索引 須為 :number 類型'

            i = int(I_node.value)
            assert 1 <= i <= L_node.get_length(), '(set): 索引值須大於等於1且小於等於列表長度'

            v = L_node[i - 1]
            assert v.type is V_node.type, '(set): 新值須為 %s 類型' % (str(v.type), )

            L_node[i - 1] = V_node

            retval = L_node

        elif op == 'insert':
            L_node = Eval(L[0], env)
            I_node = Eval(L[1], env)
            V_node = Eval(L[2], env)
            assert L_node.type is ListType, '(insert): 參數1 須為 :list 類型'
            assert I_node.type is NumberType, '(insert): 參數索引 須為 :number 類型'
            assert L_node is not V_node, '(insert): 避免循環引用'

            i = int(I_node.value)
            assert -L_node.get_length()-1 <= i <= -1 or 1 <= i <= L_node.get_length()+1, '(insert): 索引值越界'

            if i < 0:
                i = i + L_node.get_length() + 1
            else:
                i = i - 1

            L_node.sub_nodes.insert(i, V_node)

            retval = L_node

        elif op == 'delete':
            L_node = Eval(L[0], env)
            I_node = Eval(L[1], env)
            assert L_node.type is ListType, '(delete): 參數1 須為 :list 類型'
            assert I_node.type is NumberType, '(delete): 參數索引 須為 :number 類型'

            i = int(I_node.value)
            if i < 0:
                i = i + L_node.get_length() + 1
            assert 1 <= i <= L_node.get_length(), '(delete): 索引值須在正確區間之內'

            L_node.sub_nodes.pop(i - 1)

            retval = L_node

        elif op == 'length':
            L_node = Eval(L[0], env)
            assert L_node.type is ListType, '(length): 參數1 須為 :list 類型'
            retval = CreateNumberNode(L_node.get_length())

        elif op == 'copy':
            L_node = Eval(L[0], env)
            assert L_node.type is ListType, '(dup): 參數1 須為 :list 類型'
            retval = CreateListNode(L_node.sub_nodes.copy())

        ##########

        elif op == 'encode':
            p = Eval(L[0], env)
            assert p.type is StringType, '(unicode): 參數1 須為 :string 類型'
            retval = CreateListNode([CreateNumberNode(ord(c)) for c in p.value])

        elif op == 'decode':
            p = Eval(L[0], env)
            assert p.type is ListType, '(unicode): 參數1 須為 :list 類型'
            retval = CreateStringNode(''.join([chr(int(sub_node.value)) for sub_node in p.sub_nodes]))

        ##########
        elif op == 'trunc':
            p = Eval(L[0], env)
            assert p.type is NumberType, '(trunc): 參數1 須為 :number 類型'

            retval = CreateNumberNode(int(p.value))

        elif op in ['add', 'sub', 'mul', 'div']:
            p1 = Eval(L[0], env)
            p2 = Eval(L[1], env)

            assert p1.type is NumberType and p2.type is NumberType, '(%s): 參數須為 :number 類型' % (op, )

            retval = CreateNumberNode({
                'add': lambda a, b: a + b,
                'sub': lambda a, b: a - b,
                'mul': lambda a, b: a * b,
                'div': lambda a, b: a / b,
            }[op](p1.value, p2.value))

        elif op in ['gt', 'lt']:
            p1 = Eval(L[0], env)
            p2 = Eval(L[1], env)

            assert p1.type is NumberType and p2.type is NumberType, '(%s): 參數須為 :number 類型' % (op, )

            retval = CreateBoolNode({
                'gt': lambda a, b: a > b,
                'lt': lambda a, b: a < b,
            }[op](p1.value, p2.value))

        else:
            pass
    else:
        pass # unexpected

    return retval

def Interp():
    while True:
        env = Env()
        env.define('_', NodeNone)
        while True:
            try:
                s = input('[REPL]<<< ').strip()

                if s == '':
                    continue
                elif s == '(reload)':
                    break
                else:
                    retval = Eval(Parse(Clean(Validate(Lex(s)))), env)
                    env.define('_', retval)

                    print('[REPL]>>> ', retval.get_expr(), sep='')

            except (InterpError, AssertionError) as err:
                print(err)
                continue

            except KeyboardInterrupt:
                print('use (exit) to exit.')
                continue

            finally:
                print()     # 輸出一個格外的空行

def Execute(path):
    code_string = open(path, encoding='utf-8').read()
    try:
        Eval(Parse(Clean(Validate(Lex('(progn %s)' % (code_string,))))), Env())
    except InterpError as err:
        print(err)

if __name__ == "__main__":
    Interp()