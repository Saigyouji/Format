import re
import random
import string

line_sep0 = '|'
line_sep1 = '\n'

def _eval_lisp(codes):
    # need to confirm that codes is a single expression

    with open('prog.til', 'w+') as fp:
        for line in codes:
            print(line, file=fp)


class InterpretError(Exception):
    def __init__(self, msg, lineno):
        self.msg = msg
        self.lineno = lineno


class LetNode:
    def __init__(self, pars, body):
        self.pars = pars
        self.body = body

    def code(self):
        if len(self.pars) == 0:
            return self.body.code() if isinstance(self.body, LetNode) else self.body

        args = ""
        for k, v in self.pars:
            args += "(%s %s)" % (k, v.code() if isinstance(v, LetNode) else v)

        body = self.body.code() if isinstance(self.body, LetNode) else self.body
        #print("!"+body)
        return "(let (%s) \n %s)" % (args, body)


class State:
    def __init__(self, name, code, node, lineno):
        self.name = name
        self.node = node
        self.code = code
        self.lineno = lineno


def find_all(s, ch):
    pos = []
    for i in range(len(s)):
        if s[i] == ch[0] and (i == 0 or s[i-1] != '\\'):
            pos.append(i)
    return pos


def find_all_set(s, st):
    pos = []
    for i in range(len(s)):
        if (s[i] in st) and (i == 0 or s[i-1] != '\\'):
            pos.append(i)
    return pos


rand_count = 0


def random_name():
    global rand_count
    rand_count += 1
    rand_str = ''.join(random.choice(string.ascii_letters + string.digits) for _ in range(10))
    return "%s%d" % (rand_str, rand_count)


def is_var_name(name):
    invalid_set = {'(', ')', ' ', '\n'}

    for i in range(len(name)):
        if name[i] in invalid_set:
            return False
    return True


def eval_line(line, lineno):
    line = line.strip()
    pos = find_all(line, '$')
    if len(pos) == 2 and line[0] == '$' and line[-1] == '$':
        expr = line[1:-1].strip()
        if len(expr) == 0:
            raise InterpretError("Code between $ can not be empty", lineno)
        return expr
    else:
        if len(pos) % 2 != 0:
            raise InterpretError("Character $ does not match", lineno)

        body = ""
        last_pos = 0

        name_list = []
        arg_list = []
        arg_str = ""

        for i in range(0, len(pos), 2):
            l, r = pos[i], pos[i+1]
            expr = line[l+1:r].strip()
            if len(expr) == 0:
                raise InterpretError("Code between $ can not be empty", lineno)
            body += line[last_pos:l] + "%s"

            if expr[0] == '(':
                name = random_name()
                name_list.append(name)
                arg_list.append((name, expr))

            else:
                if is_var_name(expr):
                    name = expr
                else:
                    raise InterpretError("Expression between $ is not valid", lineno)

            arg_str += name + " "
            last_pos = r+1

        body += line[last_pos:]

        if len(arg_str) == 0:
            body = "\"%s\"" % body
        else:
            body = "(format \"%s\" %s)" % (body, arg_str)

        return LetNode(arg_list, body).code()


def eval_env(prog, lineno):
    #assert prog

    print("!!!"+prog)

    head, body, nil = prog.split(']')
    _lineno = lineno + len(find_all(head, line_sep1))
    head = head[1:]
    env_name = head.split(' ')[0]

    body, tname = body.split('[')

    if tname != "/" + env_name:
        raise InterpretError("Environment name not match", lineno)

    head = head[len(env_name):]
    head = head.replace(line_sep0, ' ')
    head = head.replace(line_sep1, ' ')

    # TODO: Process '\='

    if head.count('=') > 0:
        seq = head.split('=')
        arg1 = [[seq[0].strip(), ""]]

        for i in range(1, len(seq) - 1):
            arg = seq[i].split(' ')[-1]
            arg1.append([arg.strip(), ""])
            arg1[i - 1][1] = seq[i][:-len(arg)].strip()

        arg1[-1][1] = seq[-1].strip()
    else:
        arg1 = []

    arg_str = ""

    for k, v in arg1:
        arg_str += "(%s %s) " % (k, v)

    func_name = random_name()

    sep_type = []

    for i in range(len(body)):
        if body[i] == line_sep0:
            sep_type.append(0)
        elif body[i] == line_sep1:
            sep_type.append(1)

    body.replace(line_sep1, line_sep0)
    lines = body.split(line_sep0)
    lineno = _lineno

    arg2 = []
    fcall = "(%s (" % func_name

    for line in lines:
        line = line.strip()
        if len(line) > 0:
            var_name = random_name()
            var_val = eval_line(line, lineno)
            arg2.append([var_name, var_val])
            fcall += var_name + " "
    fcall += "))"

    node = LetNode(arg2, fcall)
    return LetNode([[func_name, "(%s (%s))" % (env_name, arg_str)]], node)


# (let f func args )


def interpret(prog):
    lineno = 1

    prog = prog.strip()

    if prog[0] != '[':
        raise InterpretError("FMT code should begin with [", 1)

    stk = [State("", "", LetNode([], ""), 0)]

    brackets = find_all_set(prog, {'[', ']'})

    last_pos = 0

    for i in range(0, len(brackets), 2):
        if i+1 >= len(brackets) or prog[brackets[i+1]] != ']':
            raise InterpretError('Brackets [ not match', lineno)

        l, r = brackets[i], brackets[i + 1]
        lineno += len(find_all(prog[last_pos:l], line_sep1))
        info = prog[l+1:r].strip()
        env_name = info.split()[0]

        _lineno = lineno + len(find_all(prog[l:r], line_sep1))

        if env_name[0] == '/':
            if stk[-1].name != env_name[1:]:
                raise InterpretError('Environment name not match', _lineno)

            stk[-1].code += prog[last_pos:r+1]
            node = stk[-1].node
            node.body = eval_env(stk[-1].code, stk[-1].lineno)
            stk.pop()

            node_name = random_name()
            stk[-1].node.pars.append([node_name, node])
            stk[-1].code +='$%s$' % node_name

        else:
            stk.append(State(env_name, prog[l:r+1], LetNode([], ""), lineno))
        last_pos = r+1

        lineno = _lineno

    if len(stk) != 1:
        raise InterpretError('Environment not ended', lineno)

    node = stk[0].node
    node.body = eval_line(stk[0].code, 1)

    return node.code()


if __name__ == '__main__':
    print(eval_env("[itemize taa = 2] $(func 233  333)$ | qwqwqwq | qq $ qwq $ qwq  [/itemize]", 1).code())

    print(eval_env("[aa qaq = 233 qq=(foo q)] [/aa]", 1).code())
    print(eval_line("Of course that $(hahaha)$ should be", 1))
    print(eval_line("aaa $hi$ aaa", 1))

    print(interpret('[itemize] [item] aa [/item] | [item] bb [/item] [/itemize]'))