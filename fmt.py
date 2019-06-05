import re
import random
import string
import os
import sys

line_sep0 = '|'
line_sep1 = '\n'

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
        # print("!"+body)
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
        if s[i] == ch[0] and (i == 0 or s[i - 1] != '\\'):
            pos.append(i)
    return pos


def find_all_set(s, st):
    pos = []
    for i in range(len(s)):
        if (s[i] in st) and (i == 0 or s[i - 1] != '\\'):
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


def match_pos(s):
    sum = 0
    for i in range(len(s)):
        if s[i] == '(': sum += 1
        if s[i] == ')': sum -= 1

        if sum == 0: return i
    return -1


def parse_list(s):
    s = s.strip()
    if s == "": return []
    if s[0] != '(':
        if s.count(' ') == 0:
            return [s]
        else:
            head, tail = re.split(' ', s, 1)
            return [head] + parse_list(tail)

    pos = match_pos(s)
    if pos == -1:
        raise InterpretError('Bracket Not Match', 1)

    if pos + 1 == len(s):
        return [parse_list(s[1:pos])]
    else:
        return [parse_list(s[1:pos])] + parse_list(s[pos + 1:])


def print_list(l):
    if isinstance(l, str):
        return l
    s = "("
    for i in l:
        s += print_list(i) + " "
    s = s.strip() + ')'
    return s


env2struct = dict()


def eval_defn_env(defn, head, body):
    env_name = defn.split(' ')[1]
    rem = re.split(' ', defn, 2)[2]
    
    par_list = parse_list(rem)
    #print("rem %s %s" % (rem, par_list))
    #print(rem)

    if len(par_list) != 2:
        raise InterpretError("Env %s error: Parameter list error" % env_name, 1)

    arg_list1 = par_list[0]
    arg_list2 = par_list[1]

    if len(arg_list2) > 0:
        if len(arg_list2) == 1 or len(arg_list2[1]) == 0:
            raise InterpretError("Env %s error: Parameter 2 format error" % env_name, 1)
        if arg_list2[1][0] != 'list':
            raise InterpretError("Env %s error: Parameter 2 can only be list type" % env_name, 1)

    #print("???" + str(arg_list1))

    arg_list1_str = ""

    arg_type = random_name()
    env2struct[env_name] = {"name": arg_type, "kv": [], "arg-type": "void" if len(arg_list2) == 0 else arg_list2[1][1]}

    pars = random_name()

    for k, v in arg_list1:
        #print("defff %s %s" % (str(v), print_list(v)))
        env2struct[env_name]["kv"].append((k, print_list(v)))
        arg_list1_str += "(define %s %s)\n" % (k, print_list(v))
        head = "(define %s (%s-get-%s %s))\n" % (k, arg_type, k, pars) + head
    # print("???" + str(v) + print_list(v))

    # format: arg1 arg2

    arg_list2_str = "" if len(arg_list2) == 0 else "(%s (template (%s) list))" % (arg_list2[0], arg_list2[1][1]) #print_list(arg_list2)
    #print("sss %s %s" % (arg_list2_str, arg_list2[1][1]))

    if len(arg_list1) > 0:
        return "(define-struct %s %s) \n\
(define (%s (%s %s)) auto \n\
(begin %s (lambda (%s) auto %s)))" % \
               (arg_type, arg_list1_str, env_name, pars, arg_type, head, arg_list2_str, body)
    else:
        env2struct[env_name]["name"] = ""
        return "(define (%s) auto \n (begin %s (lambda (%s) auto %s)))" % (env_name, head, arg_list2_str, body)


def eval_lib(content, lib_name):
    #print("!lib" + lib_name)
    pos = find_all_set(content, {'[', ']'})

    res = ""

    if len(pos) % 4 != 0:
        raise InterpretError("Library %s error: Bracket number error" % lib_name, 1)

    stk = []

    lst = 0

    def proc(s):
        return s.replace("\\[", '[').replace("\\]", "]").strip()

    for i in range(0, len(pos), 2):
        if content[pos[i]] != '[' or content[pos[i + 1]] != ']':
            raise InterpretError("Library %s error: Bracket not match" % lib_name, 1)

        cs = proc(content[pos[i] + 1:pos[i + 1]])

        if len(cs) == 0:
            raise InterpretError("Library %s error: [] found" % lib_name, 1)

        if cs[0] == '/':
            name = cs[1:].strip()
            if stk[-1]["name"] != name:
                raise InterpretError("Library %s error: begin end not match" % lib_name, 1)

            if name == "def-env":
                if not ("body" in stk[-1].keys()):
                    raise InterpretError("Library %s error: Missing body of environment definition", 1)

                res += eval_defn_env(stk[-1]["defn"], stk[-1].get("head", ""), stk[-1]["body"])
            else:
                if len(stk) != 2:
                    raise InterpretError("Library %s error: Level error" % lib_name, 1)
                ct = proc(content[stk[-1]["pos"] + 1:pos[i]])
                stk[-2][name] = ct.strip()

            stk.pop()
        else:
            name = cs.split(' ')[0]
            if len(stk) == 0:
                res += proc(content[lst:pos[i]])
            stk.append({"pos": pos[i + 1], "name": name, "defn": cs})
        lst = pos[i+1]+1
    res += proc(content[lst:])

    return res


def eval_line(line, lineno):
    line = line.strip()
    #print("line %s" % line)
    pos = find_all(line, '#')
    if len(pos) == 2 and line[0] == '#' and line[-1] == '#':
        expr = line[1:-1].strip()
        if len(expr) == 0:
            raise InterpretError("Code between # can not be empty", lineno)
        return expr
    else:
        if len(pos) % 2 != 0:
            raise InterpretError("Character # does not match", lineno)

        body = ""
        last_pos = 0

        name_list = []
        arg_list = []
        arg_str = ""

        for i in range(0, len(pos), 2):
            l, r = pos[i], pos[i + 1]
            expr = line[l + 1:r].strip()
            if len(expr) == 0:
                raise InterpretError("Code between # can not be empty", lineno)
            body += line[last_pos:l] + "%s"

            if expr[0] == '(':
                name = random_name()
                name_list.append(name)
                arg_list.append((name, expr))

            else:
                if is_var_name(expr):
                    name = expr
                else:
                    raise InterpretError("Expression between # is not valid", lineno)

            arg_str += name + " "
            last_pos = r + 1

        body += line[last_pos:]
        body = body.replace('\\', '\\\\').replace("\"", "\\\"")

        if len(arg_str) == 0:
            body = "\"%s\"" % body
        else:
            body = "(format \"%s\" %s)" % (body, arg_str)

        return LetNode(arg_list, body).code()


def eval_env(prog, lineno):
    # assert prog

    # print("!!!"+prog)

    _pos = find_all(prog, ']')
    head = prog[:_pos[0]]
    body = prog[_pos[0]+1:_pos[1]]

    #head, body, nil = re.split(r'[^\\]\]', prog) #prog.split(']')
    _lineno = lineno + len(find_all(head, line_sep1))
    head = head[1:]
    env_name = head.split(' ')[0]

    _pos = find_all(body, '[')
    tname = body[_pos[0]+1:]
    body = body[:_pos[0]].replace('\\[', '[').replace('\\]', ']')

    #body, tname = re.split(r'[^\\]\[', body) #body.split('[')

    if tname != "/" + env_name:
        #print(tname, env_name)
        raise InterpretError("Environment name not match", lineno)
    if not (env_name in env2struct.keys()):
        raise InterpretError("Environment not found", lineno)

    head = head[len(env_name):]
    head = head.replace(line_sep0, ' ')
    head = head.replace(line_sep1, ' ')



    # TODO: Process '\='

    if head.count('=') > 0:
        seq = head.split('=')
        arg1 = [[seq[0].strip(), ""]]

        for i in range(1, len(seq) - 1):
            seq[i] = seq[i].strip()
            arg = seq[i].split(' ')[-1]
            # print("seq[%d]=%s %s" % (i, seq[i], seq[i].split(' ')))
            arg1.append([arg.strip(), ""])
            arg1[i - 1][1] = seq[i][:-len(arg)].strip()

        arg1[-1][1] = seq[-1].strip()
    else:
        arg1 = []

    func_name = random_name()

    sep_type = []

    for i in range(len(body)):
        if body[i] == line_sep0:
            sep_type.append(0)
        elif body[i] == line_sep1:
            sep_type.append(1)

    body = body.replace(line_sep1, line_sep0)
    lines = body.split(line_sep0)
    lineno = _lineno



    arg2 = []
    arg2type = env2struct[env_name]["arg-type"]

    if arg2type == "void":
        if len(arg2) > 0:
            raise InterpretError("Expected void type, found list", lineno)
        fcall = "(%s)" % func_name
    else:
        typename = arg2type

        fcall = "(%s (list (%s) " % (func_name, typename)

        for line in lines:
            line = line.strip()
            if len(line) > 0:
                var_name = random_name()
                var_val = eval_line(line, lineno)
                arg2.append([var_name, var_val])
                fcall += var_name + " "
        fcall += "))"

    node = LetNode(arg2, fcall)

    tmp_dict = env2struct[env_name]


    if tmp_dict["name"] != "":
        tmp_var = random_name()

        arg_str = ""

        for k, v in arg1:
            #print("arg1 %s %s" % (k, v))
            arg_str += "(set! %s (%s-set!-%s %s %s)) " % (tmp_var, tmp_dict["name"], k, tmp_var, v)

        # (begin (define %s (%s)) %s %s) % (tmp_var, tmp_dict["name"], arg_str, tmp_var)
        env_call = "(%s (begin (define %s (%s)) %s %s))" % (env_name, tmp_var, tmp_dict["name"], arg_str, tmp_var)
    else:
        env_call = "(%s)" % env_name

    return LetNode([[func_name, env_call]], node)


# (let f func args )


def interpret(prog):
    #print(prog)
    lineno = 1

    prog = prog.strip().replace('\r', '')

    prog_lines = prog.split('\n')

    include_field = ""

    for _lineno, line in enumerate(prog_lines):
        if line.startswith('%import'):
            tmp = line.split()
            if len(tmp) != 2:
                raise InterpretError('Import format: %import <name>', _lineno + 1)
            else:
                if os.path.exists('./%s.fmt' % tmp[1]):
                    with open("%s.fmt" % tmp[1]) as fp:
                        include_field += eval_lib(''.join(fp.readlines()), tmp[1]) + '\n'
                else:
                    raise InterpretError('%s.fmt not found' % tmp[1], _lineno + 1)
        else:
            lineno = _lineno
            break

    prog = '\n'.join(prog_lines[lineno:]).strip()
    lineno += 1

    # print("!!prog" + prog)
    # print(prog)
    if prog[0] != '[':
        raise InterpretError("FMT code should begin with [", 1)

    stk = [State("", "", LetNode([], ""), 0)]

    brackets = find_all_set(prog, {'[', ']'})

    last_pos = 0

    for i in range(0, len(brackets), 2):
        if i + 1 >= len(brackets) or prog[brackets[i + 1]] != ']':
            raise InterpretError('Brackets [ not match', lineno)

        l, r = brackets[i], brackets[i + 1]
        lineno += len(find_all(prog[last_pos:l], line_sep1))
        info = prog[l + 1:r].strip()
        env_name = info.split()[0]

        _lineno = lineno + len(find_all(prog[l:r], line_sep1))

        if env_name[0] == '/':
            if stk[-1].name != env_name[1:]:
                #print(stk[-1].name, env_name[1:])
                raise InterpretError('Environment name not match', _lineno)

            stk[-1].code += prog[last_pos:r + 1]
            node = stk[-1].node
            node.body = eval_env(stk[-1].code, stk[-1].lineno)
            stk.pop()

            node_name = random_name()
            stk[-1].node.pars.append([node_name, node])
            stk[-1].code += '#%s#\n' % node_name

        else:
            stk[-1].code += prog[last_pos:l]
            stk.append(State(env_name, prog[l:r + 1], LetNode([], ""), lineno))
        last_pos = r + 1

        lineno = _lineno

    if len(stk) != 1:
        raise InterpretError('Environment not ended', lineno)

    node = stk[0].node
    node.body = eval_line(stk[0].code, 1)

    return include_field + node.code()


def compile(file, target):
    if os.path.exists(file):
        with open(file, 'r') as fp:
            with open(target, 'w') as fw:
                lines = fp.readlines()
                #print(lines)
                # print(lines)
                print(interpret(''.join(lines)), file=fw)
    else:
        print('%s not found' % file)


if __name__ == '__main__':
    #print(eval_env("[itemize taa = 2] #(func 233  333)# | qwqwqwq | qq # qwq # qwq  [/itemize]", 1).code())

    #print(eval_env("[aa qaq = 233 qq=(foo q)] [/aa]", 1).code())
    #print(eval_line("Of course that #(hahaha)# should be", 1))
    #print(eval_line("aaa #hi# aaa", 1))
    #print(parse_list("((l (list string)) (i number)) (a number)"))
    #print(interpret('[itemize] [item] aa [/item] | [item] bb [/item] [/itemize]'))

    #print("%s" % parse_list("(l (list (string)))"))
    if len(sys.argv) != 3:
        print("Usage: python fmt.py fmt-file target-file")
    else:
        compile(sys.argv[1], sys.argv[2])
    #compile('slide.fmt', 'slide.lisp')

