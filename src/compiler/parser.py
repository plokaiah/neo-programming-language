import lexer

#######################################
# ERRORS
#######################################

class Error:
  def __init__(self, start, end, error_name, details):
    self.start = start
    self.end = end
    self.error_name = error_name
    self.details = details
  
  def as_string(self):
    result  = f'{self.error_name}: {self.details}\n'
    result += f'File {self.start.func}, line {self.start.length + 1}'
    result += '\n\n' + strn_arr(self.start.text, self.start, self.end)
    return result

class InvalidSyntaxError(Error):
  def __init__(self, start, end, details=''):
    super().__init__(start, end, 'Syntax is Invalid', details)

  def as_string(self):
    result  = self.trcbk_gen()
    result += f'{self.error_name}: {self.details}'
    result += '\n\n' + strn_arr(self.start.text, self.start, self.end)
    return result

  def trcbk_gen(self):
    result = ''
    position = self.start
    ctx = self.context

    while ctx:
      result = f'  File {position.func}, line {str(position.length + 1)}, in {ctx.display_name}\n' + result
      position = ctx.parent_entry_pos
      ctx = ctx.parent

    return 'Traceback (most recent call last):\n' + result


def strn_arr(text, start, end):
	result = ''

	# Calculate indices
	index_start = max(text.rfind('\n', 0, start.index), 0)
	index_end = text.find('\n', index_start + 1)
	if index_end < 0: index_end = len(text)
	
	# Generate each line
	line_count = end.ln - start.ln + 1
	for i in range(line_count):
		# Calculate line clmnumns
		line = text[index_start:index_end]
		clmn_begin = start.clmn if i == 0 else 0
		clmn_last = end.clmn if i == line_count - 1 else len(line) - 1

		# Append to result
		result += line + '\n'
		result += ' ' * clmn_begin + '^' * (clmn_last - clmn_begin)

		# Re-calculate indices
		index_start = index_end
		index_end = text.find('\n', index_start + 1)
		if index_end < 0: index_end = len(text)

	return result.replace('\t', '')




#######################################
# PARSER
#######################################

class Parser:
  def __init__(self, tokenList):
    self.tokenList = tokenList
    self.index = -1
    self.next()

  def next(self):
    self.index += 1
    self.update_token_now()
    return self.token_now

  def reverse(self, amount=1):
    self.index -= amount
    self.update_token_now()
    return self.token_now

  def update_token_now(self):
    if self.index >= 0 and self.index < len(self.tokenList):
      self.token_now = self.tokenList[self.index]

  def parse(self):
    res = self.lines()
    if not res.error and self.token_now.type != lexer.Neo_FuncEnd:
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        "Token cannot appear after previous tokenList"
      ))
    return res

  ###################################

  def lines(self):
    res = ParseResult()
    lines = []
    start = self.token_now.start.duplicate()

    while self.token_now.type == lexer.Neo_Nline:
      res.register_advancement()
      self.next()
    line = res.register(self.line())
    if res.error: return res
    lines.append(line)
    more_lines = True

    while True:
      newline_count = 0
      while self.token_now.type == lexer.Neo_Nline:
        res.register_advancement()
        self.next()
        newline_count += 1
      if newline_count == 0:
        more_lines = False
      
      if not more_lines: break
      line = res.try_register(self.line())
      if not line:
        self.reverse(res.to_reverse_count)
        more_lines = False
        continue
      lines.append(line)

    return res.success(ListNode(
      lines,
      start,
      self.token_now.end.duplicate()
    ))

  def line(self):
    res = ParseResult()
    start = self.token_now.start.duplicate()

    if self.token_now.compares(lexer.Neo_Keyword, 'RETURN'):
      res.register_advancement()
      self.next()

      expr = res.try_register(self.expr())
      if not expr:
        self.reverse(res.to_reverse_count)
      return res.success(ReturnNode(expr, start, self.token_now.start.duplicate()))
    
    if self.token_now.compares(lexer.Neo_Keyword, 'CONTINUE'):
      res.register_advancement()
      self.next()
      return res.success(ContinueNode(start, self.token_now.start.duplicate()))
      
    if self.token_now.compares(lexer.Neo_Keyword, 'BREAK'):
      res.register_advancement()
      self.next()
      return res.success(BreakNode(start, self.token_now.start.duplicate()))

    expr = res.register(self.expr())
    if res.error:
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        "Expected 'RETURN', 'CONTINUE', 'BREAK', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
      ))
    return res.success(expr)

  def expr(self):
    res = ParseResult()


    if self.token_now.compares(lexer.Neo_Keyword, 'VAR'):
      res.register_advancement()
      self.next()

      if self.token_now.type != lexer.Neo_Identifier:
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          "Expected identifier"
        ))

      var_name = self.token_now
      res.register_advancement()
      self.next()

      if self.token_now.type != lexer.Neo_CheckEquals:
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          "Expected '='"
        ))

      res.register_advancement()
      self.next()
      expr = res.register(self.expr())
      if res.error: return res
      return res.success(VarAssignNode(var_name, expr))

    node = res.register(self.bin_op(self.comp_expr, ((lexer.Neo_Keyword, 'AND'), (lexer.Neo_Keyword, 'OR'))))

    if res.error:
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        "Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
      ))

    return res.success(node)

  def comp_expr(self):
    res = ParseResult()

    if self.token_now.compares(lexer.Neo_Keyword, 'NOT'):
      op_tok = self.token_now
      res.register_advancement()
      self.next()

      node = res.register(self.comp_expr())
      if res.error: return res
      return res.success(UnaryOpNode(op_tok, node))
    
    node = res.register(self.bin_op(self.arith_expr, (lexer.Neo_Equal, lexer.Neo_NotEquals, lexer.Neo_LessThan, lexer.Neo_GreaterThan, lexer.Neo_LessThanEquals,lexer.Neo_GreaterThanEquals)))
    
    if res.error:
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        "Expected int, float, identifier, '+', '-', '(', '[', 'IF', 'FOR', 'WHILE', 'FUN' or 'NOT'"
      ))

    return res.success(node)

  def arith_expr(self):
    return self.bin_op(self.term, (lexer.Neo_Add, lexer.Neo_Sub))

  def term(self):
    return self.bin_op(self.factor, (lexer.Neo_Mult, lexer.Neo_Div))

  def factor(self):
    res = ParseResult()
    tok = self.token_now

    if tok.type in (lexer.Neo_Add, lexer.Neo_Sub):
      res.register_advancement()
      self.next()
      factor = res.register(self.factor())
      if res.error: return res
      return res.success(UnaryOpNode(tok, factor))

    return self.power()

  def power(self):
    return self.bin_op(self.call, (lexer.Neo_Pow, ), self.factor)

  def call(self):
    res = ParseResult()
    atom = res.register(self.atom())
    if res.error: return res

    if self.token_now.type == lexer.Neo_LBrace:
      res.register_advancement()
      self.next()
      arg_nodes = []

      if self.token_now.type == lexer.Neo_RBrace:
        res.register_advancement()
        self.next()
      else:
        arg_nodes.append(res.register(self.expr()))
        if res.error:
          return res.failure(InvalidSyntaxError(
            self.token_now.start, self.token_now.end,
            "Expected ')', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
          ))

        if self.token_now.type != lexer.Neo_RBrace:
          return res.failure(InvalidSyntaxError(
            self.token_now.start, self.token_now.end,
            f"Expected ',' or ')'"
          ))

        res.register_advancement()
        self.next()
      return res.success(CallNode(atom, arg_nodes))
    return res.success(atom)

  def atom(self):
    res = ParseResult()
    token = self.token_now

    if token.type in (lexer.Neo_Int, lexer.Neo_Float):
      res.register_advancement()
      self.next()
      return res.success(NumberNode(token))

    elif token.type == lexer.Neo_String:
      res.register_advancement()
      self.next()
      return res.success(StringNode(token))

    elif token.type == lexer.Neo_Identifier:
      res.register_advancement()
      self.next()
      return res.success(VarAccessNode(token))

    elif token.type == lexer.Neo_LBrace:
      res.register_advancement()
      self.next()
      expr = res.register(self.expr())
      if res.error: return res
      if self.token_now.type == lexer.Neo_RBrace:
        res.register_advancement()
        self.next()
        return res.success(expr)
      else:
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          "Expected ')'"
        ))

    elif token.type == lexer.Neo_LParen:
      list_expr = res.register(self.list_expr())
      if res.error: return res
      return res.success(list_expr)
    
    elif token.compares(lexer.Neo_Keyword, 'IF'):
      if_expr = res.register(self.if_expr())
      if res.error: return res
      return res.success(if_expr)

    elif token.compares(lexer.Neo_Keyword, 'FOR'):
      for_expr = res.register(self.for_expr())
      if res.error: return res
      return res.success(for_expr)

    elif token.compares(lexer.Neo_Keyword, 'WHILE'):
      while_expr = res.register(self.while_expr())
      if res.error: return res
      return res.success(while_expr)

    elif token.compares(lexer.Neo_Keyword, 'FUN'):
      func_def = res.register(self.func_def())
      if res.error: return res
      return res.success(func_def)

    return res.failure(InvalidSyntaxError(
      token.start, token.end,
      "Expected int, float, identifier, '+', '-', '(', '[', IF', 'FOR', 'WHILE', 'FUN'"
    ))

  def list_expr(self):
    res = ParseResult()
    element_nodes = []
    start = self.token_now.start.duplicate()

    if self.token_now.type != lexer.Neo_LParen:
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected '('"
      ))

    res.register_advancement()
    self.next()

    if self.token_now.type == lexer.Neo_RParen:
      res.register_advancement()
      self.next()
    else:
      element_nodes.append(res.register(self.expr()))
      if res.error:
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          "Expected ']', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
        ))

      if self.token_now.type != lexer.Neo_RParen:
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          f"Expected ',' or ')'"
        ))

      res.register_advancement()
      self.next()

    return res.success(ListNode(
      element_nodes,
      start,
      self.token_now.end.duplicate()
    ))


  def if_expr(self):
    res = ParseResult()
    all_cases = res.register(self.if_expr_cases('IF'))
    if res.error: return res
    cases, else_case = all_cases
    return res.success(IfNode(cases, else_case))

  def if_expr_b(self):
    return self.if_expr_cases('ELIF')
    
  def if_expr_c(self):
    res = ParseResult()
    else_case = None

    if self.token_now.compares(lexer.Neo_Keyword, 'ELSE'):
      res.register_advancement()
      self.next()

      if self.token_now.type == lexer.Neo_Nline:
        res.register_advancement()
        self.next()

        lines = res.register(self.lines())
        if res.error: return res
        else_case = (lines, True)

        if self.token_now.compares(lexer.Neo_Keyword, 'END'):
          res.register_advancement()
          self.next()
        else:
          return res.failure(InvalidSyntaxError(
            self.token_now.start, self.token_now.end,
            "Expected 'END'"
          ))
      else:
        expr = res.register(self.line())
        if res.error: return res
        else_case = (expr, False)

    return res.success(else_case)

  def if_expr_b_or_c(self):
    res = ParseResult()
    cases, else_case = [], None

    if self.token_now.compares(lexer.Neo_Keyword, 'ELIF'):
      all_cases = res.register(self.if_expr_b())
      if res.error: return res
      cases, else_case = all_cases
    else:
      else_case = res.register(self.if_expr_c())
      if res.error: return res
    
    return res.success((cases, else_case))

  def if_expr_cases(self, case_keyword):
    res = ParseResult()
    cases = []
    else_case = None

    if not self.token_now.compares(lexer.Neo_Keyword, case_keyword):
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected '{case_keyword}'"
      ))

    res.register_advancement()
    self.next()

    condition = res.register(self.expr())
    if res.error: return res

    if not self.token_now.compares(lexer.Neo_Keyword, 'THEN'):
      if self.token_now.compares(lexer.Neo_Keyword, '?'):
        res.register_advancement()
        self.next()

        lines = res.register(self.lines())
        if res.error: return res
        cases.append((condition, lines, True))

        if self.token_now.compares(lexer.Neo_Keyword, ':'):
          res.register_advancement()
          self.next()

          lines = res.register(self.lines())

      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected 'THEN'"
      ))

    res.register_advancement()
    self.next()

    if self.token_now.type == lexer.Neo_Nline:
      res.register_advancement()
      self.next()

      lines = res.register(self.lines())
      if res.error: return res
      cases.append((condition, lines, True))

      if self.token_now.compares(lexer.Neo_Keyword, 'END'):
        res.register_advancement()
        self.next()
      else:
        all_cases = res.register(self.if_expr_b_or_c())
        if res.error: return res
        new_cases, else_case = all_cases
        cases.extend(new_cases)
    else:
      expr = res.register(self.line())
      if res.error: return res
      cases.append((condition, expr, False))

      all_cases = res.register(self.if_expr_b_or_c())
      if res.error: return res
      new_cases, else_case = all_cases
      cases.extend(new_cases)

    return res.success((cases, else_case))

  def for_expr(self):
    res = ParseResult()

    if not self.token_now.compares(lexer.Neo_Keyword, 'FOR'):
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected 'FOR'"
      ))

    res.register_advancement()
    self.next()

    if self.token_now.type != lexer.Neo_Identifier:
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected identifier"
      ))

    var_name = self.token_now
    res.register_advancement()
    self.next()
    
    res.register_advancement()
    self.next()

    start_value = res.register(self.expr())
    if res.error: return res

    if not self.token_now.compares(lexer.Neo_Keyword, 'TO'):
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected 'TO'"
      ))
    
    res.register_advancement()
    self.next()

    end_value = res.register(self.expr())
    if res.error: return res

    if self.token_now.compares(lexer.Neo_Keyword, 'STEP'):
      res.register_advancement()
      self.next()

      step_value = res.register(self.expr())
      if res.error: return res
    else:
      step_value = None

    if not self.token_now.compares(lexer.Neo_Keyword, 'THEN'):
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected 'THEN'"
      ))

    res.register_advancement()
    self.next()

    if self.token_now.type == lexer.Neo_Nline:
      res.register_advancement()
      self.next()

      body = res.register(self.lines())
      if res.error: return res

      if not self.token_now.compares(lexer.Neo_Keyword, 'END'):
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          f"Expected 'END'"
        ))

      res.register_advancement()
      self.next()

      return res.success(ForNode(var_name, start_value, end_value, step_value, body, True))
    
    body = res.register(self.line())
    if res.error: return res

    return res.success(ForNode(var_name, start_value, end_value, step_value, body, False))

  def while_expr(self):
    res = ParseResult()

    if not self.token_now.compares(lexer.Neo_Keyword, 'WHILE'):
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected 'WHILE'"
      ))

    res.register_advancement()
    self.next()

    condition = res.register(self.expr())
    if res.error: return res

    if not self.token_now.compares(lexer.Neo_Keyword, 'THEN'):
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected 'THEN'"
      ))

    res.register_advancement()
    self.next()

    if self.token_now.type == lexer.Neo_Nline:
      res.register_advancement()
      self.next()

      body = res.register(self.lines())
      if res.error: return res

      if not self.token_now.compares(lexer.Neo_Keyword, 'END'):
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          f"Expected 'END'"
        ))

      res.register_advancement()
      self.next()

      return res.success(WhileNode(condition, body, True))
    
    body = res.register(self.line())
    if res.error: return res

    return res.success(WhileNode(condition, body, False))

  def func_def(self):
    res = ParseResult()

    if not self.token_now.compares(lexer.Neo_Keyword, 'FUN'):
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected 'FUN'"
      ))

    res.register_advancement()
    self.next()

    if self.token_now.type == lexer.Neo_Identifier:
      var_name_tok = self.token_now
      res.register_advancement()
      self.next()
      if self.token_now.type != lexer.Neo_LBrace:
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          f"Expected '('"
        ))
    else:
      var_name_tok = None
      if self.token_now.type != lexer.Neo_LBrace:
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          f"Expected identifier or '('"
        ))
    
    res.register_advancement()
    self.next()
    arg_name_tokens = []

    if self.token_now.type == lexer.Neo_Identifier:
      arg_name_tokens.append(self.token_now)
      res.register_advancement()
      self.next()

      
      if self.token_now.type != lexer.Neo_RBrace:
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          f"Expected ',' or ')'"
        ))
    else:
      if self.token_now.type != lexer.Neo_RBrace:
        return res.failure(InvalidSyntaxError(
          self.token_now.start, self.token_now.end,
          f"Expected identifier or ')'"
        ))

    res.register_advancement()
    self.next()

    
    if self.token_now.type != lexer.Neo_Nline:
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected '->' or NEWLINE"
      ))

    res.register_advancement()
    self.next()

    body = res.register(self.lines())
    if res.error: return res

    if not self.token_now.compares(lexer.Neo_Keyword, 'END'):
      return res.failure(InvalidSyntaxError(
        self.token_now.start, self.token_now.end,
        f"Expected 'END'"
      ))

    res.register_advancement()
    self.next()
    
    return res.success(FuncDefNode(
      var_name_tok,
      arg_name_tokens,
      body,
      False
    ))

  ###################################

  def bin_op(self, func_a, ops, func_b=None):
    if func_b == None:
      func_b = func_a
    
    res = ParseResult()
    left = res.register(func_a())
    if res.error: return res

    while self.token_now.type in ops or (self.token_now.type, self.token_now.value) in ops:
      op_tok = self.token_now
      res.register_advancement()
      self.next()
      right = res.register(func_b())
      if res.error: return res
      left = BinOpNode(left, op_tok, right)

    return res.success(left)



#########################
 # Parse Result
 #######################
class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.last_registered_advance_count = 0
        self.advance_count = 0
        self.to_reverse_count = 0

    def register_advancement(self):
        self.last_registered_advance_count = 1
        self.advance_count += 1

    def register(self, res):
        self.last_registered_advance_count = res.advance_count
        self.advance_count += res.advance_count
        if res.error: self.error = res.error
        return res.node

    def try_register(self, res):
        if res.error:
            self.to_reverse_count = res.advance_count
            return None
        return self.register(res)

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        if not self.error or self.last_registered_advance_count == 0:
            self.error = error
        return self

#######################
     # NODES
 #################
class NumberNode:
  def __init__(self, token):
    self.token = token

    self.start = self.token.start
    self.end = self.token.end

  def __repr__(self):
    return f'{self.token}'

class StringNode:
  def __init__(self, token):
    self.token = token

    self.start = self.token.start
    self.end = self.token.end

  def __repr__(self):
    return f'{self.token}'

class ListNode:
  def __init__(self, element_nodes, start, end):
    self.element_nodes = element_nodes

    self.start = start
    self.end = end


class VarAccessNode:
  def __init__(self, var_name_token):
    self.var_name_token = var_name_token

    self.start = self.var_name_token.start
    self.end = self.var_name_token.end

class VarAssignNode:
  def __init__(self, var_name_token, value_node):
    self.var_name_token = var_name_token
    self.value_node = value_node

    self.start = self.var_name_token.start
    self.end = self.value_node.end

class BinOpNode:
  def __init__(self, left_node, op_token, right_node):
    self.left_node = left_node
    self.op_token = op_token
    self.right_node = right_node

    self.start = self.left_node.start
    self.end = self.right_node.end

  def __repr__(self):
    return f'({self.left_node}, {self.op_token}, {self.right_node})'

class UnaryOpNode:
  def __init__(self, op_token, node):
    self.op_token = op_token
    self.node = node

    self.start = self.op_token.start
    self.end = node.end

  def __repr__(self):
    return f'({self.op_token}, {self.node})'

class IfNode:
  def __init__(self, cases, else_case):
    self.cases = cases
    self.else_case = else_case

    self.start = self.cases[0][0].start
    self.end = (self.else_case or self.cases[len(self.cases) - 1])[0].end

class TernaryNode:
  def __init__(self, cases,else_case):
    self.cases = cases
    self.else_case = else_case

    self.start = self.cases[0][0].start
    self.end = (self.else_case or self.cases[len(self.cases) - 1])[0].end

class ForNode:
  def __init__(self, var_name_token, start_value_node, end_value_node, step_value_node, body_node, should_return_null):
    self.var_name_token = var_name_token
    self.start_value_node = start_value_node
    self.end_value_node = end_value_node
    self.step_value_node = step_value_node
    self.body_node = body_node
    self.should_return_null = should_return_null

    self.start = self.var_name_token.start
    self.end = self.body_node.end


class WhileNode:
  def __init__(self, condition_node, body_node, should_return_null):
    self.condition_node = condition_node
    self.body_node = body_node
    self.should_return_null = should_return_null

    self.start = self.condition_node.start
    self.end = self.body_node.end

class FuncDefNode:
  def __init__(self, var_name_token, arg_name_tokens, body_node, should_auto_return):
    self.var_name_token = var_name_token
    self.arg_name_tokens = arg_name_tokens
    self.body_node = body_node
    self.should_auto_return = should_auto_return

    if self.var_name_token:
      self.start = self.var_name_token.start
    elif len(self.arg_name_tokens) > 0:
      self.start = self.arg_name_tokens[0].start
    else:
      self.start = self.body_node.start

    self.end = self.body_node.end

class CallNode:
  def __init__(self, node_to_call, arg_nodes):
    self.node_to_call = node_to_call
    self.arg_nodes = arg_nodes

    self.start = self.node_to_call.start

    if len(self.arg_nodes) > 0:
      self.end = self.arg_nodes[len(self.arg_nodes) - 1].end
    else:
      self.end = self.node_to_call.end

class ReturnNode:
  def __init__(self, node_to_return, start, end):
    self.node_to_return = node_to_return

    self.start = start
    self.end = end

class ContinueNode:
  def __init__(self, start, end):
    self.start = start
    self.end = end

class BreakNode:
  def __init__(self, start, end):
    self.start = start
    self.end = end


