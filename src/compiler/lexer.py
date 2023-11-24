# Import statement
import string
import os
import sys
cwd = os.path.abspath(os.getcwd())
sys.path.insert(1,os.path.abspath(os.getcwd())+'/runtime')
import interpreter


# Declaring Constants
Numericals = '0123456789'
Alphabet = string.ascii_letters
AlphaNumericals = Alphabet + Numericals


# Declaring Keywords
Keywords = ['NOT','OR','AND','VAR','END','IF','THEN','ELSE','FOR','STEP',
 'TO', 'WHILE','FUN','BREAK','CONTINUE','RETURN']


# Declaring Gen_Tokens
Neo_Int			= 'Integer'
Neo_Float    	= 'Float'
Neo_String		= 'String'
Neo_Identifier	= 'Identifier'
Neo_Keyword		= 'Keyword'
Neo_Add     	= 'Add'
Neo_Sub      	= 'Sub'
Neo_Mult      	= 'Mult'
Neo_Div      	= 'Div'
Neo_CheckEquals		= 'CheckEquals'
Neo_LBrace   	= 'LBrace'
Neo_RBrace   	= 'RBrace'
Neo_Equal		= 'Equal'
Neo_NotEquals					= 'NotEquals'
Neo_LessThan					= 'LessThan'
Neo_GreaterThan					= 'GreaterThan'
Neo_LessThanEquals				= 'LessThanEquals'
Neo_GreaterThanEquals			= 'GreaterThanEquals'
Neo_FuncEnd				= 'FuncEnd'
Neo_Nline      = 'Nline'
Neo_Pow  = 'Npow'
Neo_LParen = 'LParen'
Neo_RParen = 'RParen'


class Gen_Token:
	def __init__(self, type, value=None, start=None, end=None):
		self.value = value
		self.type = type
		
		if start:
			self.start = start.duplicate()
			self.end = start.duplicate()
			self.end.next()

		if end:
			self.end = end.duplicate()

	def compares(self, type, value):
		return self.type == type and self.value == value
	
	def __repr__(self):
		if self.value:
			return f'{self.type}--{self.value}'
		return f'{self.type}'


# Iterating through Positions
class Position:
	def __init__(self, index, length, clmn, func, text):
		self.clmn = clmn
		self.index = index
		self.text = text
		self.length = length
		self.func = func
	
	def next(self, char_now = None):
		self.clmn += 1
		self.index += 1
		
		if char_now == '\n':
			self.clmn = 0
			self.length += 1
			
		return self

	def duplicate(self):
		return Position(self.index, self.length, self.clmn, self.func, self.text)


# Code for lexer
class Lexer:
	def __init__(self, func, user_input):
		self.user_input = user_input
		self.func = func
		self.char_now = None
		self.position = Position(-1, 0, -1, func, user_input)
		self.next()
	
	def next(self):
		self.position.next(self.char_now)
		self.char_now = self.user_input[self.position.index] if self.position.index < len(self.user_input) else None

	def tokens_generate(self):

		tokensList = []

		while self.char_now != None:
			if self.char_now in ' ':
				self.next()
			elif self.char_now in Alphabet:
				tokensList.append(self.iden_generate())
			elif self.char_now in Numericals:
				tokensList.append(self.numerical_generate())
			elif self.char_now == '"':
				tokensList.append(self.string_generate())
			elif self.char_now in ';\n':
				tokensList.append(Gen_Token(Neo_Nline, start=self.position))
				self.next()
			elif self.char_now == '&':
				tokensList.append(Gen_Token(Neo_Add, start=self.position))
				self.next()
			elif self.char_now == '-':
				tokensList.append(Gen_Token(Neo_Sub, start=self.position))
				self.next()
			elif self.char_now == '@':
				tokensList.append(Gen_Token(Neo_Mult, start=self.position))
				self.next()
			elif self.char_now == '#':
				tokensList.append(Gen_Token(Neo_Div, start=self.position))
				self.next()
			elif self.char_now == '^':
				tokensList.append(Gen_Token(Neo_Pow, start=self.position))
				self.next()
			elif self.char_now == '[':
				tokensList.append(Gen_Token(Neo_LBrace, start=self.position))
				self.next()
			elif self.char_now == ']':
				tokensList.append(Gen_Token(Neo_RBrace, start=self.position))
				self.next()
			elif self.char_now == '!':
				token, error = self.notEquals()
				if error: return [], error
				tokensList.append(token)
			elif self.char_now == '=':
				tokensList.append(self.equals_func())
			elif self.char_now == '<':
				tokensList.append(self.Less_than_func())
			elif self.char_now == '>':
				tokensList.append(self.Greater_than_func())
			elif self.char_now == '(':
				tokensList.append(Gen_Token(Neo_LParen, start=self.position))
				self.next()
			elif self.char_now == ')':
				tokensList.append(Gen_Token(Neo_RParen, start=self.position))
				self.next()    
			else:
				self.next()
				return []

		tokensList.append(Gen_Token(Neo_FuncEnd, start=self.position))
		return tokensList, None


	def string_generate(self):
		chr_escs = {
			'n': '\n',
			't': '\t'
		}

		chr_esc = False
		string = ''
		start = self.position.duplicate()
		self.next()

		
		while self.char_now != None and (self.char_now != '"' or chr_esc):
			if chr_esc:
				string += chr_escs.get(self.char_now, self.char_now)
			else:
				if self.char_now == '\\':
					chr_esc = True
				else:
					string += self.char_now
			self.next()
			chr_esc = False
		
		self.next()
		return Gen_Token(Neo_String, string, start, self.position)


	def numerical_generate(self):
		nCount = 0
		nStr = ''
		start = self.position.duplicate()

		while self.char_now != None and self.char_now in Numericals + '.':
			if self.char_now == '.':
				if nCount == 1: break
				nCount += 1
			nStr += self.char_now
			self.next()

		if nCount == 0:
			return Gen_Token(Neo_Int, int(nStr), start, self.position)
		else:
			return Gen_Token(Neo_Float, float(nStr), start, self.position)



	def iden_generate(self):
		
		start = self.position.duplicate()
		id_str = ''

		while self.char_now != None and self.char_now in AlphaNumericals + '_':
			id_str += self.char_now
			self.next()

		tokenType = Neo_Keyword if id_str in Keywords else Neo_Identifier
		return Gen_Token(tokenType, id_str, start, self.position)


	def equals_func(self):
		start = self.position.duplicate()
		tokenType = Neo_CheckEquals
		self.next()

		if self.char_now == '=':
			
			tokenType = Neo_Equal
			self.next()

		return Gen_Token(tokenType, start=start, end=self.position)


	def Greater_than_func(self):
		start = self.position.duplicate()
		tokenType = Neo_GreaterThan
		self.next()

		if self.char_now == '=':
			
			tokenType = Neo_GreaterThanEquals
			self.next()

		return Gen_Token(tokenType, start=start, end=self.position)



	def notEquals(self):
		start = self.position.duplicate()
		self.next()

		if self.char_now == '=':
			self.next()
			return Gen_Token(Neo_NotEquals, start=start, end=self.position), None

		self.next()
		return None
	
	

	def Less_than_func(self):
		start = self.position.duplicate()
		tokenType = Neo_LessThan
		self.next()

		if self.char_now == '=':
			
			tokenType = Neo_LessThanEquals
			self.next()

		return Gen_Token(tokenType, start=start, end=self.position)

	


