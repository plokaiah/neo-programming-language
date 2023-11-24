
import sys
import os



sys.path.insert(1,os.path.abspath(os.getcwd())+'/runtime')
sys.path.insert(1,os.path.abspath(os.getcwd())+'/compiler')
import interpreter
import lexer
path = os.path.abspath(os.getcwd())

while True:
	user_input = input('neo > ')
	if user_input.strip() == "": continue
	result,error = interpreter.run('<stdin>', user_input, path)
	print(result)

