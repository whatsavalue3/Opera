import std.stdio;
import std.file;
import std.conv;
import std.ascii;

enum TokenType
{
	EOF,
	
	NAME,
	NUMBER,
	
	TICK,
	TILDE,
	EXCLAMATION,
	AT,
	HASH,
	DOLLAR,
	PERCENT,
	CARET,
	AMPERSAND,
	STAR,
	MINUS,
	EQUALS,
	PLUS,
	COLON,
	SEMICOLON,
	PIPE,
	COMMA,
	LESSER,
	DOT,
	GREATER,
	SLASH,
	QUESTION,
	
	PAREN_LEFT,
	PAREN_RIGHT,
	BRACKET_LEFT,
	BRACKET_RIGHT,
	BRACE_LEFT,
	BRACE_RIGHT,
}

struct Token
{
	TokenType type;
	ulong pos;
	string name;
}

TokenType[char] charToToken = [
	'`': TokenType.TICK,
	'~': TokenType.TILDE,
	'!': TokenType.EXCLAMATION,
	'@': TokenType.AT,
	'#': TokenType.HASH,
	'$': TokenType.DOLLAR,
	'%': TokenType.PERCENT,
	'^': TokenType.CARET,
	'&': TokenType.AMPERSAND,
	'*': TokenType.STAR,
	'-': TokenType.MINUS,
	'=': TokenType.EQUALS,
	'+': TokenType.PLUS,
	':': TokenType.COLON,
	';': TokenType.SEMICOLON,
	'|': TokenType.PIPE,
	',': TokenType.COMMA,
	'<': TokenType.LESSER,
	'.': TokenType.DOT,
	'>': TokenType.GREATER,
	'/': TokenType.SLASH,
	'?': TokenType.QUESTION,
	
	'(': TokenType.PAREN_LEFT,
	')': TokenType.PAREN_RIGHT,
	'[': TokenType.BRACKET_LEFT,
	']': TokenType.BRACKET_RIGHT,
	'{': TokenType.BRACE_LEFT,
	'}': TokenType.BRACE_RIGHT,
];

class Lexer
{
	string code;
	ulong i = 0;
	
	Token next()
	{
		if(i >= code.length)
		{
			return Token(type:TokenType.EOF,pos:i,name:"");
		}
		while(isWhite(code[i]))
		{
			i++;
		}
		if(code[i] in charToToken)
		{
			return Token(type:charToToken[code[i]],pos:i,name:code[i..++i]);
		}
		if(isAlpha(code[i]))
		{
			ulong first = i;
			while(isAlpha(code[i]))
			{
				i++;
			}
			return Token(type:TokenType.NAME,pos:first,name:code[first..i]);
		}
		if(isDigit(code[i]))
		{
			ulong first = i;
			while(isDigit(code[i]))
			{
				i++;
			}
			return Token(type:TokenType.NUMBER,pos:first,name:code[first..i]);
		}
		return Token(type:TokenType.EOF,pos:i,name:"");
	}
	
	bool atEnd()
	{
		return i >= code.length;
	}
	
	bool optional(TokenType type)
	{
		ulong origi = i;
		if(this.next().type == type)
		{
			return true;
		}
		i = origi;
		return false;
	}
	
}

class TokenGroup
{
	Token[] tokens;
	ulong i = 0;
	
	Token next()
	{
		if(atEnd())
		{
			return Token(type:TokenType.EOF,pos:0,name:"");
		}
		return tokens[i++];
	}
	
	bool atEnd()
	{
		return i >= tokens.length;
	}
}

enum ExpressionType
{
	Immediate,
	Name,
	Add,
	Sub
}

struct Expression
{
	bool exists = false;
	ExpressionType type;
	string value;
	Expression[] inner;
}

struct Macro
{
	MacroExpression pattern;
	Expression definition;
}

struct MacroExpression
{
	TokenGroup tokens;
}

Token[] GetUntil(Lexer l, TokenType inc, TokenType dec)
{
	ulong score = 1;
	
	Token[] tokens;
	
	while(score != 0)
	{
		Token t = l.next();
		if(t.type == dec)
		{
			score--;
		}
		else if(t.type == inc)
		{
			score++;
		}
		else if(t.type == TokenType.EOF)
		{
			return [];
		}
		tokens ~= t;
	}
	
	return tokens;
}

TokenGroup GetTokenGroup(Lexer l)
{
	TokenGroup g = new TokenGroup();
	Token t = l.next();
	
	switch(t.type)
	{
		case TokenType.PAREN_LEFT:
			g.tokens = GetUntil(l, TokenType.PAREN_LEFT,TokenType.PAREN_RIGHT);
			return g;
		case TokenType.BRACE_LEFT:
			g.tokens = GetUntil(l, TokenType.BRACE_LEFT,TokenType.BRACE_RIGHT);
			return g;
		case TokenType.BRACKET_LEFT:
			g.tokens = GetUntil(l, TokenType.BRACKET_LEFT,TokenType.BRACKET_RIGHT);
			return g;
		default:
			g.tokens ~= t;
			return g;
	}
}

MacroExpression ParseMacroExpression(Lexer l)
{
	MacroExpression me;
	me.tokens = GetTokenGroup(l);
	return me;
}

Macro ParseMacro(Lexer l)
{
	Macro m;
	m.pattern = ParseMacroExpression(l);
	m.definition = ParseExpression(l);
	return m;
}

struct Type
{
	Expression size;
	
}


struct Place
{
	string name;
	Type type;
	bool constant = false;
	Expression default_value;
}

Type ParseType(Lexer l)
{
	Token token = l.next();
	Type type;
	if(token.type == TokenType.HASH)
	{
		type.size = ParseExpression(l);
	}
	else
	{
		assert(0);
	}
	return type;
}



Place ParsePlace(Lexer l)
{
	Place p;
	p.constant = l.optional(TokenType.EXCLAMATION);
	p.type = ParseType(l);
	p.name = l.next().name;
	if(l.optional(TokenType.EQUALS))
	{
		p.default_value = ParseExpression(l);
	}
	places[p.name] = p;
	return p;
}

Expression ParseNumber(Lexer l, string name)
{
	Expression e;
	e.type = ExpressionType.Immediate;
	e.value = name;
	e.exists = true;
	return e;
}

Expression ParseName(Lexer l, string name)
{
	Expression e;
	e.type = ExpressionType.Name;
	e.value = name;
	e.exists = true;
	return e;
}

Expression[][1024] stack;
ulong scopeindex = 0;
Place[string] places;

Expression PopStack()
{
	Expression top = stack[scopeindex][$-1];
	stack[scopeindex].length--;
	return top;
}

Expression ExpressionSubtract(Expression a, Expression b)
{
	if(a.type == ExpressionType.Immediate)
	{
		if(b.type == ExpressionType.Immediate)
		{
			return Expression(exists:true,type:ExpressionType.Immediate,value:to!string(to!long(a.value) - to!long(b.value)));
		}
	}
	assert(0);
}

Expression ExecuteExpression(Expression e)
{
	switch(e.type)
	{
		case ExpressionType.Name:
			return ExecuteExpression(places[e.value].default_value);
			break;
		case ExpressionType.Sub:
			return ExpressionSubtract(ExecuteExpression(e.inner[0]),ExecuteExpression(e.inner[1]));
			break;
		default:
			writeln(e);
			return e;
			break;
	}
}

Expression ParsePrefixExpression(Lexer l)
{
	Expression e;
	Token t = l.next();
	switch(t.type)
	{
		case TokenType.EQUALS:
			ParseMacro(l);
			break;
		case TokenType.AT:
			ParsePlace(l);
			break;
		case TokenType.NUMBER:
			e = ParseNumber(l, t.name);
			break;
		case TokenType.NAME:
			e = ParseName(l, t.name);
			break;
		case TokenType.PAREN_LEFT:
			scopeindex++;
			while(!l.atEnd())
			{
				if(l.optional(TokenType.PAREN_RIGHT))
				{
					stack[scopeindex-1] ~= stack[scopeindex];
					stack[scopeindex].length = 0;
					scopeindex--;
					break;
				}
				Expression ex = ParseExpression(l);
				if(ex.exists)
				{
					stack[scopeindex] ~= ex;
				}
			}
			break;
		case TokenType.PERCENT:
			ParseExpression(l);
			writeln(stack[scopeindex]);
			break;
		case TokenType.CARET:
			e = ExecuteExpression(ParseExpression(l));
			break;
		default:
			writeln(t);
			assert(0);
			break;
	}
	
	return e;
}

Expression ParseExpression(Lexer l)
{
	Expression e = ParsePrefixExpression(l);
	ulong origi = l.i;
	Token t = l.next();
	switch(t.type)
	{
		case TokenType.PLUS:
			Expression olde = e;
			e.type = ExpressionType.Add;
			e.inner ~= olde;
			e.inner ~= ParseExpression(l);
			e.exists = true;
			break;
		case TokenType.MINUS:
			Expression olde = e;
			e.type = ExpressionType.Sub;
			e.inner ~= olde;
			e.inner ~= ParseExpression(l);
			e.exists = true;
			break;
		case TokenType.SEMICOLON:
			e.exists = false;
			break;
		default:
			l.i = origi;
			break;
	}
	return e;
}

void main(string[] args)
{
	if(args.length == 1)
	{
		return;
	}
	Lexer l = new Lexer();
	
	l.code = cast(string)read(args[1]);
	while(!l.atEnd())
	{
		Expression e = ParseExpression(l);
		if(e.exists)
		{
			stack[scopeindex] ~= e;
		}
	}
	writeln(places);
}
