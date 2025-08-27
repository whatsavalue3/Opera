import std.stdio;
import std.file;
import std.conv;
import std.ascii;
import std.typecons;
import std.algorithm;

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
	
	DOUBLE_EQUALS,
}

struct Token
{
	TokenType type;
	ulong pos;
	string name;
}

TokenType[string] stringToToken = [
	"==": TokenType.DOUBLE_EQUALS,
];

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

bool isAlphaUnder(char c)
{
	return c == '_' || isAlpha(c);
}

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
		foreach(tok, type; stringToToken)
		{
			if(code[i..$].startsWith(tok))
			{
				i+= tok.length;
				return Token(type:type,pos:i,name:code[i-tok.length..i]);
			}
		}
		if(code[i] in charToToken)
		{
			return Token(type:charToToken[code[i]],pos:i,name:code[i..++i]);
		}
		if(isAlphaUnder(code[i]))
		{
			ulong first = i;
			while(isAlphaUnder(code[i]))
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
	Sub,
	Call,
	Print,
	Discard,
	Group,
	Ternary,
	Equality,
	Function,
}

struct Expression
{
	bool exists = false;
	ExpressionType type;
	string value;
	Expression[] inner;
	Expression[] call;
	Function[] func;
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
	bool isref;
	string reference;
}


struct Place
{
	string name;
	Type type;
	bool constant = false;
	Expression default_value;
}

struct Function
{
	string name;
	Type type;
	Place[] args;
	Expression inner;
}



Type ParseType(Lexer l)
{
	Token token = l.next();
	Type type;
	if(token.type == TokenType.HASH)
	{
		type.size = ParseExpression(l);
		return type;
	}
	else if(token.type == TokenType.EXCLAMATION)
	{
		return type;
	}
	type.isref = true;
	type.reference = token.name;
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
	return p;
}

Type ParseGroup(Lexer l)
{
	Type t;
	string name = l.next().name;
	t = ParseType(l);
	types[name] = t;
	return t;
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



Expression ParseFunction(Lexer l)
{
	Function f;
	f.type = ParseType(l);
	f.name = l.next().name;
	assert(l.next().type == TokenType.PAREN_LEFT);
	while(!l.optional(TokenType.PAREN_RIGHT))
	{
		Place arg = ParsePlace(l);
		f.args ~= arg;
		l.optional(TokenType.COMMA);
	}
	f.inner = ParseExpression(l);
	Expression e;
	e.exists = true;
	e.type = ExpressionType.Function;
	e.func ~= f;
	//functions[f.name] = f;
	return e;
}

Expression ParseCall(Lexer l, Expression tocall)
{
	Expression e;
	e.type = ExpressionType.Call;
	e.exists = true;
	Token t;
	while(t.type != TokenType.PAREN_RIGHT)
	{
		e.inner ~= ParseExpression(l);
		t = l.next();
		if(t.type != TokenType.COMMA)
		{
			assert(t.type == TokenType.PAREN_RIGHT);
		}
	}
	e.call ~= tocall;
	return e;
}


Expression[][1024] stack;
ulong scopeindex = 0;
Place[string] places;
Function[string] functions;
Type[string] types;

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

Expression ExpressionAdd(Expression a, Expression b)
{
	if(a.type == ExpressionType.Immediate)
	{
		if(b.type == ExpressionType.Immediate)
		{
			return Expression(exists:true,type:ExpressionType.Immediate,value:to!string(to!long(a.value) + to!long(b.value)));
		}
	}
	assert(0);
}

Expression ExpressionEquality(Expression a, Expression b)
{
	if(a.type == ExpressionType.Immediate)
	{
		if(b.type == ExpressionType.Immediate)
		{
			return Expression(exists:true,type:ExpressionType.Immediate,value:to!string(a.value == b.value ? 1 : 0));
		}
	}
	assert(0);
}

bool IsTruthy(Expression e)
{
	return e.type == ExpressionType.Immediate && e.value != "0";
}

Expression ExecuteExpression(Expression e, Place[string] scop)
{
	switch(e.type)
	{
		case ExpressionType.Name:
			return ExecuteExpression(scop[e.value].default_value, scop);
			break;
		case ExpressionType.Print:
			Expression r = ExecuteExpression(e.inner[0], scop);
			writeln(r);
			return r;
			break;
		case ExpressionType.Sub:
			return ExpressionSubtract(ExecuteExpression(e.inner[0], scop),ExecuteExpression(e.inner[1], scop));
			break;
		case ExpressionType.Add:
			return ExpressionAdd(ExecuteExpression(e.inner[0], scop),ExecuteExpression(e.inner[1], scop));
			break;
		case ExpressionType.Equality:
			return ExpressionEquality(ExecuteExpression(e.inner[0], scop),ExecuteExpression(e.inner[1], scop));
			break;
		case ExpressionType.Call:
			Expression f = e.call[0];
			
			Place[string] newscop = scop;
			foreach(i, value; functions[f.value].args)
			{
				newscop[value.name] = value;
				newscop[value.name].default_value = e.inner[i];
			
			}
			return ExecuteExpression(functions[f.value].inner, newscop);
			break;
		case ExpressionType.Discard:
			Expression r = ExecuteExpression(e.inner[0], scop);
			r.exists = false;
			return r;
			break;
		case ExpressionType.Group:
			Expression toreturn = Expression(exists:false);
			scopeindex++;
			foreach(ei; e.inner)
			{
				Expression r = ExecuteExpression(ei, scop);
				if(r.exists)
				{
					stack[scopeindex] ~= r;
					toreturn = r;
				}
			}
			scopeindex--;
			return toreturn;
			break;
		case ExpressionType.Ternary:
			Expression r = ExecuteExpression(e.inner[0], scop);
			if(IsTruthy(r))
			{
				return ExecuteExpression(e.inner[1], scop);
			}
			else if(e.inner.length == 3)
			{
				return ExecuteExpression(e.inner[2], scop);
			}
			r.exists = false;
			r.type = ExpressionType.Discard;
			return r;
			break;
		case ExpressionType.Function:
			functions[e.func[0].name] = e.func[0];
			Expression r;
			return r;
			break;
		default:
			//writeln(e);
			return e;
			break;
	}
}

Expression ParsePrefixExpression(Lexer l)
{
	ulong origi = l.i;
	Expression e;
	Token t = l.next();
	switch(t.type)
	{
		case TokenType.EQUALS:
			ParseMacro(l);
			break;
		case TokenType.AT:
			Place p = ParsePlace(l);
			places[p.name] = p;
			break;
		case TokenType.AMPERSAND:
			e = ParseFunction(l);
			break;
		case TokenType.DOLLAR:
			ParseGroup(l);
			break;
		case TokenType.NUMBER:
			e = ParseNumber(l, t.name);
			break;
		case TokenType.NAME:
			e = ParseName(l, t.name);
			break;
		case TokenType.PAREN_LEFT:
			while(!l.atEnd())
			{
				if(l.optional(TokenType.PAREN_RIGHT))
				{
					break;
				}
				Expression ex = ParseExpression(l);
				if(ex.exists)
				{
					e.inner ~= ex;
				}
			}
			e.type = ExpressionType.Group;
			break;
		case TokenType.PERCENT:
			e = Expression(exists:true, type:ExpressionType.Print, inner:[ParseExpression(l)]);
			//writeln(stack[scopeindex]);
			break;
		//case TokenType.CARET:
		//	e = ExecuteExpression(ParseExpression(l),places);
		//	break;
		default:
			l.i = origi;
			break;
	}
	
	return e;
}

Expression ParseInfixExpression(Lexer l)
{
	Expression e = ParsePrefixExpression(l);
	ulong origi = l.i;
	Token t = l.next();
	switch(t.type)
	{
		case TokenType.PLUS:
			Expression olde = e;
			e.type = ExpressionType.Add;
			e.inner.length = 0;
			e.inner ~= olde;
			e.inner ~= ParseExpression(l);
			e.exists = true;
			break;
		case TokenType.MINUS:
			Expression olde = e;
			e.type = ExpressionType.Sub;
			e.inner.length = 0;
			e.inner ~= olde;
			e.inner ~= ParseExpression(l);
			e.exists = true;
			break;
		case TokenType.QUESTION:
			Expression olde = e;
			e.type = ExpressionType.Ternary;
			e.inner.length = 0;
			e.inner ~= olde;
			e.inner ~= ParseExpression(l);
			if(l.optional(TokenType.COLON))
			{
				e.inner ~= ParseExpression(l);
			}
			e.exists = true;
			break;
		case TokenType.DOUBLE_EQUALS:
			Expression olde = e;
			e.type = ExpressionType.Equality;
			e.inner.length = 0;
			e.inner ~= olde;
			e.inner ~= ParseExpression(l);
			e.exists = true;
			break;
		case TokenType.PAREN_LEFT:
			e = ParseCall(l, e);
			break;
		default:
			l.i = origi;
			break;
	}
	return e;
}

Expression ParseExpression(Lexer l)
{
	Expression e = ParseInfixExpression(l);
	ulong origi = l.i;
	Token t = l.next();
	switch(t.type)
	{
		case TokenType.SEMICOLON:
			Expression discard;
			discard.type = ExpressionType.Discard;
			discard.inner ~= e;
			discard.exists = true;
			return discard;
			break;
		default:
			l.i = origi;
			break;
	}
	return e;
}
/*
void WalkExpression(Expression e, void action(Expression))
{
	action(e);
	foreach(inner; e.inner)
	{
		WalkExpression(inner, action);
	}
}

void DerefTypes(Expression e)
{
	
}
*/
void DerefAllTypes()
{
	foreach(ref place; places)
	{
		if(place.type.isref)
		{
			place.type = types[place.type.reference];
		}
	}
	foreach(ref func; functions)
	{
		if(func.type.isref)
		{
			func.type = types[func.type.reference];
		}
	}
}

Expression[] program;

void FunctionPass()
{
	foreach(e; program)
	{
		ExecuteExpression(e, places);
	}
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
		program ~= ParseExpression(l);
	}
	FunctionPass();
	DerefAllTypes();
	ExecuteExpression(functions["main"].inner,places);
	//writeln(places);
	//writeln(functions);
}
