#include <cstdlib>
#include <iostream>
#include <fstream>
#include <stage0.h>
#include <ctime>
#include <iomanip>
#include <ctype.h>
#include <map>


Compiler::Compiler(char **argv) // constructor
{
	sourceFile.open(argv[1]);
	listingFile.open(argv[2]);
	objectFile.open(argv[3]);
}

Compiler::~Compiler() // destructor
{
	sourceFile.close();
	listingFile.close();
	objectFile.close();
}

void Compiler::createListingHeader()
{
	time_t now = time (NULL);
	listingFile << "STAGE0: " << "Brayden Payne Cade Campbell Brayden Sommerfield " << setw(31) << ctime(&now) << endl;
	listingFile << setw(8) << "LINE NO." << setw(30) << "SOURCE STATEMENT" << endl << endl;
	//line numbers and source statements should be aligned under the headings
}

void Compiler::parser()
{
	nextChar();
		//ch must be initialized to the first character of the source file
		if (nextToken() != "program"){
		processError("keyword \"program\" expected");
		//a call to nextToken() has two effects
		// (1) the variable, token, is assigned the value of the next token
		// (2) the next token is read from the source file in order to make
		// the assignment. The value returned by nextToken() is also
		// the next token.
		}
	prog();
	//parser implements the grammar rules, calling first rule
	listingFile << endl;
}



void Compiler::createListingTrailer()
{
	if (errorCount >= 1){
		listingFile << left << setw(28) << "COMPILATION TERMINATED "  << errorCount <<  " ERROR ENCOUNTERED" << endl;
	}else{
		listingFile << left << setw(28) << "COMPILATION TERMINATED "  << errorCount <<  " ERRORS ENCOUNTERED" << endl;
	}
}

void Compiler::processError(string err)
{
	errorCount++;
	listingFile << endl << "Error: "  << "Line " << lineNo << ": " << err << endl << endl;;
	createListingTrailer();
	exit(1);
}

void Compiler::prog() //token should be "program"
{
	
	if (token != "program"){
		processError("Keyword \"program\" Expected");
	}
	progStmt();
	if (token == "const"){
		consts();
	}	
	
	if (token == "var"){
		vars();
	}
	if (token != "begin"){
		processError("Keyword \"begin\" expected");
	}
	beginEndStmt();
	
}


void Compiler::progStmt() //token should be "program"
{
	string x;
	if (token != "program"){
		processError("keyword \"program\" expected");
	}
	x = nextToken();
	if (!isNonKeyId(x)){
		processError("Program name expected");
	}
	nextToken();
	if (token != ";"){
		processError("\";\" expected");
	}
	nextToken();
	code("program", x);
	insert(x, PROG_NAME, CONSTANT, x, NO, 0);
	
}

void Compiler::consts() //token should be "const"
{
	if (token != "vars" && token != "begin"){
		if (token != "const"){
			processError("keyword \"const\" expected");
		}
		nextToken();
		if (!isNonKeyId(token)){
			processError("non-keyword identifier must follow \"const\"");
		}
		constStmts();
	}
}


void Compiler::vars() //token should be "var"
{
	if (token != "begin"){

		if (token != "var"){
			processError("keyword \"var\" expected");
		}
		nextToken();
		
		if (!isNonKeyId(token)){
			processError("non-keyword identifier must follow \"var\"");
		}
		varStmts();
	}
}


void Compiler::beginEndStmt() //token should be "begin"
{
	
	if (token != "begin"){
		processError("keyword \"begin\" expected");
	}
	nextToken();
	if (token != "end"){
		processError("keyword \"end\" expected");
	}
	nextToken();
	if (token != "."){
		processError("period expected");
	}
	nextToken();
	if (token != "$"){
		processError("No text may follow \"end\"");
	}
	code("end", ".");
}


void Compiler::constStmts() //token should be NON_KEY_ID
{
	//
	if (token != "var" && token != "begin"){
		if (token == "const"){
			processError("non-keyword identifier, \"begin\", or \"var\" expected");
		}
		string x,y,z;
		if (!isNonKeyId(token)){
			processError("non-keyword identifier expected");
		}
		x = token;
		nextToken();
		//
		//
		if (token != "="){
			processError("\"=\" expected");
		}
		y = nextToken();
		if (y != "+" && y != "-" && y != "not" && !isNonKeyId(y) && y != "false" && y != "true" && !isInteger(y)){
			processError("token to right of \"=\" illegal");
		}
		if (y == "+" || y == "-"){
			nextToken();
			if (!isInteger(token)){
				processError("integer expected after sign");
			}
			y = y + token;
		}
		if (y == "not"){
			nextToken();
			token = whichValue(token);
			if (!isBoolean(token)){
				processError("boolean expected after \"not\"");
			}
			if (token == "true"){
				y = "false";
			}else{
				y = "true";
			}
		}
		nextToken();
		if (token != ";"){
			processError("semicolon expected");
		}
		
		if (whichType(y) != BOOLEAN && whichType(y) != INTEGER){
			processError("data type of token on the right-hand side must be INTEGER or BOOLEAN");
		}
		
		insert(x, whichType(y), CONSTANT, whichValue(y), YES, 1);
		x = nextToken();
		if (!isNonKeyId(x) && x != "begin" && x != "var"){
			processError("non-keyword identifier, \"begin\", or \"var\" expected");
		}
		if (isNonKeyId(x)){
			constStmts();
		}
	}
}

void Compiler::varStmts() //token should be NON_KEY_ID
{
	if (token != "begin"){
		if (token == "const"){
			processError("non-keyword identifier or \"begin\" expected");
		}
			string x,y;
			storeTypes dataType;
			if (!isNonKeyId(token)){
				processError("non-keyword identifier expected");
			}
			x = ids();
			if (token != ":"){
				processError("\":\" expected");
			}
			nextToken();
			if (token != "integer" && token != "boolean"){
				processError("illegal type follows \":\"");
			}
			y = token;
			nextToken();
			if (token != ";"){
				processError("semicolon expected");
			}
			if (y == "integer"){
				dataType = INTEGER;
			}else if (y == "boolean"){
				dataType = BOOLEAN;
			}
			insert(x, dataType, VARIABLE, "", YES, 1);
			nextToken();
			if (token != "begin" &&  !isNonKeyId(token)){
				processError("non-keyword identifier or \"begin\" expected");
			}
			if (isNonKeyId(token)){
				varStmts();
			}
	}
}

string Compiler::ids() //token should be NON_KEY_ID
{
	string temp, tempString;
	if (token != ":"){
		
		if (!isNonKeyId(token)){
			processError("non-keyword identifier expected");
		}

		tempString = token;
		temp = token;
		nextToken();
		if (token == ","){
			if (!isNonKeyId(nextToken())){
				processError("non-keyword identifier expected");
			}
		tempString = temp + "," + ids();
		}
		return tempString;
	}else{
		return nextToken();
	}
	
	
}

void Compiler::insert(string externalName, storeTypes inType, modes inMode, string inValue, allocation inAlloc, int inUnits)
 //create symbol table entry for each identifier in list of external names
 //Multiply inserted names are illegal
{
	
	string name;
	uint leftName = 0;

	while (leftName < externalName.length())
	{
      name = "";
      while (name == "")
      {
         while (leftName < externalName.length() && externalName[leftName] != ',')
         {
            name = name + externalName[leftName];
            leftName = leftName + 1;
         }

         leftName += 1;

      if (!name.empty())
      {
         if (name.length() > 15)
         {
            name = name.substr(0,15);
         }
            if (symbolTable.count(name) > 0)
            {
               processError("symbol " + name + " is multiply defined");
            }
            else if (isKeyword(name) && name != "true" && name != "false")
            {
               processError("illegal use of keyword");
            }
            else //create table entry
            {
               if (isupper(name[0]))
               {
                  symbolTable.insert({name,SymbolTableEntry(name,inType,inMode,inValue,inAlloc,inUnits)});
               }
               else
               {
                  //symbolTable[name]=(genInternalName(inType),inType,inMode,inValue,inAlloc,inUnits)
                  symbolTable.insert({name,SymbolTableEntry(genInternalName(inType),inType,inMode,inValue,inAlloc,inUnits)});
               }
            }
         }
      }
      if (symbolTable.size() > 256)
      {
         processError("symbol table cannot exceed 256");
      }
	}
}

storeTypes Compiler::whichType(string name) //tells which data type a name has
{
	
	storeTypes dataType;
	if (isLiteral(name)){
		if (name == "true" || name == "false"){
			dataType = BOOLEAN;
		}else{
			dataType = INTEGER;
		}
	}else{	//name is an identifier and hopefully a constant
		if (symbolTable.count(name) == 1){
			dataType = symbolTable.find(name)->second.getDataType();
		}else{
			processError("reference to undefined constant");
		}
	}
 return dataType;
}


string Compiler::whichValue(string name) //tells which value a name has
{
	
	string value;
	if (isLiteral(name)){
		value = name;
	}else{ //name is an identifier and hopefully a constant
		if (symbolTable.count(name) == 1 && symbolTable.find(name)->second.getValue() != ""){
			value = symbolTable.find(name)->second.getValue();
		}else{
			processError("reference to undefined constant");
		}
	}
	return value;
}

void Compiler::code(string op, string operand1, string operand2)
{
	
	if (op == "program"){
		emitPrologue(operand1);
	}else if(op == "end"){
		emitEpilogue();
	}else{
	processError("compiler error since function code should not be called with illegal arguments");
	}
}

void Compiler::emit(string label, string instruction, string operands, string comment)
{
	
	objectFile << left;
	objectFile << setw(8) << label;
	objectFile << setw(8) << instruction;
	objectFile << setw(24) << operands;
	objectFile << comment << endl;
}

void Compiler::emitPrologue(string progName, string operand2)
{
	time_t now = time (NULL);
	objectFile << "; Brayden Payne Cade Campbell Brayden Sommerfield" << setw(31) << ctime(&now);
	objectFile << "%INCLUDE \"Along32.inc\"" << endl << "%INCLUDE \"Macros_Along.inc\"" << endl << endl;
	emit("SECTION", ".text");
	emit("global", "_start", "", "; program " + progName.substr(0, 15));
	objectFile << endl;
	emit("_start:");
}

void Compiler::emitEpilogue(string operand1, string operand2)
{
	
	emit("","Exit", "{0}");
	objectFile << endl;
	emitStorage();
}

void Compiler::emitStorage()
{
	
	string name;
	emit("SECTION", ".data");
	for (auto it = symbolTable.begin(); it != symbolTable.end(); ++it){
		if(it->second.getAlloc() == YES && it->second.getMode() == CONSTANT){
			string newValue;
			if(it->second.getValue() == "true"){
				newValue = "-1";
			}else if (it->second.getValue() == "false"){
				newValue = "0";
			}else{
				newValue = it->second.getValue();
			}
				
			emit(it->second.getInternalName(), "dd", newValue , "; " + it->first);
		}
	}
	objectFile << endl;
	emit("SECTION", ".bss");
	for (auto it = symbolTable.begin(); it != symbolTable.end(); ++it){
		if(it->second.getAlloc() == YES && it->second.getMode() == VARIABLE){
			emit(it->second.getInternalName(), "resd", "1", "; " + it->first);
		}
	}
	//for those entries in the symbolTable that have
	//an allocation of YES and a storage mode of VARIABLE
	//{ call emit to output a line to objectFile }
}

string Compiler::nextToken(){//returns the next token or end of file marker
	
	token = "";
	while (token == ""){
			if (ch == '{'){	
				while (nextChar() != '}' && ch != END_OF_FILE)
				{ 
					
					
				}
				if (ch == END_OF_FILE){
					processError("unexpected end of file");
				}else{
					nextChar();
				}
			}

			else if (ch == '}'){	
				processError("\"}\"cannot begin token");
			}
			
			
			
			else if (isspace(ch)){
				nextChar();
			}
			
			else if (isSpecialSymbol(ch)){
				token = ch;
				nextChar();
				
			}
			
			else if(islower(ch)){		
				 token = ch;
				while ((islower(nextChar()) || isdigit(ch) || ch == '_') && ch != END_OF_FILE){
					if (token.back() == '_' && ch == '_'){
						processError("\"_\" must be followed by a letter or number");
					}
						token+=ch;
				}
				if (ch == END_OF_FILE){
					processError("unexpected end of file");
				}
			}

			else if (isdigit(ch)){
				token = ch;
				while (isdigit(nextChar()) && ch != END_OF_FILE){
					token += ch;
				}
				if (ch == END_OF_FILE){
					processError("unexpected end of file");
				}
			}
			
			else if (ch == END_OF_FILE) {
				token = ch;
			}

			else {
				string errSym = string(1, ch);
				string quote = "\"";
				//string quote = string(1, quote);
				processError("illegal symbol " + quote + errSym + quote);
			}
	}
return token;
}


char Compiler::nextChar(){//returns the next character or end of file marker
	
	sourceFile.get(ch);
	static char prevChar = '\n';

	if (sourceFile.eof()){
		ch = END_OF_FILE;
		//return ch;//use a special character to designate end of file
	}else{
        if(prevChar == '\n'){
            lineNo++;
            listingFile << setw(5) << lineNo << '|';
        }
        listingFile << ch;
    }
    prevChar = ch;
    return ch;
}

bool Compiler::isKeyword(string s) const{// determines if s is a keyword
	if(s == "program" || s == "begin" || s == "end" || s == "var" || s == "const" || s == "integer" || s == "boolean" || s == "true" || s == "false" || s == "not"){
		return true;
	}else{
		return false;
	}
}	

bool Compiler::isSpecialSymbol(char c) const{// determines if c is a special symbol
	if (c == '=' || c == ':' || c == ',' || c == ';' || c == '.' || c == '+' || c == '-'){
		return true;
	}else{
		return false;
	}
}

bool Compiler::isBoolean(string s) const{  // determines if s is a boolean
	if (s == "true" || s == "false"){
		return true;
	}else{
		return false;
	}
}

bool Compiler::isLiteral(string s) const{  // determines if s is an integer
	if (isInteger(s) || s == "false" || s == "true" || s == "not" || s == "+" || s == "-"){
		return true;
	}else{
		return false;
	}
}


bool Compiler::isNonKeyId(string s) const{ // determines if s is a non_key_id
	
	string subs = s.substr(0, s.length());
	if (isalpha(s[0])){
		for (unsigned int i = 0; i < subs.length(); i++){
			if (isalnum(subs[i])){
				return true;
			}
		}
	}

	return false;
}

bool Compiler::isInteger(string s) const{ // determines if s is a literal
	
	for (unsigned int i = 0; i < s.length()-1; i++){
		if (s[0] == '+' || s[0] == '-'){
			i++;
		}
		if (!isdigit(s[i]))
		{
				return false;
		}
	}
	return true;
}

string Compiler::genInternalName(storeTypes stype) const{
	
	string newName;
	static int numBool /*= 0*/, numInt/* = 0*/, numProg /*= 0*/;
	if (stype == BOOLEAN){
		newName = "B";
		newName += to_string(numBool);
		numBool++;
	}
	
	if (stype == INTEGER){
		newName = "I";
		newName += to_string(numInt);
		numInt++;
	}
	
	if (stype == PROG_NAME){
		newName = "P";
		newName += to_string(numProg);
		numProg++;
	}
	
	return newName;
}