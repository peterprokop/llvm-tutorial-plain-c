#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define NUM_BINOPS 4
#define ALLOC_STRUCT(name, structType) struct structType *name = \
(struct structType *) malloc(sizeof(struct structType))


#pragma mark Lexer

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
    tok_eof = -1,
    
    // commands
    tok_def = -2,
    tok_extern = -3,
    
    // primary
    tok_identifier = -4,
    tok_number = -5
};

static char IdentifierStr[64]; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

/// gettok - Return the next token from standard input.
static int gettok() {
    static int LastChar = ' ';
    
    // Skip any whitespace.
    while (isspace(LastChar)) {
        LastChar = getchar();
    }
    
    //printf("%c\n", LastChar);
    
    if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr[0] = LastChar;
        int charIndex = 1;
        while (isalnum((LastChar = getchar()))) {
            IdentifierStr[charIndex] = LastChar;
            charIndex++;
        }
        IdentifierStr[charIndex] = 0;
        
        if (!strcmp(IdentifierStr, "def")) {
            return tok_def;
        }
        if (!strcmp(IdentifierStr, "extern")) {
            return tok_extern;
        }
        
        return tok_identifier;
    }
    
    if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
        char NumStr[64];
        int charIndex = 1;
        do {
            NumStr[charIndex] = LastChar;
            LastChar = getchar();
            charIndex++;
        } while (isdigit(LastChar) || LastChar == '.');
        NumStr[charIndex] = 0;
        
        NumVal = strtod(NumStr, 0);
        return tok_number;
    }

    if (LastChar == '#') {
        // Comment until end of line.
        do
            LastChar = getchar();
        while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        
        if (LastChar != EOF)
            return gettok();
    }
    
    // Check for end of file.  Don't eat the EOF.
    if (LastChar == EOF) {
        return tok_eof;
    }
    
    // Otherwise, just return the character as its ascii value.
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

#pragma mark Abstract Syntax Tree (aka Parse Tree)

/// NumberExprAST - Expression class for numeric literals like "1.0".
struct NumberExprAST {
    double Val;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
struct VariableExprAST {
    char Name[64];
};

/// BinaryExprAST - Expression class for a binary operator.
struct BinaryExprAST {
    char Op;
    void *LHS, *RHS;
};

/// CallExprAST - Expression class for function calls.
struct CallExprAST {
    char Callee[64];
    void *Args[64];
    int NumArgs;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
struct PrototypeAST {
    char Name[64];
    char *Args[64];
    int NumArgs;
};

/// FunctionAST - This class represents a function definition itself.
struct FunctionAST {
    struct PrototypeAST *Proto;
    void *Body;
};

#pragma mark Parser

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static char BinopPrecedenceOperators[NUM_BINOPS];
static int BinopPrecedencePrecedence[NUM_BINOPS];

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
    if (!isascii(CurTok)) {
        return -1;
    }
    
    for (int i = 0; i < NUM_BINOPS; i++) {
        if (CurTok == (int) BinopPrecedenceOperators[i]) {
            // Make sure it's a declared binop.
            int TokPrec = BinopPrecedencePrecedence[i];
            if (TokPrec <= 0)
                return -1;
            return TokPrec;
        }
    }
    
    return -1;
}

/// Error* - These are little helper functions for error handling.
void* Error(const char *Str) {
    fprintf(stderr, "Error: %s\n", Str);
    return NULL;
}

struct PrototypeAST* ErrorP(const char *Str) {
    Error(Str);
    return NULL;
}

static void* ParseExpression();

/// numberexpr ::= number
static void* ParseNumberExpr() {
    ALLOC_STRUCT(Result, NumberExprAST);
    Result->Val = NumVal;
    
    getNextToken(); // consume the number
    return Result;
}

/// parenexpr ::= '(' expression ')'
static void* ParseParenExpr() {
    getNextToken(); // eat (.
    void *V = ParseExpression();
    if (!V) {
        return NULL;
    }
    
    if (CurTok != ')') {
        return Error("expected ')'");
    }
    
    getNextToken(); // eat ).
    return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static void* ParseIdentifierExpr() {
    char IdName[64];
    strcpy(IdName, IdentifierStr);
    
    getNextToken(); // eat identifier.
    
    if (CurTok != '(') { // Simple variable ref.
        ALLOC_STRUCT(Result, VariableExprAST);
        strcpy(Result->Name, IdName);
        
        return Result;
    }
    
    // Call.
    getNextToken(); // eat (

    ALLOC_STRUCT(Result, CallExprAST);
    Result->NumArgs = 0;
    strcpy(Result->Callee, IdName);
    
    if (CurTok != ')') {
        while (1) {
            void* Arg = ParseExpression();
            if (Arg != NULL) {
                Result->Args[Result->NumArgs] = Arg;
                Result->NumArgs++;
            } else {
                return NULL;
            }
            
            if (CurTok == ')') {
                break;
            }
            
            if (CurTok != ',') {
                return Error("Expected ')' or ',' in argument list");
            }
            getNextToken();
        }
    }
    
    // Eat the ')'.
    getNextToken();
    
    return Result;
    //make_unique<CallExprAST>(IdName, std::move(Args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static void* ParsePrimary() {
    switch (CurTok) {
        default:
            return Error("unknown token when expecting an expression");
        case tok_identifier:
            return ParseIdentifierExpr();
        case tok_number:
            return ParseNumberExpr();
        case '(':
            return ParseParenExpr();
    }
}

/// binoprhs
///   ::= ('+' primary)*
static void* ParseBinOpRHS(int ExprPrec, void* LHS) {
    // If this is a binop, find its precedence.
    while (1) {
        int TokPrec = GetTokPrecedence();
        
        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec)
            return LHS;
        
        // Okay, we know this is a binop.
        int BinOp = CurTok;
        getNextToken(); // eat binop
        
        // Parse the primary expression after the binary operator.
        void *RHS = ParsePrimary();
        if (!RHS) {
            return NULL;
        }
        
        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, RHS);
            if (!RHS)
                return NULL;
        }
        
        ALLOC_STRUCT(Result, BinaryExprAST);
        Result->LHS = LHS;
        Result->RHS = RHS;
        Result->Op = BinOp;
        
        // Merge LHS/RHS.
        LHS = Result;
        
        //make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

/// expression
///   ::= primary binoprhs
///
static void* ParseExpression() {
    void *LHS = ParsePrimary();
    if (!LHS) {
        return NULL;
    }
    
    return ParseBinOpRHS(0, LHS);
}

/// prototype
///   ::= id '(' id* ')'
static struct PrototypeAST* ParsePrototype() {
    if (CurTok != tok_identifier) {
        return ErrorP("Expected function name in prototype");
    }
    
    ALLOC_STRUCT(Result, PrototypeAST);
    strcpy(Result->Name, IdentifierStr);
    
    getNextToken();
    
    if (CurTok != '(') {
        return ErrorP("Expected '(' in prototype");
    }
    
    
    Result->NumArgs = 0;
    
    //std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier) {
        //ArgNames.push_back(IdentifierStr);
        Result->Args[Result->NumArgs] = (char *) malloc(64 * sizeof(char));
        strcpy(Result->Args[Result->NumArgs], IdentifierStr);
        Result->NumArgs++;
    }
    
    if (CurTok != ')') {
        return ErrorP("Expected ')' in prototype");
    }
    
    // success.
    getNextToken(); // eat ')'.
    
    return Result;
    //make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

/// definition ::= 'def' prototype expression
static struct FunctionAST* ParseDefinition() {
    getNextToken(); // eat def.
    struct PrototypeAST *Proto = ParsePrototype();
    if (!Proto) {
        return NULL;
    }
    
    void *E = ParseExpression();
    if (E != NULL) {
        ALLOC_STRUCT(Result, FunctionAST);
        Result->Proto = Proto;
        Result->Body = E;
        
        return Result;
    }
    
    return NULL;
}

/// toplevelexpr ::= expression
static struct FunctionAST* ParseTopLevelExpr() {
    void *E = ParseExpression();
    if (E != NULL) {
        // Make an anonymous proto.
        ALLOC_STRUCT(Proto, PrototypeAST);
        strcpy(Proto->Name, "__anon_expr");
        Proto->NumArgs = 0;
        
        ALLOC_STRUCT(Result, FunctionAST);
        Result->Proto = Proto;
        Result->Body = E;
        
        return Result;
        
    }
    return NULL;
}

/// external ::= 'extern' prototype
static struct PrototypeAST* ParseExtern() {
    getNextToken(); // eat extern.
    return ParsePrototype();
}

#pragma mark Top-Level parsing

static void HandleDefinition() {
    if (ParseDefinition()) {
        fprintf(stderr, "Parsed a function definition.\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern() {
    if (ParseExtern()) {
        fprintf(stderr, "Parsed an extern\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression() {
    // Evaluate a top-level expression into an anonymous function.
    if (ParseTopLevelExpr()) {
        fprintf(stderr, "Parsed a top-level expr\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
    while (1) {
        fprintf(stderr, "ready> ");
        switch (CurTok) {
            case tok_eof:
                return;
            case ';': // ignore top-level semicolons.
                getNextToken();
                break;
            case tok_def:
                HandleDefinition();
                break;
            case tok_extern:
                HandleExtern();
                break;
            default:
                HandleTopLevelExpression();
                break;
        }
    }
}

#pragma mark Main code

int main() {
    // Install standard binary operators.
    // 1 is lowest precedence.
    char binops[] = "<+-*";
    int precedence[] = {10, 20, 30, 40};
    
    for (int i = 0; i < NUM_BINOPS; i++) {
        BinopPrecedenceOperators[i] = binops[i];
        BinopPrecedencePrecedence[i] = precedence[i];    
    }
    
    
    // Prime the first token.
    fprintf(stderr, "ready> ");
    getNextToken();
    
    // Run the main "interpreter loop" now.
    MainLoop();
    
    return 0;
}