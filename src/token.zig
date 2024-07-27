const TokenType = []u8; //string type

const Token = struct {
    token_type: TokenType,
    literal: []u8,
};

const ILLEGAL: TokenType = "ILLEGAL";
const EOF: TokenType = "EOF";
const IDENT: TokenType = "IDENT";

const INT: TokenType = "INT";
const ASSIGN: TokenType = "=";
const PLUS: TokenType = "+";
const COMMA: TokenType = ",";
const SEMICOLON: TokenType = ";";

const LPAREN: TokenType = "(";
const RPAREN: TokenType = ")";
const LBRACE: TokenType = "{";
const RBRACE: TokenType = "}";

const FUNCTION: TokenType = "FUNCTION";
const LET: TokenType = "LET";
