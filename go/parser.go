package covey

import (
	"fmt"
	"strconv"
	"strings"
)

type TokenType int

const (
	TokenTypePlus TokenType = iota
	TokenTypeMinus
	TokenTypeBang
	TokenTypeNumber
	TokenTypeEof
	TokenTypeStar
	TokenTypeSlash
	TokenTypeQuestionMark
	TokenTypeColon
	TokenTypeIdentifier
)

type Token struct {
	tokenType TokenType
	Lexeme    string
	literal   interface{}
}

type Expr interface {
}

type LiteralExpr struct {
	Value interface{}
}

type VariableExpr struct {
	Name Token
}

type UnaryExpr struct {
	Operator Token
	Operand  Expr
}

type BinaryExpr struct {
	Left     Expr
	Operator Token
	Right    Expr
}

type ConditionalExpr struct {
	Condition  Expr
	ThenBranch Expr
	ElseBranch Expr
}

var stringToTokenType = map[string]TokenType{
	"*": TokenTypeStar,
	"+": TokenTypePlus,
	"/": TokenTypeSlash,
	"-": TokenTypeMinus,
	"?": TokenTypeQuestionMark,
	":": TokenTypeColon,
}

func scanner(input string) []Token {
	tokens := make([]Token, 0)

	splitInput := strings.Split(input, " ")
	for _, inputItem := range splitInput {
		var token Token

		num, err := strconv.Atoi(inputItem)
		if err == nil {
			token = Token{TokenTypeNumber, inputItem, num}
		} else {
			tokenType, ok := stringToTokenType[(inputItem)]
			if ok {
				token = Token{tokenType, inputItem, nil}
			} else {
				token = Token{TokenTypeIdentifier, inputItem, nil}
			}
		}

		tokens = append(tokens, token)
	}

	tokens = append(tokens, Token{TokenTypeEof, "", nil})
	return tokens
}

type ParseError struct {
	Token   Token
	Message string
}

func (p ParseError) Error() string {
	return fmt.Sprintf("Error at token: %s: %s", p.Token.Lexeme, p.Message)
}

type RecursiveDescentParser struct {
	current int
	tokens  []Token
}

func NewRecursiveDescentParser(tokens []Token) *RecursiveDescentParser {
	return &RecursiveDescentParser{tokens: tokens}
}

func (p *RecursiveDescentParser) peek() Token {
	return p.tokens[p.current]
}

func (p *RecursiveDescentParser) previous() Token {
	return p.tokens[p.current-1]
}

func (p *RecursiveDescentParser) advance() Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *RecursiveDescentParser) isAtEnd() bool {
	return p.peek().tokenType == TokenTypeEof
}

func (p *RecursiveDescentParser) consume(tokenType TokenType, message string) Token {
	if p.check(tokenType) {
		return p.advance()
	}
	panic(ParseError{p.peek(), message})
}

func (p *RecursiveDescentParser) match(tokenTypes ...TokenType) bool {
	for _, tokenType := range tokenTypes {
		if p.check(tokenType) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *RecursiveDescentParser) check(tokenType TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().tokenType == tokenType
}

func (p *RecursiveDescentParser) Parse() Expr {
	return p.ternary()
}

func (p *RecursiveDescentParser) ternary() Expr {
	expr := p.term()

	if p.match(TokenTypeQuestionMark) {
		thenBranch := p.ternary()
		p.consume(TokenTypeColon, "Expect colon after ternary condition")
		elseBranch := p.ternary()
		return ConditionalExpr{expr, thenBranch, elseBranch}
	}

	return expr
}

func (p *RecursiveDescentParser) term() Expr {
	expr := p.factor()

	for p.match(TokenTypeMinus, TokenTypePlus) {
		operator := p.previous()
		right := p.factor()
		expr = BinaryExpr{expr, operator, right}
	}
	return expr
}

func (p *RecursiveDescentParser) factor() Expr {
	expr := p.unary()

	for p.match(TokenTypeSlash, TokenTypeStar) {
		operator := p.previous()
		right := p.unary()
		expr = BinaryExpr{expr, operator, right}
	}
	return expr
}

func (p *RecursiveDescentParser) unary() Expr {
	if p.match(TokenTypeBang, TokenTypeMinus) {
		operator := p.previous()
		expr := p.unary()
		return UnaryExpr{operator, expr}
	}
	return p.primary()
}

func (p *RecursiveDescentParser) primary() Expr {
	switch {
	case p.match(TokenTypeNumber):
		return LiteralExpr{p.previous().literal}
	case p.match(TokenTypeIdentifier):
		return VariableExpr{p.previous()}
	default:
		panic(ParseError{p.peek(), "Expect expression"})
	}
}

type PrefixParseFn func() Expr

type InfixParseFn func(left Expr) Expr

type ParseRule struct {
	prefix     PrefixParseFn
	infix      InfixParseFn
	precedence Precedence
}

type PrattParser struct {
	tokens     []Token
	current    int
	parseRules [TokenTypeIdentifier + 1]ParseRule
}

func NewPrattParser(tokens []Token) *PrattParser {
	p := &PrattParser{
		tokens: tokens,
	}

	p.parseRules = [TokenTypeIdentifier + 1]ParseRule{
		{nil, p.binary, PrecedenceTerm},
		{p.unary, p.binary, PrecedenceTerm},
		{p.unary, nil, PrecedenceNone},
		{p.number, nil, PrecedenceNone},
		{nil, nil, PrecedenceNone},
		{nil, p.binary, PrecedenceFactor},
		{nil, p.binary, PrecedenceFactor},
		{nil, p.ternary, PrecedenceTernary},
		{nil, nil, PrecedenceNone},
		{p.variable, nil, PrecedenceNone},
	}

	return p
}

func (p *PrattParser) peek() Token {
	return p.tokens[p.current]
}

func (p *PrattParser) previous() Token {
	return p.tokens[p.current-1]
}

func (p *PrattParser) advance() Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *PrattParser) isAtEnd() bool {
	return p.peek().tokenType == TokenTypeEof
}

func (p *PrattParser) consume(tokenType TokenType, message string) Token {
	if p.check(tokenType) {
		return p.advance()
	}
	panic(ParseError{p.peek(), message})
}

func (p *PrattParser) match(tokenTypes ...TokenType) bool {
	for _, tokenType := range tokenTypes {
		if p.check(tokenType) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *PrattParser) check(tokenType TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().tokenType == tokenType
}

type Precedence int

const (
	PrecedenceNone Precedence = iota
	PrecedenceTernary
	PrecedenceTerm
	PrecedenceFactor
	PrecedenceUnary
)

func (p *PrattParser) Parse() Expr {
	return p.parsePrecedence(PrecedenceTernary)
}

func (p *PrattParser) parsePrecedence(precedence Precedence) Expr {
	nextToken := p.advance()
	prefixRule := p.getRule(nextToken.tokenType).prefix
	if prefixRule == nil {
		panic(ParseError{p.previous(), "Expect expression."})
	}

	expr := prefixRule()

	for p.getRule(p.peek().tokenType).precedence >= precedence {
		nextToken := p.advance()
		infixRule := p.getRule(nextToken.tokenType).infix
		expr = infixRule(expr)
	}

	return expr
}

func (p *PrattParser) getRule(tokenType TokenType) ParseRule {
	return p.parseRules[tokenType]
}

func (p *PrattParser) number() Expr {
	return LiteralExpr{p.previous().literal}
}

func (p *PrattParser) binary(left Expr) Expr {
	operator := p.previous()
	rule := p.getRule(operator.tokenType)
	right := p.parsePrecedence(rule.precedence + 1)
	return BinaryExpr{left, operator, right}
}

func (p *PrattParser) unary() Expr {
	operator := p.previous()
	operand := p.parsePrecedence(PrecedenceUnary)
	return UnaryExpr{operator, operand}
}

func (p *PrattParser) variable() Expr {
	return VariableExpr{p.previous()}
}

func (p *PrattParser) ternary(left Expr) Expr {
	thenBranch := p.parsePrecedence(PrecedenceTernary)
	p.consume(TokenTypeColon, "Expect colon after ternary condition")
	elseBranch := p.parsePrecedence(PrecedenceTernary)
	return ConditionalExpr{left, thenBranch, elseBranch}
}
