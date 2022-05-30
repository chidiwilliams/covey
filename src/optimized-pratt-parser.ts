import {
  BinaryExpr,
  ConditionalExpr,
  Expr,
  LiteralExpr,
  Parser,
  Precedence,
  TokenType,
  UnaryExpr,
  VariableExpr,
} from './parser';

export type PrefixParseFn = (parser: OptimizedPrattParser) => Expr;

export type InfixParseFn = (parser: OptimizedPrattParser, left: Expr) => Expr;

export interface ParseRule {
  prefix?: PrefixParseFn;
  infix?: InfixParseFn;
  precedence: Precedence;
}

export class OptimizedPrattParser extends Parser {
  override parse(): Expr {
    return this.parsePrecedence(Precedence.TERNARY);
  }

  public parsePrecedence(precedence: Precedence): Expr {
    const nextToken = this.advance();
    const prefixRule = this.getRule(nextToken.tokenType).prefix;
    if (!prefixRule) {
      throw new Error('Expect expression.');
    }

    let expression = prefixRule(this);

    while (this.getRule(this.peek().tokenType).precedence >= precedence) {
      const nextToken = this.advance();
      const infixRule = this.getRule(nextToken.tokenType).infix!;
      expression = infixRule(this, expression);
    }

    return expression;
  }

  public getRule(tokenType: TokenType): ParseRule {
    return parseRules[tokenType]!;
  }
}

var parseRules: ParseRule[] = [
  { infix: binary, precedence: Precedence.TERM }, // PLUS
  { prefix: unary, infix: binary, precedence: Precedence.TERM }, // MINUS
  { prefix: unary, precedence: Precedence.NONE }, // BANG
  { prefix: number, precedence: Precedence.NONE }, // NUMBER
  { precedence: Precedence.NONE }, // EOF
  { infix: binary, precedence: Precedence.FACTOR }, // STAR
  { infix: binary, precedence: Precedence.FACTOR }, // SLASH
  { infix: ternary, precedence: Precedence.TERNARY }, // QUESTION_MARK
  { precedence: Precedence.NONE }, // COLON
  { prefix: variable, precedence: Precedence.NONE }, // IDENTIFIER
];

function number(parser: OptimizedPrattParser) {
  return new LiteralExpr(parser.previous().literal!);
}

function variable(parser: OptimizedPrattParser) {
  return new VariableExpr(parser.previous());
}

function unary(parser: OptimizedPrattParser) {
  const operator = parser.previous();
  const operand = parser.parsePrecedence(Precedence.UNARY);
  return new UnaryExpr(operator, operand);
}

function binary(parser: OptimizedPrattParser, left: Expr) {
  const operator = parser.previous();
  const rule = parser.getRule(operator.tokenType);
  const right = parser.parsePrecedence(rule.precedence + 1);
  return new BinaryExpr(left, operator, right);
}

function ternary(parser: OptimizedPrattParser, left: Expr) {
  const thenBranch = parser.parsePrecedence(Precedence.TERNARY);
  parser.consume(TokenType.COLON, 'Expect colon after ternary condition.');
  const elseBranch = parser.parsePrecedence(Precedence.TERNARY);
  return new ConditionalExpr(left, thenBranch, elseBranch);
}
