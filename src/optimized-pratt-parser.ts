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

export type OptimizedPrefixParseFn = (p: OptimizedPrattParser) => Expr;

export type OptimizedInfixParseFn = (p: OptimizedPrattParser, left: Expr) => Expr;

export interface OptimizedParseRule {
  prefix?: OptimizedPrefixParseFn;
  infix?: OptimizedInfixParseFn;
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

  public getRule(tokenType: TokenType): OptimizedParseRule {
    return parseRules[tokenType]!;
  }
}

var parseRules: OptimizedParseRule[] = [
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

function number(p: OptimizedPrattParser) {
  return new LiteralExpr(p.previous().literal!);
}

function variable(p: OptimizedPrattParser) {
  return new VariableExpr(p.previous());
}

function unary(p: OptimizedPrattParser) {
  const operator = p.previous();
  const operand = p.parsePrecedence(Precedence.UNARY);
  return new UnaryExpr(operator, operand);
}

function binary(p: OptimizedPrattParser, left: Expr) {
  const operator = p.previous();
  const rule = p.getRule(operator.tokenType);
  const right = p.parsePrecedence(rule.precedence + 1);
  return new BinaryExpr(left, operator, right);
}

function ternary(p: OptimizedPrattParser, left: Expr) {
  const thenBranch = p.parsePrecedence(Precedence.TERNARY);
  p.consume(TokenType.COLON, 'Expect colon after ternary condition.');
  const elseBranch = p.parsePrecedence(Precedence.TERNARY);
  return new ConditionalExpr(left, thenBranch, elseBranch);
}
