class Token {
  constructor(public tokenType: TokenType, public lexeme: string | null, public literal: number | null) {}
}

enum TokenType {
  PLUS = 'PLUS',
  MINUS = 'MINUS',
  BANG = 'BANG',
  NUMBER = 'NUMBER',
  EOF = 'EOF',
  STAR = 'STAR',
  SLASH = 'SLASH',
  QUESTION_MARK = 'QUESTION_MARK',
  COLON = 'COLON',
  IDENTIFIER = 'IDENTIFIER',
}

interface Expr {}

class VariableExpr implements Expr {
  constructor(public name: string) {}
}

class UnaryExpr implements Expr {
  constructor(public operator: Token, public expression: Expr) {}
}

class BinaryExpr implements Expr {
  constructor(public left: Expr, public operator: Token, public right: Expr) {}
}

class ConditionalExpr implements Expr {
  constructor(public condition: Expr, public thenBranch: Expr, public elseBranch: Expr) {}
}

class LiteralExpr {
  constructor(public value: number) {}
}

class ParseError extends Error {
  constructor(token: Token, message: string) {
    super(`Error at token: ${token.lexeme}: ${message}`);
  }
}

class Parser {
  private current = 0;

  constructor(private tokens: Token[]) {}

  protected parse(): Expr {
    throw new Error('not implemented');
  }

  protected previous() {
    return this.tokens[this.current - 1]!;
  }

  protected advance() {
    if (!this.isAtEnd()) {
      this.current++;
    }
    return this.previous();
  }

  protected isAtEnd() {
    return this.peek().tokenType === TokenType.EOF;
  }

  protected peek(): Token {
    return this.tokens[this.current]!;
  }

  protected consume(tokenType: TokenType, message: string): Token {
    if (this.check(tokenType)) {
      return this.advance();
    }
    throw new ParseError(this.peek(), message);
  }

  protected check(tokenType: TokenType) {
    if (this.isAtEnd()) {
      return false;
    }

    return this.peek().tokenType === tokenType;
  }

  protected match(...tokenTypes: TokenType[]): boolean {
    for (const tokenType of tokenTypes) {
      if (this.check(tokenType)) {
        this.advance();
        return true;
      }
    }

    return false;
  }
}

class RecursiveDescentParser extends Parser {
  constructor(tokens: Token[]) {
    super(tokens);
  }

  override parse(): Expr {
    return this.ternary();
  }

  private ternary(): Expr {
    let expression = this.term();

    if (this.match(TokenType.QUESTION_MARK)) {
      const thenBranch = this.ternary();
      this.consume(TokenType.COLON, 'Expect colon after ternary condition.');
      const elseBranch = this.ternary();
      return new ConditionalExpr(expression, thenBranch, elseBranch);
    }

    return expression;
  }

  private term(): Expr {
    let expression = this.factor();

    while (this.match(TokenType.MINUS, TokenType.PLUS)) {
      const operator = this.previous();
      const right = this.factor();
      expression = new BinaryExpr(expression, operator, right);
    }

    return expression;
  }

  private factor(): Expr {
    let expression = this.unary();

    while (this.match(TokenType.SLASH, TokenType.STAR)) {
      const operator = this.previous();
      const right = this.unary();
      expression = new BinaryExpr(expression, operator, right);
    }

    return expression;
  }

  private unary(): Expr {
    if (this.match(TokenType.BANG, TokenType.MINUS)) {
      const operator = this.previous();
      const expression = this.unary();
      return new UnaryExpr(operator, expression);
    }
    return this.primary();
  }

  private primary(): Expr {
    switch (true) {
      case this.match(TokenType.NUMBER):
        return new LiteralExpr(this.previous().literal!);
      case this.match(TokenType.IDENTIFIER):
        return new VariableExpr(this.previous().lexeme!);
      default:
        throw new ParseError(this.peek(), 'Expect expression.');
    }
  }
}

enum Precedence {
  NONE,
  TERNARY,
  TERM,
  UNARY,
  FACTOR,
}

type PrefixParseFn = () => Expr;

type InfixParseFn = (left: Expr) => Expr;

interface ParseRule {
  prefix?: PrefixParseFn;
  infix?: InfixParseFn;
  precedence: Precedence;
}

class PrattParser extends Parser {
  private number: PrefixParseFn = () => {
    return new LiteralExpr(this.previous().literal!);
  };

  private unary: PrefixParseFn = () => {
    const operator = this.previous();
    const operand = this.parsePrecedence(Precedence.UNARY);
    return new UnaryExpr(operator, operand);
  };

  private binary: InfixParseFn = (left: Expr) => {
    const operator = this.previous();
    const rule = this.getRule(operator.tokenType);
    const right = this.parsePrecedence(rule.precedence + 1);
    return new BinaryExpr(left, operator, right);
  };

  private ternary: InfixParseFn = (left: Expr) => {
    const thenBranch = this.parsePrecedence(Precedence.TERNARY + 1);
    this.consume(TokenType.COLON, 'Expect colon after ternary condition.');
    const elseBranch = this.parsePrecedence(Precedence.TERNARY + 1);
    return new ConditionalExpr(left, thenBranch, elseBranch);
  };

  private variable: PrefixParseFn = () => {
    return new VariableExpr(this.previous().lexeme!);
  };

  private parseRules: Record<TokenType, ParseRule> = {
    NUMBER: { prefix: this.number, precedence: Precedence.NONE },
    MINUS: { prefix: this.unary, infix: this.binary, precedence: Precedence.TERM },
    PLUS: { infix: this.binary, precedence: Precedence.TERM },
    BANG: { prefix: this.unary, precedence: Precedence.NONE },
    EOF: { precedence: Precedence.NONE },
    STAR: { infix: this.binary, precedence: Precedence.FACTOR },
    SLASH: { infix: this.binary, precedence: Precedence.FACTOR },
    QUESTION_MARK: { infix: this.ternary, precedence: Precedence.TERNARY },
    COLON: { precedence: Precedence.NONE },
    IDENTIFIER: { prefix: this.variable, precedence: Precedence.NONE },
  };

  constructor(tokens: Token[]) {
    super(tokens);
  }

  override parse(): Expr {
    return this.parsePrecedence(Precedence.TERNARY);
  }

  private parsePrecedence(precedence: Precedence): Expr {
    let nextToken = this.advance();

    const prefixRule = this.getRule(nextToken.tokenType).prefix;
    if (!prefixRule) {
      throw new Error('Expect expression.');
    }

    let expression = prefixRule();

    while (precedence <= this.getRule(this.peek().tokenType).precedence) {
      const nextToken = this.advance();
      const infixRule = this.getRule(nextToken.tokenType).infix!;
      expression = infixRule(expression);
    }

    return expression;
  }

  private getRule(tokenType: TokenType) {
    return this.parseRules[tokenType];
  }
}

import assert from 'assert';

const tests: { input: string; expression: Expr }[] = [
  { input: '1', expression: new LiteralExpr(1) },
  {
    input: '- 1 + 23 * 4 + 2 / 5 + 2',
    expression: new BinaryExpr(
      new BinaryExpr(
        new BinaryExpr(
          new UnaryExpr(new Token(TokenType.MINUS, '-', null), new LiteralExpr(1)),
          new Token(TokenType.PLUS, '+', null),
          new BinaryExpr(new LiteralExpr(23), new Token(TokenType.STAR, '*', null), new LiteralExpr(4))
        ),
        new Token(TokenType.PLUS, '+', null),
        new BinaryExpr(new LiteralExpr(2), new Token(TokenType.SLASH, '/', null), new LiteralExpr(5))
      ),
      new Token(TokenType.PLUS, '+', null),
      new LiteralExpr(2)
    ),
  },
  {
    input: 'age + 4 ? 5 : 9 * height',
    expression: new ConditionalExpr(
      new BinaryExpr(new VariableExpr('age'), new Token(TokenType.PLUS, '+', null), new LiteralExpr(4)),
      new LiteralExpr(5),
      new BinaryExpr(new LiteralExpr(9), new Token(TokenType.STAR, '*', null), new VariableExpr('height'))
    ),
  },
];

tests.forEach((test) => {
  const tokens = scanner(test.input);

  const rdParser = new RecursiveDescentParser(tokens);
  assert.deepEqual(rdParser.parse(), test.expression);

  const prattParser = new PrattParser(tokens);
  assert.deepEqual(prattParser.parse(), test.expression);
});

function scanner(input: string): Token[] {
  return input
    .split(' ')
    .map((str) => {
      const numVal = parseFloat(str);
      if (!Number.isNaN(numVal)) {
        return new Token(TokenType.NUMBER, str, parseFloat(str));
      }
      const stringToTokenType: Record<string, TokenType> = {
        '*': TokenType.STAR,
        '+': TokenType.PLUS,
        '/': TokenType.SLASH,
        '-': TokenType.MINUS,
        '?': TokenType.QUESTION_MARK,
        ':': TokenType.COLON,
      };
      const tokenType = stringToTokenType[str];
      if (tokenType) {
        return new Token(tokenType, str, null);
      }
      return new Token(TokenType.IDENTIFIER, str, null);
    })
    .concat([new Token(TokenType.EOF, null, null)]);
}
