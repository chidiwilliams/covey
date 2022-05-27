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

interface Expression {}

class NameExpression implements Expression {
  constructor(public name: string) {}
}

class UnaryExpression implements Expression {
  constructor(public operator: Token, public expression: Expression) {}
}

class BinaryExpression implements Expression {
  constructor(public left: Expression, public operator: Token, public right: Expression) {}
}

class ConditionalExpression implements Expression {
  constructor(public condition: Expression, public thenBranch: Expression, public elseBranch: Expression) {}
}

class LiteralExpression {
  constructor(public value: number) {}
}

class ParseError extends Error {
  constructor(token: Token, message: string) {
    super(`Error at token: ${token.lexeme}: ${message}`);
  }
}

class BaseParser {
  private current = 0;

  constructor(private tokens: Token[]) {}

  protected parse(): Expression {
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
}

class RecursiveDescentParser extends BaseParser {
  constructor(tokens: Token[]) {
    super(tokens);
  }

  /**
   * Grammar:
   * ternary      => term ( "?" ternary ":" ternary )*
   * term         => factor ( ( "+" | "-" ) factor )*
   * factor       => unary ( ( "/" | "*" ) unary )*
   * unary        => ( "!" | "-" ) unary | primary
   * primary      => NUMBER
   */
  override parse(): Expression {
    return this.ternary();
  }

  private ternary(): Expression {
    let expression = this.term();

    if (this.match(TokenType.QUESTION_MARK)) {
      const thenBranch = this.ternary();
      this.consume(TokenType.COLON, 'Expect colon after ternary condition.');
      const elseBranch = this.ternary();
      return new ConditionalExpression(expression, thenBranch, elseBranch);
    }

    return expression;
  }

  private term(): Expression {
    let expression = this.factor();

    while (this.match(TokenType.MINUS, TokenType.PLUS)) {
      const operator = this.previous();
      const right = this.factor();
      expression = new BinaryExpression(expression, operator, right);
    }

    return expression;
  }

  private factor(): Expression {
    let expression = this.unary();

    while (this.match(TokenType.SLASH, TokenType.STAR)) {
      const operator = this.previous();
      const right = this.unary();
      expression = new BinaryExpression(expression, operator, right);
    }

    return expression;
  }

  private unary(): Expression {
    if (this.match(TokenType.BANG, TokenType.MINUS)) {
      const operator = this.previous();
      const expression = this.unary();
      return new UnaryExpression(operator, expression);
    }
    return this.primary();
  }

  private primary(): Expression {
    switch (true) {
      case this.match(TokenType.NUMBER):
        return new LiteralExpression(this.previous().literal!);
      case this.match(TokenType.IDENTIFIER):
        return new NameExpression(this.previous().lexeme!);
      default:
        throw new ParseError(this.peek(), 'Expect expression.');
    }
  }

  private match(...tokenTypes: TokenType[]): boolean {
    for (const tokenType of tokenTypes) {
      if (this.check(tokenType)) {
        this.advance();
        return true;
      }
    }

    return false;
  }
}

enum Precedence {
  NONE,
  TERNARY,
  TERM,
  UNARY,
  FACTOR,
}

type PrefixParseFn = () => Expression;

type InfixParseFn = (left: Expression) => Expression;

interface ParseRule {
  prefix?: PrefixParseFn;
  infix?: InfixParseFn;
  precedence: Precedence;
}

class PrattParser extends BaseParser {
  private number: PrefixParseFn = () => {
    return new LiteralExpression(this.previous().literal!);
  };

  private unary: PrefixParseFn = () => {
    const operator = this.previous();
    const operand = this.parsePrecedence(Precedence.UNARY);
    return new UnaryExpression(operator, operand);
  };

  private binary: InfixParseFn = (left: Expression) => {
    const operator = this.previous();
    const rule = this.getRule(operator.tokenType);
    const right = this.parsePrecedence(rule!.precedence + 1);
    return new BinaryExpression(left, operator, right);
  };

  private ternary: InfixParseFn = (left: Expression) => {
    const thenBranch = this.parsePrecedence(Precedence.TERNARY + 1);
    this.consume(TokenType.COLON, 'Expect colon after ternary condition.');
    const elseBranch = this.parsePrecedence(Precedence.TERNARY + 1);
    return new ConditionalExpression(left, thenBranch, elseBranch);
  };

  private variable: PrefixParseFn = () => {
    return new NameExpression(this.previous().lexeme!);
  };

  private parseRules: Record<TokenType, ParseRule> = {
    NUMBER: { prefix: this.number, precedence: Precedence.NONE },
    MINUS: { prefix: this.unary, infix: this.binary, precedence: Precedence.TERM },
    PLUS: { infix: this.binary, precedence: Precedence.TERM },
    BANG: { precedence: Precedence.NONE },
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

  override parse(): Expression {
    return this.parsePrecedence(Precedence.TERNARY);
  }

  private parsePrecedence(precedence: Precedence): Expression {
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

const tests: { input: string; expression: Expression }[] = [
  { input: '1', expression: new LiteralExpression(1) },
  {
    input: '- 1 + 23 * 4 + 2 / 5 + 2',
    expression: new BinaryExpression(
      new BinaryExpression(
        new BinaryExpression(
          new UnaryExpression(new Token(TokenType.MINUS, '-', null), new LiteralExpression(1)),
          new Token(TokenType.PLUS, '+', null),
          new BinaryExpression(
            new LiteralExpression(23),
            new Token(TokenType.STAR, '*', null),
            new LiteralExpression(4)
          )
        ),
        new Token(TokenType.PLUS, '+', null),
        new BinaryExpression(new LiteralExpression(2), new Token(TokenType.SLASH, '/', null), new LiteralExpression(5))
      ),
      new Token(TokenType.PLUS, '+', null),
      new LiteralExpression(2)
    ),
  },
  {
    input: 'age + 4 ? 5 : 9 * height',
    expression: new ConditionalExpression(
      new BinaryExpression(new NameExpression('age'), new Token(TokenType.PLUS, '+', null), new LiteralExpression(4)),
      new LiteralExpression(5),
      new BinaryExpression(new LiteralExpression(9), new Token(TokenType.STAR, '*', null), new NameExpression('height'))
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
