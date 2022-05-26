class Token {
  constructor(public tokenType: TokenType, public lexeme: string | null, public literal: number | null) {}
}

enum TokenType {
  PLUS = 'PLUS',
  MINUS = 'MINUS',
  TILDE = 'TILDE',
  BANG = 'BANG',
  NAME = 'NAME',
  NUMBER = 'NUMBER',
  EOF = 'EOF',
  STAR = 'STAR',
  SLASH = 'SLASH',
  CARET = 'CARET',
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

interface Parser {
  parse(): Expression;
}

class ParseError extends Error {
  constructor(token: Token, message: string) {
    super(message);
  }
}

class RecursiveDescentParser implements Parser {
  private current = 0;

  constructor(private tokens: Token[]) {}

  /**
   * Grammar:
   * ternary      => term ( "?" ternary ":" ternary )*
   * term         => factor ( ( "+" | "-" ) factor )*
   * factor       => unary ( ( "/" | "*" ) unary )*
   * unary        => ( "!" | "-" ) unary | primary
   * primary      => NUMBER
   */
  parse(): Expression {
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

  private consume(tokenType: TokenType, message: string): Token {
    if (this.check(tokenType)) {
      return this.advance();
    }
    throw new ParseError(this.peek(), message);
  }

  private advance() {
    if (!this.isAtEnd()) {
      this.current++;
    }
    return this.previous();
  }

  private previous(): Token {
    return this.tokens[this.current - 1];
  }

  private check(tokenType: TokenType) {
    if (this.isAtEnd()) {
      return false;
    }

    return this.peek().tokenType === tokenType;
  }

  private isAtEnd() {
    return this.peek().tokenType === TokenType.EOF;
  }

  private peek(): Token {
    return this.tokens[this.current];
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
  const parser = new RecursiveDescentParser(tokens);
  assert.deepEqual(parser.parse(), test.expression);
});

function scanner(input: string): Token[] {
  return input
    .split(' ')
    .map((str) => {
      const numVal = parseFloat(str);
      if (!Number.isNaN(numVal)) {
        return new Token(TokenType.NUMBER, str, parseFloat(str));
      }
      const stringToTokenType: { [k: string]: TokenType } = {
        '*': TokenType.STAR,
        '+': TokenType.PLUS,
        '/': TokenType.SLASH,
        '-': TokenType.MINUS,
        '^': TokenType.CARET,
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
