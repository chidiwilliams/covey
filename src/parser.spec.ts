import assert from 'assert';
import {
  BinaryExpr,
  ConditionalExpr,
  Expr,
  LiteralExpr,
  PrattParser,
  RecursiveDescentParser,
  scanner,
  Token,
  TokenType,
  UnaryExpr,
  VariableExpr,
} from './parser';

const tests: { input: string; expression: Expr }[] = [
  { input: '1', expression: new LiteralExpr(1) },
  {
    input: '- - - 1',
    expression: new UnaryExpr(
      new Token(TokenType.MINUS, '-', null),
      new UnaryExpr(
        new Token(TokenType.MINUS, '-', null),
        new UnaryExpr(new Token(TokenType.MINUS, '-', null), new LiteralExpr(1))
      )
    ),
  },
  {
    input: '1 + 2 + 3',
    expression: new BinaryExpr(
      new BinaryExpr(new LiteralExpr(1), new Token(TokenType.PLUS, '+', null), new LiteralExpr(2)),
      new Token(TokenType.PLUS, '+', null),
      new LiteralExpr(3)
    ),
  },
  {
    input: '- 1 * - 1 + - 2 / - 2',
    expression: new BinaryExpr(
      new BinaryExpr(
        new UnaryExpr(new Token(TokenType.MINUS, '-', null), new LiteralExpr(1)),
        new Token(TokenType.STAR, '*', null),
        new UnaryExpr(new Token(TokenType.MINUS, '-', null), new LiteralExpr(1))
      ),
      new Token(TokenType.PLUS, '+', null),
      new BinaryExpr(
        new UnaryExpr(new Token(TokenType.MINUS, '-', null), new LiteralExpr(2)),
        new Token(TokenType.SLASH, '/', null),
        new UnaryExpr(new Token(TokenType.MINUS, '-', null), new LiteralExpr(2))
      )
    ),
  },
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
      new BinaryExpr(
        new VariableExpr(new Token(TokenType.IDENTIFIER, 'age', null)),
        new Token(TokenType.PLUS, '+', null),
        new LiteralExpr(4)
      ),
      new LiteralExpr(5),
      new BinaryExpr(
        new LiteralExpr(9),
        new Token(TokenType.STAR, '*', null),
        new VariableExpr(new Token(TokenType.IDENTIFIER, 'height', null))
      )
    ),
  },
  {
    input: '0 ? 1 : 1 ? 2 : 3',
    expression: new ConditionalExpr(
      new LiteralExpr(0),
      new LiteralExpr(1),
      new ConditionalExpr(new LiteralExpr(1), new LiteralExpr(2), new LiteralExpr(3))
    ),
  },
  {
    input: '1 ? 0 ? 2 : 3 : 1',
    expression: new ConditionalExpr(
      new LiteralExpr(1),
      new ConditionalExpr(new LiteralExpr(0), new LiteralExpr(2), new LiteralExpr(3)),
      new LiteralExpr(1)
    ),
  },
];

tests.forEach((test) => {
  const tokens = scanner(test.input);

  const rdParser = new RecursiveDescentParser(tokens);
  assert.deepEqual(rdParser.parse(), test.expression, 'recursive descent');

  const prattParser = new PrattParser(tokens);
  assert.deepEqual(prattParser.parse(), test.expression, 'pratt');
});
