/**
 * COVEY
 *
 * Covey implements an expression parser using two different parsing techniques:
 *    - Recursive descent parsing ({@link RecursiveDescentParser})
 *    - Pratt parsing ({@link PrattParser})
 *
 * Both parsers receive a list of tokens and return an Abstract Syntax Tree (AST) of expressions.
 *
 * First, we'll define the types and classes for the input tokens and the output expressions:
 */

/**
 * A Token is a semantic unit of the source expression.
 */
export class Token {
  /**
   * @param tokenType The type of the token
   * @param lexeme The string value of the token as it occurs in the source expression
   * @param literal The literal value of the token. For example, the number value of a numeric token.
   */
  constructor(public tokenType: TokenType, public lexeme: string | null, public literal: number | null) {}
}

/**
 * A TokenType represent a type of token.
 */
export enum TokenType {
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

/**
 * All expressions extend this base class.
 */
export class Expr {}

/**
 * A literal expression holds a literal value.
 *
 * For example:
 *    "3" => new LiteralExpr(3)
 */
export class LiteralExpr extends Expr {
  /**
   * @param value The literal value
   */
  constructor(public value: number) {
    super();
  }
}

/**
 * A variable expression holds a variable.
 *
 * For example:
 *    "height" => new VariableExpr("height")
 */
export class VariableExpr extends Expr {
  /**
   * @param name The token holding the string representation of the variable
   */
  constructor(public name: Token) {
    super();
  }
}

/**
 * A unary expression represents an operation with one operand.
 *
 * For example:
 *    "-32" => new UnaryExpr(new Token("-"), new LiteralExpr(32))
 */
export class UnaryExpr extends Expr {
  /**
   * @param operator The unary operator
   * @param operand The operand
   */
  constructor(public operator: Token, public operand: Expr) {
    super();
  }
}

/**
 * A binary expression represents an operation on two operands.
 *
 * For example:
 *    "2 + 8" => new BinaryExpr(new LiteralExpr(2), new Token("+"), new LiteralExpr(8))
 */
export class BinaryExpr extends Expr {
  /**
   * @param left The left operand
   * @param operator The binary operator
   * @param right The right operand
   */
  constructor(public left: Expr, public operator: Token, public right: Expr) {
    super();
  }
}

/**
 * A conditional expression represents a conditional operation.
 *
 * For example:
 *    "1 ? 2 : 3" => new ConditionalExpr(new LiteralExpr(1), new LiteralExpr(2), new LiteralExpr(3))
 */
export class ConditionalExpr extends Expr {
  /**
   * @param condition The condition to be met. The expression is to be interpreted as a boolean value.
   * @param thenBranch The resulting expression if the condition is true
   * @param elseBranch The resulting expression if the condition is false
   */
  constructor(public condition: Expr, public thenBranch: Expr, public elseBranch: Expr) {
    super();
  }
}

/**
 * We can now implement a scanner to convert the string
 * representation of a source expression into a list of tokens.
 */

/**
 * First, we declare a map of token strings to their respective types...
 */
const stringToTokenType: Record<string, TokenType> = {
  '*': TokenType.STAR,
  '+': TokenType.PLUS,
  '/': TokenType.SLASH,
  '-': TokenType.MINUS,
  '?': TokenType.QUESTION_MARK,
  ':': TokenType.COLON,
};

/**
 * Then, we define the scanner. This naive implementation doesn't handle whitespaces
 * and errors adequately. It's only meant for illustration/testing purposes.
 */

/**
 * The scanner returns the tokens in an expression string. It splits the string by a single
 * space character. Then it checks if the token is a number, or if it matches the pre-declared
 * types, else it sets it as an identifier token. At the end of the list, the scanner adds an
 * EOF token which the parser later uses to know when it has reached the final token.
 *
 * @param input  The source expression
 * @returns A list of the tokens in the expression
 */
export function scanner(input: string): Token[] {
  // First, we split the input expression by a single space character
  return (
    input
      .split(' ')
      .map((str) => {
        // If a string in the list _can_ be a number, then it _is_ a number...
        const numVal = parseFloat(str);
        if (!Number.isNaN(numVal)) {
          return new Token(TokenType.NUMBER, str, parseFloat(str));
        }

        // If not: if it matches one of the mapped token
        // types, then we create a token of that type
        const tokenType = stringToTokenType[str];
        if (tokenType) {
          return new Token(tokenType, str, null);
        }

        // If it is none of those token types,
        // we'll just make it an identifier token
        return new Token(TokenType.IDENTIFIER, str, null);
      })
      // At the end of the list of tokens, we'll add an EOF token. The parser
      // will use this token to know when it has reached the end of the token list.
      .concat([new Token(TokenType.EOF, null, null)])
  );
}

/**
 * That scanner won't be winning any Scanner of the Year awards, but it'll do for now.
 *
 * Let's implement parsing. We'll define a ParseError class:
 */

/**
 * A ParseError is an error that occurs during parsing.
 */
class ParseError extends Error {
  /**
   * @param token The token being parsed when the error occured
   * @param message The error message
   */
  constructor(private token: Token, message: string) {
    super(`Error at token: ${token.lexeme}: ${message}`);
  }
}

/**
 * Then, we'll define a (base) Parser, which the recursive
 * descent parser and the Pratt parser both extend.
 */

/**
 * A parser parses a list of tokens into an AST of expressions.
 * This class contains the base constructor for the two parsers as
 * well as some helper functions for peeking around the token list
 * and matching token types.
 */
class Parser {
  /**
   * The index of the current token being parsed.
   */
  private current = 0;

  /**
   * @param tokens The tokens to be parsed
   */
  constructor(private tokens: Token[]) {}

  /**
   * Returns the parsed expression tree.
   * This method is to be implemented by the parsers.
   */
  protected parse(): Expr {
    throw new Error('not implemented');
  }

  /**
   * The next few methods are (possibly boring) helper methods the parsers use
   * to walk through the list of tokens. You can skip through them quickly to
   * the next class if you'd like.
   */

  /**
   * Returns the next token in the list
   */
  protected peek(): Token {
    return this.tokens[this.current]!;
  }

  /**
   * Returns the previous token in the list
   */
  protected previous(): Token {
    return this.tokens[this.current - 1]!;
  }

  /**
   * Advances to the next token (if possible) and returns the previous one
   */
  protected advance(): Token {
    if (!this.isAtEnd()) {
      this.current++;
    }
    return this.previous();
  }

  /**
   * Returns true if all the tokens have been parsed
   */
  protected isAtEnd(): boolean {
    return this.peek().tokenType === TokenType.EOF;
  }

  /**
   * Checks if the next token matches the given type. If it does, we
   * advance to the next index and return the previous token. If not,
   * we throw a {@link ParseError} with the given message.
   *
   * @param tokenType Expected token type
   * @param message Error message
   */
  protected consume(tokenType: TokenType, message: string): Token {
    if (this.check(tokenType)) {
      return this.advance();
    }
    throw new ParseError(this.peek(), message);
  }

  /**
   * Returns true if the next set of tokens match the given token types.
   * @param tokenTypes Expected token types
   */
  protected match(...tokenTypes: TokenType[]): boolean {
    for (const tokenType of tokenTypes) {
      if (this.check(tokenType)) {
        this.advance();
        return true;
      }
    }
    return false;
  }

  /**
   * Returns true if we're not at the end of the token list and
   * the next token matches the given token type.
   * @param tokenType Expected token type
   */
  private check(tokenType: TokenType) {
    if (this.isAtEnd()) {
      return false;
    }
    return this.peek().tokenType === tokenType;
  }
}

/**
 * Phew... Hopefully, those helper methods weren't too much to go through
 * and we can now return to more interesting business.
 *
 * Before we write the parsers, let's talk about precedence.
 *
 * Precedence determines which operations to perform first in an expression.
 * For example, the expression `2 + 3 * 5` is evaluated as "2 plus the result
 * of 3 times 5", because "*" has a higher precedence than "+", even though "+"
 * appears first in the expression.
 *
 * In school, you may have learned about precedence as PEMDAS or BODMAS:
 * "Brackets, Or, Division, Multiplication, Addition, and Subtraction", which
 * describes the precedence of the operations from highest to lowest. Here,
 * we'll use the {@link https://en.cppreference.com/w/c/language/operator_precedence C Reference Manual}
 * as a guide.
 *
 * The goal of the parsers, regardless of the parsing strategy, is to parse the
 * expressions according to these precedence rules, with the highest precedence
 * operations lower down the tree (at the leaf nodes). The resulting parse tree of
 * the previous expression, for example, should be:
 *
 *    new BinaryExpr(2, '+', new BinaryExpr(3, '*', 5))
 *
 * Both parsers work with a similar strategy: They walk through the list of tokens
 * returned by the scanner, keeping track of a stack. At a token that represents
 * a low precedence operation, the parser pushes the operands to the stack. And at
 * a higher precedence operation, it pops the stack to create the expressions, until
 * it builds up the entire tree. This "stack" for managing the operation precedence
 * is implemented with the function call stack.
 */

/**
 * In recursive descent parsing, we may first define a grammar for the expression
 * language in LL(1) form:
 *
 * expression => ternary
 * ternary    => term "?" ternary ":" ternary
 * term       => factor ( ( "-" | "+") factor )*
 * factor     => unary ( ( "*" | "/") unary )*
 * unary      => ( "-" ) unary | primary
 * primary    => NUMBER | IDENTIFIER
 *
 * This grammar describes the precedence from lowest (top) to highest (bottom).
 *    - An "expression" is expression with a precedence of "ternary" or lower
 *    - A "ternary" consists of a conditional with a precedence of "term" or lower,
 *      a then branch with a precedence of "ternary" or lower, and an else branch
 *      with a precedence of "ternary" or lower
 *    - A "term" consists of an expression with a precedence of "factor" or lower,
 *      followed by zero of more instances of a "-" or "+" token followed by an
 *      expression with a precedence of "factor" or lower
 *    - and so on, until...
 *    - A "primary" is either a number or an identifier
 *
 * To parse an expression using recursive descent parsing, we start from the top
 * of the grammar and try to parse using the grammar rules until we reach the
 * terminal nodes.
 */

/**
 * A recursive descent parser implement recursive descent parsing.
 */
export class RecursiveDescentParser extends Parser {
  /**
   * Parses an expression with a precedence of "ternary" or higher.
   */
  override parse(): Expr {
    return this.ternary();
  }

  /**
   * Parses an expression with a precedence of "ternary" or higher.
   * It first parses a "term" expression, then if it matches a question mark token
   * next, it parses the then and else branches, both with precedences of "ternary"
   * or lower.
   */
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

  /**
   * Parses an expression with a precedence of "term" or higher.
   * It first parses a "factor" expression. Then, if it can match a minus
   * or plus sign token, it parses another "factor" expression and continues
   * to do so to make a binary expression.
   */
  private term(): Expr {
    let expression = this.factor();

    while (this.match(TokenType.MINUS, TokenType.PLUS)) {
      const operator = this.previous();
      const right = this.factor();
      expression = new BinaryExpr(expression, operator, right);
    }

    return expression;
  }

  /**
   * Parses an expression with a precedence of "factor" or higher.
   * It first parses a "unary" expression. Then, if it can match a slash or star
   * token, it parses another "unary" expression and continues to do so to make a
   * binary expression.
   */
  private factor(): Expr {
    let expression = this.unary();

    while (this.match(TokenType.SLASH, TokenType.STAR)) {
      const operator = this.previous();
      const right = this.unary();
      expression = new BinaryExpr(expression, operator, right);
    }

    return expression;
  }

  /**
   * Parses an expression with a precedence of "unary" or higher.
   * It first checks if a bang or minus token exists. If it does, it recursively
   * parses a "unary" expression. If not, it parses a "primary" expression.
   */
  private unary(): Expr {
    if (this.match(TokenType.BANG, TokenType.MINUS)) {
      const operator = this.previous();
      const expression = this.unary();
      return new UnaryExpr(operator, expression);
    }
    return this.primary();
  }

  /**
   * Parses an expression with a precedence of "primary".
   * A primary expression can either be a literal number or a variable
   * expression. If we do not match either of those, we throw a parse error.
   */
  private primary(): Expr {
    switch (true) {
      case this.match(TokenType.NUMBER):
        return new LiteralExpr(this.previous().literal!);
      case this.match(TokenType.IDENTIFIER):
        return new VariableExpr(this.previous());
      default:
        throw new ParseError(this.peek(), 'Expect expression.');
    }
  }
}

/**
 * Next, we'll implement Pratt parsing.
 *
 * Pratt parsing is as follows:
 *     - Every expression consists of a "prefix expression" followed by
 *       zero or more "infix expressions" of the same or lower precedence
 *     - An prefix expression is a number, a variable, or a "unary expression"
 *     - A unary expression is a unary token (e.g. "-") followed by an
 *       expression with a precedence of at least UNARY
 *     - An infix expression is a "binary expression" (e.g. "-", "+", "/", "*")
 *       or a "ternary expression" ("?:")
 *     - A binary expression matches the binary operator and the right operand,
 *       which must be an expression with a precedence greater than that of the
 *       binary operator
 *     - A ternary expression matches the "?" and the then and else branches,
 *       which must both have precedences of at least TERNARY
 *
 * For example, the expression "- age + 23 / 5 - 10" is parsed as:
 *     - A prefix "- age" followed by an infix "+ 23 / 5" and another infix "- 10"
 *         - "- age" parses as the unary operator "-" followed by the expression "age"
 *             - "age" is prefix "age" followed by no infixes
 *         - "+ 23 / 5" is the binary operator "+" followed by the expression "23 / 5"
 *             - "23 / 5" is prefix "23" followed by the infix "/ 5"
 *         - "- 10" is the binary operator "-" followed by the expression "10"
 *             - "10" is, well, 10
 *
 * That was a bit of a mouthful, but hopefully it passes across the point. The
 * key idea behind Pratt parsing lies in this statement:
 *      "Every expression consists of a 'prefix expression' followed by
 *       zero or more 'infix expressions' of the same or lower precedence"
 *
 * We start parsing with the lowest possible precedence (TERNARY) and following
 * this rule until we parse the entire expression.
 *
 * Let's define a few helper types:
 */

/**
 * Operator precedence, from the lowest (NONE = 0) to the highest (UNARY)
 */
enum Precedence {
  NONE,
  TERNARY,
  TERM,
  FACTOR,
  UNARY,
}

/**
 * A prefix parse function parses a prefix expression
 */
type PrefixParseFn = () => Expr;

/**
 * An infix parse function accepts the left operand of an
 * infix expression and parses the rest of the expression
 */
type InfixParseFn = (left: Expr) => Expr;

/**
 * A parse rule defines the parse functions for a token type
 * as well as its precedence (only applies when used as infix)
 */
interface ParseRule {
  prefix?: PrefixParseFn;
  infix?: InfixParseFn;
  precedence: Precedence;
}

/**
 * A Pratt parser implements Pratt parsing
 */
export class PrattParser extends Parser {
  /**
   * Parses an expression with a precedence of TERNARY or higher
   */
  override parse(): Expr {
    return this.parsePrecedence(Precedence.TERNARY);
  }

  /**
   * Parses an expression at a given precedence. It checks the next token and
   * then parses it as an prefix expression. Then it checks the (new) next token,
   * and if the infix expression corresponding to that token has a precedence
   * that is at least the same as the given precedence, it parses it as an infix
   * expression. Parsing stops when the next token matches an infix expression
   * with a lower precedence or when there are no more tokens to parse.
   * @param precedence
   */
  private parsePrecedence(precedence: Precedence): Expr {
    const nextToken = this.advance();
    const prefixRule = this.getRule(nextToken.tokenType).prefix;
    if (!prefixRule) {
      throw new Error('Expect expression.');
    }

    let expression = prefixRule();

    while (this.getRule(this.peek().tokenType).precedence >= precedence) {
      const nextToken = this.advance();
      const infixRule = this.getRule(nextToken.tokenType).infix!;
      expression = infixRule(expression);
    }

    return expression;
  }

  /**
   * The next few methods parse prefix and infix expressions...
   */

  /**
   * Parses a number. Creates and returns a literal expression
   * containing the literal value of the previous token.
   */
  private number: PrefixParseFn = () => {
    return new LiteralExpr(this.previous().literal!);
  };

  /**
   * Parses a variable expression. The previous token holds
   * the variable name.
   */
  private variable: PrefixParseFn = () => {
    return new VariableExpr(this.previous());
  };

  /**
   * Parses a unary expression. The operator is the previous token,
   * while the operand is the result of parsing at a precedence of
   * UNARY.
   */
  private unary: PrefixParseFn = () => {
    const operator = this.previous();
    const operand = this.parsePrecedence(Precedence.UNARY);
    return new UnaryExpr(operator, operand);
  };

  /**
   * Parses a binary expression. The left operand is provided as an
   * argument, the operand is the previous token, and the right operand
   * is the result of parsing at a precedence of the operator's precedence
   * plus 1.
   */
  private binary: InfixParseFn = (left: Expr) => {
    const operator = this.previous();
    const rule = this.getRule(operator.tokenType);
    const right = this.parsePrecedence(rule.precedence + 1);
    return new BinaryExpr(left, operator, right);
  };

  /**
   * Parses a ternary expression. The condition is provided as an argument,
   * and the then and else branches are the results of parsing at a precedence
   * of TERNARY.
   */
  private ternary: InfixParseFn = (left: Expr) => {
    const thenBranch = this.parsePrecedence(Precedence.TERNARY);
    this.consume(TokenType.COLON, 'Expect colon after ternary condition.');
    const elseBranch = this.parsePrecedence(Precedence.TERNARY);
    return new ConditionalExpr(left, thenBranch, elseBranch);
  };

  /**
   * To wrap things up, we define:
   */

  /**
   * A map of token types to their corresponding parsing functions and precedences
   */
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

  /**
   * ...and a function that:
   */

  /**
   *
   * Returns the parsing functions and precedences for a token type
   */
  private getRule(tokenType: TokenType) {
    return this.parseRules[tokenType];
  }
}
