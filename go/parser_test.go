package covey

import (
	"reflect"
	"testing"
)

var tests = []struct {
	input string
	want  Expr
}{
	{
		input: "- - - 1",
		want: UnaryExpr{
			Token{TokenTypeMinus, "-", nil}, UnaryExpr{
				Token{TokenTypeMinus, "-", nil},
				UnaryExpr{
					Token{TokenTypeMinus, "-", nil},
					LiteralExpr{1},
				},
			},
		},
	},
	{
		input: "2 - 3 - 4",
		want: BinaryExpr{
			BinaryExpr{
				LiteralExpr{2},
				Token{TokenTypeMinus, "-", nil},
				LiteralExpr{3},
			},
			Token{TokenTypeMinus, "-", nil},
			LiteralExpr{4},
		},
	},
	{
		input: "age + 4 ? 5 : 9 * height",
		want: ConditionalExpr{
			BinaryExpr{
				VariableExpr{Token{TokenTypeIdentifier, "age", nil}},
				Token{TokenTypePlus, "+", nil},
				LiteralExpr{4},
			},
			LiteralExpr{5},
			BinaryExpr{
				LiteralExpr{9},
				Token{TokenTypeStar, "*", nil},
				VariableExpr{Token{TokenTypeIdentifier, "height", nil}},
			},
		},
	},
}

func TestRecursiveDescentParser_Parse(t *testing.T) {
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			tokens := scanner(tt.input)
			p := NewRecursiveDescentParser(tokens)
			if got := p.Parse(); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Parse() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestPrattParser_Parse(t *testing.T) {
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			tokens := scanner(tt.input)
			p := NewPrattParser(tokens)
			if got := p.Parse(); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Parse() = %v, want %v", got, tt.want)
			}
		})
	}
}

func benchmarkPrattParser(b *testing.B, input string) {
	for i := 0; i < b.N; i++ {
		parser := NewPrattParser(scanner(input))
		parser.Parse()
	}
}

func benchmarkRecursiveDescentParser(b *testing.B, input string) {
	for i := 0; i < b.N; i++ {
		parser := NewRecursiveDescentParser(scanner(input))
		parser.Parse()
	}
}

var shortExpr = "1 + 3 - 5"

func BenchmarkRecursiveDescentParserShort(b *testing.B) {
	benchmarkRecursiveDescentParser(b, shortExpr)
}

func BenchmarkPrattParserShort(b *testing.B) {
	benchmarkPrattParser(b, shortExpr)
}

var longExpr = "- 1 + 23 * 4 + age + 4 ? 5 : 9 * height / 5 + 2"

func BenchmarkRecursiveDescentParserLong(b *testing.B) {
	benchmarkRecursiveDescentParser(b, longExpr)
}

func BenchmarkPrattParserLong(b *testing.B) {
	benchmarkPrattParser(b, longExpr)
}

var veryLongExpr = "2 / 89 + 37 ? 9 : 17 * 90 - 3 + 7 / 1 - - 4 + 89 * 3 + 1 + 9 - 47 - - 9 + 2 ? 4 : 37 * 9 + 0 / 21 + 8 - 9 - 2 / 4"

func BenchmarkRecursiveDescentParserVeryLong(b *testing.B) {
	benchmarkRecursiveDescentParser(b, veryLongExpr)
}

func BenchmarkPrattParserVeryLong(b *testing.B) {
	benchmarkPrattParser(b, veryLongExpr)
}
