import Benchmark from 'benchmark';
import { PrattParser, RecursiveDescentParser, scanner } from './parser';

const allInputs = [
  '1 + 3 - 5',
  '- 1 + 23 * 4 + age + 4 ? 5 : 9 * height / 5 + 2',
  '2 / 89 + 37 ? 9 : 17 * 90 - 3 + 7 / 1 - - 4 + 89 * 3 + 1 + 9 - 47 - - 9 + 2 ? 4 : 37 * 9 + 0 / 21 + 8 - 9 - 2 / 4',
];

allInputs.forEach((input) => {
  const tokens = scanner(input);
  const suite = new Benchmark.Suite(input);

  console.log(`Input: ${input}`);
  suite
    .add('Recursive Descent', () => {
      const parser = new RecursiveDescentParser(tokens);
      parser.parse();
    })
    .add('Pratt', () => {
      const parser = new PrattParser(tokens);
      parser.parse();
    })
    .on('cycle', (event: any) => {
      console.log(String(event.target));
    })
    .run({ async: false });
  console.log();
});
