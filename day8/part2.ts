const file = await Deno.readTextFile("./input.txt");

type InstructionParams = {
  index: number;
  num: number;
};


function swapInstruction([name, num]: [string, number]): [string, number] {
    if(name === 'jmp') {
        return ['nop', num]
    } else if(name === 'nop') {
        return ['jmp', num];
    } else {
        return [name, num];
    }
}

function parseInstructions(content: string): [string, number][] {
  const instructions = content.split("\n");
  return instructions.map((instruction) => {
    const [name, numStr] = instruction.split(" ");
    const num = parseInt(numStr);
    return [name, num];
  });
}

function isInfiniteLoop(instructions: [string, number][]): [boolean, number] {
  let count = 0;
  let i = 0;
  let linesVisited: number[] = [];
  const cmds: Record<string, (op: InstructionParams) => number> = {
    nop: ({ index }) => index + 1,
    jmp: ({ num, index }) => index + num,
    acc: ({ num, index }) => {
      count += num;
      return index + 1;
    },
  };
  while (!linesVisited.includes(i + 1) && i < instructions.length) {
    linesVisited.push(i + 1);
    const [name, num] = instructions[i]
    i = cmds[name]({ num, index: i });
  }
  return [i < instructions.length, count];
}

function solve(content: string) {
    let isLoop = true;
    let indexToAlter = 0;
    while(isLoop) {
        const instructions = parseInstructions(content);
        instructions[indexToAlter] = swapInstruction(instructions[indexToAlter]);
        const [loop, count] = isInfiniteLoop(instructions);
        console.log(indexToAlter, loop, count);
        isLoop = loop;
        if(!isLoop) {
            return count;
        }
        indexToAlter++;
    }
}

console.log(solve(file));
