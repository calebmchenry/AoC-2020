const file = await Deno.readTextFile('./input.txt');

let count = 0;

type InstructionParams = {
    index: number;
    num: number;
}
const cmds: Record<string, (op: InstructionParams) => number> = {
    'nop': ({index}) => index + 1,
    'jmp': ({num, index}) => index + num,
    'acc': ({num, index}) => {
        count += num
        return index + 1
    }
}

const linesVisited: number[] = [];

function solve(content: string) {
    let i = 0;
    const instructions = content.split('\n');
    while(!linesVisited.includes(i+1)) {
        linesVisited.push(i+1);
        const [name, numStr] = instructions[i].split(' ');
        const num = parseInt(numStr);
        i = cmds[name]({num, index: i});
    }
    return count; 
}

console.log(solve(file));
