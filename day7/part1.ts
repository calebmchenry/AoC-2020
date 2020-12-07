const content = await Deno.readTextFile('./input.txt');

function solve(str: string) {
    const lines = str.split('\n');
    const upColorMap: Record<string, string[]> = lines.reduce((prev: Record<string, string[]>, current) => {
            let [color, bagContents] = current.split('bags contain');
            color = color.trim();
            const colors = bagContents.includes('no other bags') ? [] :bagContents.split(', ').map(s => s.substring(2).replace('bags', '').replace('bag', '').replace('.', '').trim());
            const partialMap=  colors.forEach((c) => {
                const currentOptions = prev[c] ?? [];
                const newOptions = Array.from(new Set([...currentOptions, color]));
                prev[c] = newOptions;
            }, {})
			return prev
        }, {});
    console.log(upColorMap);
    // return traverseColorMap(upColorMap);
    return traverseColorMap(upColorMap).length;
}

// function traverseColorMap(map: Record<string, string[]>, keyColor = "shiny gold"): number {
//     const colors = map[keyColor] ?? [];
//     if(colors.length === 0) {
//         return 1;
//     } else {
//         return colors.reduce((prev, color) => {
//             return prev + traverseColorMap(map, color);
//         }, 0);
//     }
// }

function traverseColorMap(map: Record<string, string[]>, keyColor = "shiny gold"): string[] {
     const colors = map[keyColor] ?? [];
     const upwardColors = colors.flatMap(c => {
         return traverseColorMap(map, c);
     });
     return unique([...colors, ...upwardColors]);
};

function unique(array: string[]): string[] {
    return Array.from(new Set([...array]))
}

console.log(solve(content));
