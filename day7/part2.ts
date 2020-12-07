const content = await Deno.readTextFile("./input.txt");

type NumColor = {
  number: number;
  color: string;
};

function solve(str: string) {
  const lines = str.split("\n");
  const upColorMap: Record<string, NumColor[]> = lines.reduce(colorMap, {});
  console.log(upColorMap);
  return traverseColorMap(upColorMap) - 1;
}

function colorMap(prev: Record<string, NumColor[]>, current: string): Record<string, NumColor[]> {
      let [color, bagContents] = current.split("bags contain");
      color = color.trim();
      const colors = bagContents.includes("no other bags")
        ? []
        : bagContents.split(", ").map((s) => {
            const number = parseInt(s.substring(0, 2));
            const color = s
              .substring(2)
              .replace("bags", "")
              .replace("bag", "")
              .replace(".", "")
              .trim();
            return { number, color };
          });
      return { ...prev, [color]: colors };
}

function traverseColorMap(
  map: Record<string, NumColor[]>,
  keyNumColor = { color: "shiny gold", number: 1 },
  nest = 1
): number {
  const colors = map[keyNumColor.color] ?? [];
  const upwardSum = colors
    .map((entry) => {
      return traverseColorMap(map, entry, keyNumColor.number * nest);
    })
    .reduce((prev, current) => {
      return prev + current;
    }, 0);
  return keyNumColor.number * nest + upwardSum;
}

console.log(solve(content));
