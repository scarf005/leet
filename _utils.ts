const problem = /^(?<number>\d+)\.(?<slug>.+)\.scala$/
export const parseProblem = (path: string) =>
  problem.exec(path)?.groups as { number: string; slug: string } | undefined

export const exitWith = (message: string) => {
  console.error(message)
  Deno.exit(0)
}
