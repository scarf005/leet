#!/usr/bin/env -S deno run --allow-run --allow-env -RW
import $ from "@david/dax"
import { exitWith, parseProblem } from "./_utils.ts"

const files = await $`git diff --name-only --cached`.printCommand().lines()

const numbers = files.flatMap((file) => parseProblem(file)?.number ?? [])

if (files.length !== numbers.length) exitWith("non-problem in staged files")
if (numbers.length === 0) exitWith("No problems to commit")

await $`git commit --message "feat: ${numbers.join(", ")}"`.printCommand()
