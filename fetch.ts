#!/usr/bin/env -S deno run -RW --allow-run --allow-env --allow-net=leetcode.com
import { Command } from "@cliffy/command"
import { gray, green } from "@std/fmt/colors"
import { dedent } from "@std/text/unstable-dedent"
import { open } from "@opensrc/deno-open"
import { Mobius } from "graphql-mobius"
import $ from "@david/dax"

const typeDefs = /* GraphQL */ `
  type CodeSnippet {
    lang: String!
    langSlug: String!
    code: String!
  }
  type Question {
    questionFrontendId: String!
    title: String!
    titleSlug: String!
    exampleTestcaseList: [String!]!
    metaData: String!
    codeSnippets: [CodeSnippet!]!
  }
  type DailyQuestion {
    question: Question!
  }
  type Query {
    question(titleSlug: String!): Question!
    activeDailyCodingChallengeQuestion(): DailyQuestion!
  }
`

const question = {
  titleSlug: true,
  questionFrontendId: true,
  codeSnippets: { code: true, lang: true },
} as const

const client = new Mobius({ url: "https://leetcode.com/graphql", typeDefs })

const queryQuestion = (titleSlug: string) =>
  client.query({ question: { where: { titleSlug }, select: question } })
    .then((res) => res!.question)

const queryDailyQuestion = () =>
  client.query({ activeDailyCodingChallengeQuestion: { select: { question } } })
    .then((res) => res!.activeDailyCodingChallengeQuestion.question)

export const tryParseURL = (url: string | URL) => {
  try {
    return new URL(url)
  } catch {
    return undefined
  }
}

export const extractProblemName = (url: string | URL) => {
  const parsed = tryParseURL(url)
  if (!parsed) return undefined
  if (parsed.hostname !== "leetcode.com") return undefined

  const matchResult = /problems\/(?<problem>[^/]+)\/.*/.exec(parsed.pathname)
  if (!matchResult?.groups) return undefined
  return matchResult.groups.problem
}

const fetchProblem = async (url?: string) => {
  const { questionFrontendId, codeSnippets, titleSlug } =
    await (url ? queryQuestion(extractProblemName(url)!) : queryDailyQuestion())

  const id = questionFrontendId.toString().padStart(4, "0")
  const scala = codeSnippets.find((s) => s.lang === "Scala")
  if (!scala) return
  const path = `${id}.${titleSlug}.scala`
  const code = scala.code.replace(
    /(?<before>.*)(?<brace>\{)(?<after>.*)/s,
    `$<before>$<brace>\n      ???$<after>`,
  )
  const name = /def\s+(?<name>\w+)\s*\(.*\).*/.exec(scala.code)?.groups?.name

  await Deno.writeTextFile(
    path,
    dedent`
      package leet.\`${id}\`

      ${code}

      import munit.FunSuite

      class Suite extends FunSuite:
          import Solution.*
          import upickle.default.*

          List(
          ).foreach { case (input, expected) =>
              test(s"${name}($input) = $expected"):
                  assertEquals(${name}(input), expected)
          }
      `,
    { createNew: true },
  )
  await $`scala fmt ${path}`.quiet()
  console.log(dedent`
    Fetched ${gray(`https://leetcode.com/problems/${titleSlug}`)}
    Created ${green(path)}
  `)
}

const openInBrowser = async (path: string) => {
  const regex = /^\d+\.(?<slug>.+)\.scala$/
  const slug = regex.exec(path)?.groups?.slug
  if (!slug) return
  await open(`https://leetcode.com/problems/${slug}`)
}

if (import.meta.main) {
  const open = new Command()
    .description("Open the problem in browser.")
    .arguments("<path:string>")
    .action(async (_, path) => await openInBrowser(path))

  await new Command()
    .name("fetch")
    .description(
      "Fetch a problem from LeetCode and create a boilerplate file. If no URL is given, fetch the daily problem.",
    )
    .arguments("[url:string]")
    .action(async (_, url) => await fetchProblem(url))
    .command("open", open)
    .parse(Deno.args)
}
