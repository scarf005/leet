import { assertEquals } from "@std/assert"
import { extractProblemName } from "./fetch.ts"

const expected = "sort-colors"
const cases = [
  {
    name: "handles normal URLs",
    url: "https://leetcode.com/problems/sort-colors/description/",
  },
  {
    name: "handles URLs with trailing slashes",
    url: "https://leetcode.com/problems/sort-colors/description//",
  },
  {
    name: "handles URLs with query parameters",
    url:
      "https://leetcode.com/problems/sort-colors/description/?envType=other&envId=2025-05-17",
  },
  {
    name: "handles URLs with hash fragments",
    url: "https://leetcode.com/problems/sort-colors/description/#/other",
  },
  {
    name: "handles URLs with both query parameters and hash fragments",
    url:
      "https://leetcode.com/problems/sort-colors/description/?envType=other&envId=2025-05-17#/other",
  },
  {
    name: "handles daily questions",
    url:
      "https://leetcode.com/problems/sort-colors/description/?envType=daily-question&envId=2025-05-17",
  },
]
cases.forEach(({ name, url }) => {
  Deno.test(`extractProblemName() ${name}`, () => {
    assertEquals(extractProblemName(url), expected)
  })
})
