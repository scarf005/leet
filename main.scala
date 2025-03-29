package client

import sttp.client4.quick._
import sttp.client4.Response
import upickle.default.*
import graphql.*

import org.scalafmt.interfaces.Scalafmt
import java.nio.file.Paths

case class CodeSnippet(lang: String, langSlug: String, code: String)
    derives GraphQLWriter, ReadWriter
case class Question(
    questionFrontendId: Int,
    title: String,
    titleSlug: String,
    exampleTestcaseList: List[String],
    metaData: String,
    codeSnippets: List[CodeSnippet],
) derives ReadWriter, GraphQLWriter
case class Data(question: Question) derives ReadWriter
case class JsonResponse(data: Data) derives ReadWriter

def query(titleSlug: String) =
    val graphqlQuery =
        s"""
        |query {
        |  question(titleSlug: "$titleSlug")
        |    ${GraphQLWriter.render[Question]}
        |
        |}
        """.stripMargin

    val jsonBody = ujson.Obj("query" -> graphqlQuery)
    println(jsonBody)
    val request = quickRequest
        .get(uri"https://leetcode.com/graphql")
        .header("Content-Type", "application/json")
        .header("Referer", "https://leetcode.com")
        .body(jsonBody.render(indent = 2))

    println(request.toCurl)

    val response = request.send(backend)
    println(response)
    read[JsonResponse](response.body).data.question

val scalafmt = Scalafmt.create(this.getClass.getClassLoader)

@main def main() =
    val question = query("clear-digits")

    val scalaSnippet = question.codeSnippets.find(_.lang == "Scala").get
    val formatted = scalafmt.format(
      Paths.get(".scalafmt.conf"),
      Paths.get("dummy-file.scala"),
      scalaSnippet.code,
    )
    val template = f"""
    |package leet.`${question.questionFrontendId}%04d`
    |
    |import scala.annotation.tailrec
    |
    |$formatted
    """.stripMargin
    println(template)
    println(question.exampleTestcaseList)
    // val data = ujson.read(text)("data")("question")
    // println(data("codeSnippets").render(indent = 2))
    // println(ujson.read(data("metaData").str).render(indent = 2))
