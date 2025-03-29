
package graphql

import scala.deriving.Mirror
import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

import upickle.default.*

trait GraphQLWriter[A]:
    def render: String

object GraphQLWriter:
    inline def apply[A: GraphQLWriter as writer]: writer.type = writer

    inline def render[A: GraphQLWriter as writer]: String = writer.render

    given [A] => GraphQLWriter[A]:
        override def render: String = ""

    given [C[X] <: Iterable[X], A: GraphQLWriter] => GraphQLWriter[C[A]]:
        override def render: String = GraphQLWriter[A].render

    private inline def summonInstances[Elems <: Tuple]: List[GraphQLWriter[?]] =
        inline erasedValue[Elems] match
            case _: EmptyTuple => Nil
            case _: (elem *: elems) =>
                summonInline[GraphQLWriter[elem]] :: summonInstances[elems]

    private inline def deriveInstances[Elems <: Tuple]: List[GraphQLWriter[?]] =
        inline erasedValue[Elems] match
            case _: EmptyTuple => Nil
            case _: (elem *: elems) =>
                GraphQLWriter.derived(using summonInline[Mirror.Of[elem]]) :: deriveInstances[elems]

    private def derivedProductRender[A](
        label: String,
        fields: List[Any],
        fieldsRenders: List[GraphQLWriter[?]],
    ): GraphQLWriter[A] =
        new GraphQLWriter[A]:
            override def render: String =
                (fields lazyZip fieldsRenders).view
                    .map { (field, fieldRender) => s"${field}${fieldRender.render}" }
                    .mkString(start = s"{", sep = " ", end = "}")

    private def derivedSumRender[A](
        label: String,
        casesRenders: List[GraphQLWriter[?]],
    ): GraphQLWriter[A] =
        new GraphQLWriter[A]:
            override def render: String = casesRenders.view
                .map(_.render)
                .mkString(start = s"( ", sep = " | ", end = " )")

    inline def derived[A: Mirror.Of as mirror]: GraphQLWriter[A] = inline mirror match
        case p: Mirror.ProductOf[A] =>
            derivedProductRender(
              label = constValue[p.MirroredLabel],
              fields = constValueTuple[p.MirroredElemLabels].toList,
              fieldsRenders = summonInstances[p.MirroredElemTypes],
            )

        case s: Mirror.SumOf[A] =>
            derivedSumRender(
              label = constValue[s.MirroredLabel],
              casesRenders = deriveInstances[s.MirroredElemTypes],
            )

final case class User(name: String, age: Int) derives ReadWriter, GraphQLWriter

final case class Data(
    num: Int,
    text: String,
    admin: User,
    users: List[User],
    flags: List[Boolean],
) derives ReadWriter, GraphQLWriter

// enum States derives GraphQLWriter:
//     case Start
//     case Running(paused: Boolean)
//     case Finished(status: Int)

// @main def main(): Unit =
//     println(GraphQLWriter.render[Data])
//     // println(GraphQLWriter.render[States])
