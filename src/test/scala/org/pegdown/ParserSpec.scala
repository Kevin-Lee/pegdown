package org.pegdown

import org.parboiled.Rule
import org.parboiled.matchers.{AnyOfMatcher, CharMatcher, NothingMatcher}
import org.parboiled.support.Characters
import org.pegdown.plugins.PegDownPlugins
import org.specs2.mutable._

import scala.annotation.tailrec
import scala.util.Random

/**
 * @author Lee, Seong Hyun (Kevin)
 * @since 2015-07-04
 */
class ParserSpec extends Specification with ParserHelper {

  for (options <- allOptions; option2 <- options) {
    val theOptions = (for (option <- option2) yield option.code).foldLeft(0)(_ | _)
    s"ParserSpec(${option2.map(_.name).mkString(" | ")} == $theOptions, 1000L, Parser.DefaultParseRunnerProvider, PegDownPlugins.NONE)" should {
      val parser = new Parser(theOptions, 1000L, Parser.DefaultParseRunnerProvider, PegDownPlugins.NONE)
      val expected = specialCharsBuilder(theOptions, PegDownPlugins.NONE)
      s"  specialChar() === $expected" in {
        val actual = parser.SpecialChar()
        val actualChars = extractSpecialChars(actual)
        actualChars must not be (None)
        actualChars.get === expected
      }
    }
  }

  s"ParserSpec(Extensions.ALL, 1000L, Parser.DefaultParseRunnerProvider, PegDownPlugins.NONE)" should {
    val parser = new Parser(Extensions.ALL, 1000L, Parser.DefaultParseRunnerProvider, PegDownPlugins.NONE)
    val expected = specialCharsBuilder(Extensions.ALL, PegDownPlugins.NONE)
    s"  specialChar() === $expected" in {
      val actual = parser.SpecialChar()
      val actualChars = extractSpecialChars(actual)
      actualChars must not be (None)
      actualChars.get === expected
    }
  }

  s"ParserSpec(${options.map(_.name).mkString(" | ")} == ${options.map(_.code).foldLeft(0)((a, b) => a | b)}, 1000L, Parser.DefaultParseRunnerProvider, PegDownPlugins.NONE)" should {
    val allKnownOptions = options.map(_.code).foldLeft(0)((a, b) => a | b)
    val parser = new Parser(allKnownOptions, 1000L, Parser.DefaultParseRunnerProvider, PegDownPlugins.NONE)
    val expected = specialCharsBuilder(allKnownOptions, PegDownPlugins.NONE)
    s"  specialChar() === $expected" in {
      val actual = parser.SpecialChar()
      val actualChars = extractSpecialChars(actual)
      actualChars must not be (None)
      actualChars.get === expected
    }
  }

  val plugins = PegDownPlugins.builder withSpecialChars('ƒ', 'Ω', '∫', 'ß', 'å') build

  s"ParserSpec(Extensions.NONE, 1000L, Parser.DefaultParseRunnerProvider, PegDownPlugins(specialChars = ${plugins.getSpecialChars.mkString("[", ",", "]")}))" should {
    val parser = new Parser(Extensions.NONE, 1000L, Parser.DefaultParseRunnerProvider, plugins)
    val expected = specialCharsBuilder(Extensions.NONE, plugins)
    s"  specialChar() === $expected" in {
      val actual = parser.SpecialChar()
      val actualChars = extractSpecialChars(actual)
      actualChars must not be (None)
      actualChars.get === expected
    }
  }

  s"ParserSpec(Extensions.ALL, 1000L, Parser.DefaultParseRunnerProvider, PegDownPlugins(specialChars = ${plugins.getSpecialChars.mkString("[", ",", "]")}))" should {
    val parser = new Parser(Extensions.ALL, 1000L, Parser.DefaultParseRunnerProvider, plugins)
    val expected = specialCharsBuilder(Extensions.ALL, plugins)
    s"  specialChar() === $expected" in {
      val actual = parser.SpecialChar()
      val actualChars = extractSpecialChars(actual)
      actualChars must not be (None)
      actualChars.get === expected
    }
  }

  val (extensionOptions, smartOptions, htmlOptions) = (
    Set(
      Extensions.ABBREVIATIONS,
      Extensions.ANCHORLINKS,
      Extensions.AUTOLINKS,
      Extensions.DEFINITIONS,
      Extensions.FENCED_CODE_BLOCKS,
      Extensions.HARDWRAPS,
      Extensions.NONE,
      Extensions.STRIKETHROUGH,
      Extensions.TABLES,
      Extensions.WIKILINKS),
    Set(Extensions.QUOTES,
      Extensions.SMARTS,
      Extensions.SMARTYPANTS),
    Set(
      Extensions.SUPPRESS_ALL_HTML,
      Extensions.SUPPRESS_HTML_BLOCKS,
      Extensions.SUPPRESS_INLINE_HTML)
    )

  "Extensions's options" should {
    val expected = 0
    val extensionOptionPars = collectPairs(extensionOptions)(extensionOptions ++ smartOptions ++ htmlOptions)
    extensionOptionPars.foreach { case (a, b) =>
      s"not have any common bit: ($a, $b) => ($a & $b) === $expected" in {
        val actual = a & b
        actual === expected
      }
    }
  }

  "Extensions's smartOptions" should {
    val expected = 0
    val smartExtensionOptionPairs = collectPairs(smartOptions)(extensionOptions ++ htmlOptions)
    smartExtensionOptionPairs.foreach { case (a, b) =>
      s"not have any common bit: ($a, $b) => ($a & $b) === $expected" in {
        val actual = a & b
        actual === expected
      }
    }
  }

  "Extensions's htmlOptions" should {
    val expected = 0
    val htmlExtensionOptionPairs = collectPairs(htmlOptions)(extensionOptions ++ smartOptions)
    htmlExtensionOptionPairs.foreach { case (a, b) =>
      s"not have any common bit: ($a, $b) => ($a & $b) === $expected" in {
        val actual = a & b
        actual === expected
      }
    }
  }

}

trait ParserHelper {
  def extractSpecialChars(rule: Rule): Option[Characters] = rule match {
    case cm: CharMatcher =>
      Option(Characters.of(cm.character))
    case _: NothingMatcher =>
      None
    case am: AnyOfMatcher =>
      Option(am.characters)
  }

  def specialCharsBuilder(options: Int, plugins: PegDownPlugins): Characters = {

    def ext(extension: Int): Boolean = (options & extension) > 0

    val chars = new StringBuilder("*_`&[]<>!#\\")

    import Extensions._

    if (ext(QUOTES)) chars ++= "'\""
    if (ext(SMARTS)) chars ++= ".-"
    if (ext(AUTOLINKS)) chars ++= "(){}"
    if (ext(DEFINITIONS)) chars ++= ":"
    if (ext(TABLES)) chars ++= "|"
    if (ext(DEFINITIONS) | ext(FENCED_CODE_BLOCKS)) chars ++= "~"

    for (ch <- plugins.getSpecialChars) {
      if (!chars.contains(ch.toString)) {
        chars += ch
      }
    }
    Characters.of(chars.toString)
  }

  protected case class Extension(name: String, code: Int)

  /* @formatter:off */
  protected val options = Set(
    Extension(       "ABBREVIATIONS", Extensions.ABBREVIATIONS),
    Extension(         "ANCHORLINKS", Extensions.ANCHORLINKS),
    Extension(           "AUTOLINKS", Extensions.AUTOLINKS),
    Extension(         "DEFINITIONS", Extensions.DEFINITIONS),
    Extension(  "FENCED_CODE_BLOCKS", Extensions.FENCED_CODE_BLOCKS),
    Extension(           "HARDWRAPS", Extensions.HARDWRAPS),
    Extension(                "NONE", Extensions.NONE),
    Extension(              "QUOTES", Extensions.QUOTES),
    Extension(              "SMARTS", Extensions.SMARTS),
    Extension(         "SMARTYPANTS", Extensions.SMARTYPANTS),
    Extension(       "STRIKETHROUGH", Extensions.STRIKETHROUGH),
    Extension(   "SUPPRESS_ALL_HTML", Extensions.SUPPRESS_ALL_HTML),
    Extension("SUPPRESS_HTML_BLOCKS", Extensions.SUPPRESS_HTML_BLOCKS),
    Extension("SUPPRESS_INLINE_HTML", Extensions.SUPPRESS_INLINE_HTML),
    Extension(              "TABLES", Extensions.TABLES),
    Extension(           "WIKILINKS", Extensions.WIKILINKS)
  )
  /* @formatter:on */

  /* This has to be split into smaller sets. Otherwise it takes too long to test due to too many possible option combinations. */
  protected val groupedOptions = Random.shuffle(options).grouped(5).toList

  /**
   * helper method to create list of option sets.
   *
   * {{{
   * // examples,
   *
   *    input: Set("a")
   *   output: List(Set(a))
   *
   *    input: Set("a", "b")
   *   output: List(Set(a), Set(b), Set(a, b))
   *
   *    input: Set("a", "b", "c")
   *   output: List(Set(b), Set(c), Set(a), Set(a, b), Set(b, c), Set(a, c), Set(a, b, c))
   * }}}
   *
   * @param options the given options
   * @tparam T the element type
   * @return List of option Set containing all possible combinations of the given options.
   */
  def formOptions[T](options: Set[T]): List[Set[T]] = {

    @tailrec
    def eachNumberOfOptions(options: Set[T], howMany: Int, acc: Set[Set[T]]): Set[Set[T]] = howMany match {
      case 0 =>
        acc
      case _ =>
        @tailrec
        def collectOptions(options: Set[T], howMany: Int, acc: Set[Set[T]]): Set[Set[T]] = howMany match {
          case 0 =>
            acc
          case _ =>
            collectOptions(options, howMany - 1, acc.flatMap(found => (options diff found).map(found + _)))
        }
        eachNumberOfOptions(options, howMany - 1, collectOptions(options, howMany, Set(Set())) ++ acc)
    }
    eachNumberOfOptions(options, options.size, Set()).toList.sortBy(_.size)
  }

  protected def collectPairs[T](base: Set[T])(options: Set[T]): Set[(T, T)] = base.flatMap(option => (options - option).map((option, _)))

  protected val allOptions = for (each <- groupedOptions) yield formOptions(each)

}
