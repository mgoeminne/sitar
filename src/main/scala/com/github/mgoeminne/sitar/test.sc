import scala.util.parsing.combinator.JavaTokenParsers

case class Province(name: String = "",
                    continent: Option[String] = None,
                    region: Option[String] = None,
                    area: Option[String] = None,
                    `type`: String = "inland",
                    sea_adjacency: Int,
                    terrain: String = "terra_incognita",
                    size_modifier: Int = 0,  // In %
                    climate: String,
                    ice: Boolean = false,
                    storm: Boolean = false,
                    religion: String,
                    culture: String,
                    manpower: Int,
                    income: Int,
                    goods: Int,
                    value: Int,
                    city_name: String,
                    cot_modifier: Int = 0,
                    colonization_difficulty: Int = 0,
                    // TODO : natives


)
val content =
   """
     |# Afghanistan
     |
     |historicalmonarch = {
     |	id = { type = 6 id = 03630 }
     |	startdate = {
     |		year = 1707
     |	}
     |	deathdate = {
     |		day = 1
     |		month = november
     |		year = 1715
     |	}
     |	name = "Mîr Ways"
     |	DIP = 8
     |	MIL = 8
     |	ADM = 6
     |}
     |historicalmonarch = {
     |	id = { type = 6 id = 03631 }
     |	startdate = {
     |		day = 1
     |		month = november
     |		year = 1715
     |	}
     |	deathdate = {
     |		day = 1
     |		month = march
     |		year = 1716
     |	}
     |	name = "Mîr `Abd al-`Aziz"
     |	DIP = 4
     |	MIL = 5
     |	ADM = 3
     |}
   """.stripMargin

class InputParser extends JavaTokenParsers
{
   def word: Parser[String] = """\w+""".r
   def number: Parser[String] = floatingPointNumber
   def key: Parser[String] = word
   def string: Parser[String] = """"""" ~> """[^"]+""".r <~ """""""

   def value: Parser[Any] =
                            string |
                            number |
                            word |
                            "{" ~> value <~ "}" |
                            expression

   def definition: Parser[(String, Any)] = key ~ "=" ~ value ^^ { case k ~ "=" ~ v => k -> v}

   def expression: Parser[Any] = "{" ~ rep(definition) ~ "}"

   def content: Parser[Seq[(String, Any)]] = rep1(definition)

   def province: Parser[Any] = "province = " ~> expression
}

val cleaned_content = content.replaceAll("""#.*""", "")


val parser = new InputParser
parser.parse(parser.province, """
province = {
   id = 1
   name = "PROV_Yukon"
   continent = "America"
   region = "North America"
   area = "Alaska"
   type = coastal
   sea_adjacency = 828
   terrain = mountain
   size_modifier = 0.00
   climate = arctic
   religion = exotic
   culture = aleutian
   manpower = 1
   income = 1
   goods = gold
   value = 40
   city_name = "CITY_Yukon"
   cot_modifier = -3
   colonization_difficulty = 9
   natives = {
      combat = 15
      ferocity = 2
      efficiency = 1
      tp_negotiation = 7
      tolerance = 3
   }
   gfx = {
      city = { x = 2742 y = 131 }
      army = { x = 2948 y = 164 }
      port = { x = 2757 y = 174 }
      manufactory = { x = 2822 y = 255 }
      terrain1 = { x = 2792 y = 64 variant = 3 }
      terrain2 = { x = 2807 y = 110 variant = 0 }
      terrain3 = { x = 2882 y = 85 variant = 1 }
      terrain4 = { x = 2935 y = 46 variant = 2 }
      river = { 1457 }
   }
   history = { }
}
"""

)


