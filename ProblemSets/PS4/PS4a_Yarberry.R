system("wget -o nfl.json  'http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json'")
system("cat nfl.json")

install.packages('jsonlite')
library('jsonlite')

nfldf <- fromJSON("nfl.json")

class(nfldf)
class(nfldf$players)

object(nfldf)
object(nfldf$players)

head(nfldf$players)