module MCalendar where 

import Picture
import Year

month :: Month -> Year -> Picture
month m y = info2Pic $ monthInfo2 m y

showMonth :: Month -> Year -> String
showMonth m y = showPic $ month m y
