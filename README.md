# Data.DDate

The discordian calendar for Haskell.

It exports the datatypes `DDate` and `DDateTime`, which feel relatively
similar to the ones in [Data.Dates](https://hackage.haskell.org/package/dates-0.2.2.1),
thus can convert regular DateTime instances to DDate and DDateTime instances.
They will pretty print (being an instance of `Show`).

Further exported data types are `Season` and `Day`, which will pretty
print as well, and `Yold`, which is a regular type alias for Ints.

## Usage

Let me give you a quick example:

```haskell
import Data.DDate (currentDDateTime, ddateTimeToDDate)

currentDDateString :: IO String
currentDDateString = do
  ddatetime <-  currentDDateTime
  return $ show $ ddateTimeToDDate ddatetime
-- this will return something along the lines of:
-- Sweetmorn, Confusion the 35th, 3182 YOLD
```

<hr/>

Have fun!
