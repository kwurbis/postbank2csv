{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib where

import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (isDigit)
import           Data.Csv                   hiding (Parser)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (Day, defaultTimeLocale,
                                             fromGregorian, toGregorian)
import           Data.Void
import           System.Environment
import           System.FilePath
import           System.Directory
import           System.IO
import           System.Process             (callCommand)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf


--- pdf to txt

pdf2txt :: FilePath -> FilePath -> IO ()
pdf2txt inPdf outTxt = callCommand cmd
  where cmd = "pdftotext -nopgbrk -fixed 8 -enc UTF-8 -eol unix "
              ++ inPdf ++ " " ++ outTxt


-- types

type Parser = Parsec Void Text

data Kontoauszug = Kontoauszug
  { startDate     :: Day
  , endDate       :: Day
  , balanceBefore :: Double
  , balanceAfter  :: Double
  , buchungen     :: [Buchung']
  } deriving (Show, Eq)

data Buchung' = Buchung'
  { dateBuchung'  :: Day
  , dateWert'     :: Day
  , vorgang'      :: Text
  , betrag'       :: Double
  , buchungsInfo' :: Text
  } deriving (Show, Eq)

data Buchung = Buchung
  { dateBuchung  :: ShortDate
  , dateWert     :: ShortDate
  , vorgang      :: Text
  , betrag       :: Double
  , buchungsInfo :: Text
  } deriving (Show, Eq)

type ShortDate = (Int, Int)


-- Parsers

parsePostbankFile ::
  FilePath -> IO (Either (ParseErrorBundle Text Void) Kontoauszug)
parsePostbankFile filename = do
  contents <- T.pack <$> readFile filename
  return $ parse parseKontoauszug filename contents

parseKontoauszug :: Parser Kontoauszug
parseKontoauszug = do
  (startDate, endDate) <- skipManyTill anyChar dateRange
  oldBalance <- skipManyTill anyChar (balance "Alter Kontostand")
  buchungen <- some $ try $ skipManyTill anyChar (try buchung)
  newBalance <- skipManyTill anyChar (balance "Neuer Kontostand")
  let buchungen' = map (convertShortDates startDate endDate) buchungen
  return $ Kontoauszug startDate endDate oldBalance newBalance buchungen'

dateRange :: Parser (Day, Day)
dateRange = do
  string "Kontoauszug: Postbank Giro plus vom"
  sc
  day1 <- longDate
  sc
  string "bis"
  sc
  day2 <- longDate
  return (day1, day2)

balance :: Text -> Parser Double
balance txt = do
  string txt
  skipManyTill anyChar (string "EUR")
  value <- skipManyTill anyChar germanNum
  return value

buchung :: Parser Buchung
buchung = do
  buchungstag <- shortDate
  optional (char '/')
  sc
  dateWert <- shortDate
  sc
  vorgang <- T.pack <$> someTill anyChar (try $ count 2 (char ' '))
  sc
  wert <- germanNum
  sc
  buchungsInfo <- T.unlines <$> some nonemptyLine
  sc
  return $ Buchung buchungstag dateWert vorgang wert buchungsInfo

anyChar :: Parser Char
anyChar = printChar <|> spaceChar

sc :: Parser ()                 -- space consumer
sc = L.space space1 (L.skipLineComment "#") empty

hspc :: Parser ()               -- optional horizontal space
hspc = skipMany (char ' ')

hspc1 :: Parser ()              -- some horizontal space
hspc1 = skipSome (char ' ')

emptyLine :: Parser Text
emptyLine = try $ hspc >> eol

nonemptyLine :: Parser Text
nonemptyLine = do
  hspc
  txt <- some printChar
  eol
  return $ T.pack txt

germanNum :: Parser Double
germanNum = do
  signChar <- char '+' <|> char '-'
  hspc
  beforeComma <- some $ char '.' <|> digitChar
  char ','
  afterComma <- count 2 digitChar
  eol
  let before = filter (/='.') beforeComma
  return $ read ((if signChar == '-' then "-" else "")
                 ++ before ++ "." ++ afterComma)

longDate :: Parser Day
longDate = do
  (day, month) <- shortDate
  year <- count 4 digitChar
  return $ fromGregorian (read year) month day

shortDate :: Parser ShortDate
shortDate = do
  let twoDigits = count 2 digitChar
  day <- twoDigits
  char '.'
  month <- twoDigits
  char '.'
  return (read day, read month)

convertShortDates :: Day -> Day -> Buchung -> Buchung'
convertShortDates start end Buchung{..} = Buchung'
  (shortDate2Day dateWert start end)
  (shortDate2Day dateBuchung start end)
  vorgang betrag buchungsInfo

shortDate2Day :: ShortDate -> Day -> Day -> Day
shortDate2Day (d, m) start end
  | (start <= d1) && (d1 <= end) = d1
  | (start <= d2) && (d2 <= end) = d2
  | otherwise = error "date error"
  where (y1, _, _) = toGregorian start
        (y2, _, _) = toGregorian end
        d1 = fromGregorian y1 m d
        d2 = fromGregorian y2 m d


-- csv

instance ToNamedRecord Buchung' where
  toNamedRecord Buchung'{..} = namedRecord
    [ "Buchungstag" .= dateBuchung'
    , "Wertstellung" .= dateWert'
    , "Vorgang" .= vorgang'
    , "Soll" .= if isNegative then b else BC.empty
    , "Haben" .= if isNegative then BC.empty else b
    , "BuchungsInfo" .= (TE.encodeUtf8 . T.intercalate " " . T.lines $ buchungsInfo')
    ]
    where isNegative = betrag' < 0
          b = BC.pack $ show betrag'

instance DefaultOrdered Buchung' where
  headerOrder b = header
    ["Buchungstag", "Wertstellung", "Vorgang", "Soll", "Haben", "BuchungsInfo"]

instance ToRecord Buchung' where
  toRecord Buchung'{..} = record
    [ toField dateBuchung'
    , toField dateWert'
    , TE.encodeUtf8 vorgang'
    , if isNegative
      then b
      else BC.empty
    , if isNegative
      then BC.empty
      else b
    , TE.encodeUtf8 buchungsInfo'
    ]
    where isNegative = betrag' < 0
          b = BC.pack $ show betrag'

instance ToField Day where
  toField = BC.pack . show

instance FromField ShortDate where
  parseField bs = do
    let [d, m] = init $ BC.split '.' bs
    return (read (BC.unpack d), read (BC.unpack m))

instance ToField ShortDate where
  toField (d, m) = BC.pack $ printf "%d.%d." d m


-- cli

cli :: IO ()
cli = do
  [input] <- getArgs
  isDir <- doesDirectoryExist input
  fs <- if isDir
        then (do listdir <- getDirectoryContents input
                 return [input </> f | f <- listdir, takeExtension f == ".pdf" ])
        else return [input]
  mapM_ (putStrLn . show) fs
  mapM_ postbankPdf2Csv fs

postbankPdf2Csv :: FilePath -> IO ()
postbankPdf2Csv fpdf = do
  let ftxt = fpdf -<.> ".txt"
  pdf2txt fpdf ftxt
  output <- parsePostbankFile ftxt
  removeFile ftxt
  case output of
    Left err          -> error $ errorBundlePretty err
    Right kontoauszug -> do
      let fout = takeDirectory fpdf </> getKontoauszugTimestamp kontoauszug <.> ".csv"
      saveKontoauszugCsv fout kontoauszug
      putStrLn $ "wrote " ++ fout ++ "."

getKontoauszugTimestamp :: Kontoauszug -> String
getKontoauszugTimestamp kontoauszug = shortenDate d1 ++ "_" ++ shortenDate d2
  where (d1, d2) = (startDate kontoauszug, endDate kontoauszug)
        shortenDate = filter (isDigit) . show

saveKontoauszugCsv :: FilePath -> Kontoauszug -> IO ()
saveKontoauszugCsv fout k = do
  let contents = encodeDefaultOrderedByName (buchungen k)
  BL.writeFile fout contents
