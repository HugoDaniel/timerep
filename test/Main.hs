import           Data.Fixed            (E12, Fixed (..))
import           Data.Maybe
import           Data.Text
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Data.Time.RFC2822
import           Data.Time.RFC3339
import           Data.Time.RFC822

import           Test.QuickCheck       hiding (Fixed)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck


instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*> pure 0

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary TimeZone where
  arbitrary = TimeZone <$> arbitrary <*> arbitrary <*> elements timeZones
              where timeZones = [ "ACDT", "ACST", "ACT", "ACT", "ADT", "AEDT"
                                , "AEST", "AFT", "AKDT", "AKST", "AMST", "AMST"
                                , "AMT", "AMT", "ART", "AST", "AST", "AWDT", "AWST"
                                , "AZOST", "AZT", "BDT", "BIOT", "BIT", "BOT"
                                , "BRST", "BRT", "BST", "BST", "BTT", "CAT", "CCT"
                                , "CDT", "CDT", "CEDT", "CEST", "CET", "CHADT"
                                , "CHAST", "CHOT", "ChST", "CHUT", "CIST", "CIT"
                                , "CKT", "CLST", "CLT", "COST", "COT", "CST", "CST"
                                , "CST", "CST", "CST", "CT", "CVT", "CWST", "CXT"
                                , "DAVT", "DDUT", "DFT", "EASST", "EAST", "EAT"
                                , "ECT", "ECT", "EDT", "EEDT", "EEST", "EET"
                                , "EGST", "EGT", "EIT", "EST", "EST", "FET", "FJT"
                                , "FKST", "FKST", "FKT", "FNT", "GALT", "GAMT"
                                , "GET", "GFT", "GILT", "GIT", "GMT", "GST", "GST"
                                , "GYT", "HADT", "HAEC", "HAST", "HKT", "HMT"
                                , "HOVT", "HST", "ICT", "IDT", "IOT", "IRDT"
                                , "IRKT", "IRST", "IST", "IST", "IST", "JST", "KGT"
                                , "KOST", "KRAT", "KST", "LHST", "LHST", "LINT"
                                , "MAGT", "MART", "MAWT", "MDT", "MET", "MEST"
                                , "MHT", "MIST", "MIT", "MMT", "MSK", "MST", "MST"
                                , "MST", "MUT", "MVT", "MYT", "NCT", "NDT", "NFT"
                                , "NPT", "NST", "NT", "NUT", "NZDT", "NZST", "OMST"
                                , "ORAT", "PDT", "PET", "PETT", "PGT", "PHOT"
                                , "PKT", "PMDT", "PMST", "PONT", "PST", "PST"
                                , "PYST", "PYT", "RET", "ROTT", "SAKT", "SAMT"
                                , "SAST", "SBT", "SCT", "SGT", "SLST", "SRET"
                                , "SRT", "SST", "SST", "SYOT", "TAHT", "THA", "TFT"
                                , "TJT", "TKT", "TLT", "TMT", "TOT", "TVT", "UCT"
                                , "ULAT", "USZ1", "UTC", "UYST", "UYT", "UZT"
                                , "VET", "VLAT", "VOLT", "VOST", "VUT", "WAKT"
                                , "WAST", "WAT", "WEDT", "WEST", "WET", "WIT"
                                , "WST", "YAKT", "YEKT", "Z" ]

instance Arbitrary ZonedTime where
  arbitrary = ZonedTime <$> arbitrary <*> arbitrary


main :: IO ()
main = defaultMain $ testGroup "Tests" [unitTests, properties]


unitTests :: TestTree
unitTests = testGroup "Unit tests" [casesRFC3339 , casesRFC2822, casesRFC822]

casesRFC3339, casesRFC2822 :: TestTree
casesRFC3339 = testCase "RFC 3339 cases" $ do
  isJust (parseTimeRFC3339 "1985-04-12T23:20:50.52Z") @?= True
  isJust (parseTimeRFC3339 "1996-12-19T16:39:57-08:00") @?= True
  isJust (parseTimeRFC3339 "1990-12-31T23:59:60Z") @?= True
  isJust (parseTimeRFC3339 "1990-12-31T15:59:60-08:00") @?= True
  isJust (parseTimeRFC3339 "1937-01-01T12:00:27.87+00:20") @?= True
casesRFC2822 = testCase "RFC 2822 cases" $ do
  isJust (parseTimeRFC2822 "Fri, 21 Nov 1997 09:55:06 -0600") @?= True
  isJust (parseTimeRFC2822 "Tue, 15 Nov 1994 12:45:26 GMT") @?= True
  isJust (parseTimeRFC2822 "Tue, 1 Jul 2003 10:52:37 +0200") @?= True
  isJust (parseTimeRFC2822 "Thu, 13 Feb 1969 23:32:54 -0330") @?= True
  isJust (parseTimeRFC2822 "Mon, 24 Nov 1997 14:22:01 -0800") @?= True
  isJust (parseTimeRFC2822 "Thu,          13\n     Feb\n  1969\n        23:32\n     -0330") @?= True
  isJust (parseTimeRFC2822 "Thu,          13\n     Feb\n  1969\n        23:32\n     -0330 (Newfoundland Time)") @?= False
  isJust (parseTimeRFC2822 "24 Nov 1997 14:22:01 -0800") @?= True
  isJust (parseTimeRFC2822 "15 Nov 1994 12:45:26 GMT") @?= True
  isJust (parseTimeRFC2822 "Mon,24 Nov 1997 14:22:01 -0800") @?= False
  isJust (parseTimeRFC2822 "Thu,\t13\n     Feb\n  1969\n        23:32\n     -0330 (Newfoundland Time)") @?= False
  isJust (parseTimeRFC2822 "Thu, 13 Feb 1969 23:32 -0330 (Newfoundland Time)") @?= False
casesRFC822 = testCase "RFC 822 cases" $ do
  isJust (parseTimeRFC822 "Wed, 02 Oct 2002 13:00:00 GMT") @?= True
  isJust (parseTimeRFC822 "Wed, 02 Oct 2002 13:00:00 +0100") @?= True
  isJust (parseTimeRFC822 "Wed, 02 Oct 2002 13:00 +0100") @?= True
  isJust (parseTimeRFC822 "02 Oct 2002 13:00 +0100") @?= True
  isJust (parseTimeRFC822 "02 Oct 02 13:00 +0100") @?= True


properties :: TestTree
properties = testGroup "Properties"
  [ inverseRFC3339Property
  , inverseRFC2822Property
  , inverseRFC822Property
  ]

inverseRFC3339Property, inverseRFC2822Property, inverseRFC822Property :: TestTree
inverseRFC3339Property = testProperty "parse . format = id (RFC3339)" $ \zonedTime ->
  (fmap zonedTimeToUTC . parseTimeRFC3339 . asText . formatTimeRFC3339) zonedTime == Just (zonedTimeToUTC zonedTime)
inverseRFC2822Property = testProperty "parse . format = id (RFC2822)" $ \zonedTime ->
  (fmap zonedTimeToUTC . parseTimeRFC2822 . asText . formatTimeRFC2822) zonedTime == Just (zonedTimeToUTC zonedTime)
inverseRFC822Property = testProperty "parse . format = id (RFC822)" $ \zonedTime ->
  (fmap zonedTimeToUTC . parseTimeRFC822 . asText . formatTimeRFC822) zonedTime == Just (zonedTimeToUTC zonedTime)

asText :: Text -> Text
asText = id
