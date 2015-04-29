module Test where

import           Data.Fixed                        (E12, Fixed (..))
import           Data.Text
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Data.Time.RFC2822
import           Data.Time.RFC3339

import           Distribution.TestSuite.QuickCheck
import           Test.QuickCheck                   hiding (Fixed)


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


tests :: IO [Test]
tests = return
    [ testProperty "Inverse RFC 3339." prop_inverseRFC3339
    , testProperty "Inverse RFC 2822." prop_inverseRFC2822
    ]

prop_inverseRFC3339 :: ZonedTime -> Bool
prop_inverseRFC3339 zonedTime = (fmap zonedTimeToUTC . parseTimeRFC3339 . asText . formatTimeRFC3339) zonedTime == Just (zonedTimeToUTC zonedTime)

prop_inverseRFC2822 :: ZonedTime -> Bool
prop_inverseRFC2822 zonedTime = (fmap zonedTimeToUTC . parseTimeRFC2822 . asText . formatTimeRFC2822) zonedTime == Just (zonedTimeToUTC zonedTime)

asText :: Text -> Text
asText = id
