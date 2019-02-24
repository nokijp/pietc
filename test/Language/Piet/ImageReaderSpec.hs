module Language.Piet.ImageReaderSpec
  ( main
  , spec
  ) where

import Control.Monad
import Control.Monad.Except
import Data.Vector (Vector)
import Language.Piet.Codel
import Language.Piet.ImageReader
import Test.Hspec
import TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  return ()
  describe "readCodels" $ do
    forM_
      [ ( ImageConfig { additionalColor = AdditionalColorAsBlack
                      , multicoloredCodel = MulticoloredCodelAsWhite
                      , codelSize = 5
                      }
        , blackWhiteCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelAsBlack
                      , codelSize = 5
                      }
        , whiteBlackCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelCenter
                      , codelSize = 5
                      }
        , whiteCenterCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelModal
                      , codelSize = 5
                      }
        , whiteModalCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelAverage
                      , codelSize = 5
                      }
        , whiteAverageCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorNearest
                      , multicoloredCodel = MulticoloredCodelAsWhite
                      , codelSize = 5
                      }
        , nearestWhiteCodels
        )
      ] $ \(config, expectedCodels) ->
        context ("when configured with " ++ show config) $ do
          Right codels <- runIO $ runExceptT $ readCodels config "test/resources/imagereader-test.png"
          it "returns codels" $ codels `shouldBe` expectedCodels

    context "when given an invalid codel size" $ do
      let config = defaultImageConfig { codelSize = 4 }
      Left err <- runIO $ runExceptT $ readCodels config "test/resources/imagereader-test.png"
      it "fails with CodelSizeError" $ err `shouldBe` CodelSizeError

blackWhiteCodels :: Vector (Vector Codel)
blackWhiteCodels = toVector2D [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
                              , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
                              , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
                              , [BlackCodel, BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                              , [BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel]
                              , [BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel]
                              , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                              ]

whiteBlackCodels :: Vector (Vector Codel)
whiteBlackCodels = toVector2D [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
                              , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
                              , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
                              , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                              , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                              , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                              , [BlackCodel, BlackCodel, BlackCodel, BlackCodel, WhiteCodel, WhiteCodel]
                              ]

whiteCenterCodels :: Vector (Vector Codel)
whiteCenterCodels = toVector2D [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
                               , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
                               , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
                               , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                               , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                               , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                               , [AchromaticCodel Green Normal, AchromaticCodel Blue Normal, WhiteCodel, AchromaticCodel Red Normal, WhiteCodel, WhiteCodel]
                               ]

whiteModalCodels :: Vector (Vector Codel)
whiteModalCodels = toVector2D [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
                              , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
                              , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
                              , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                              , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                              , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                              , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, WhiteCodel, AchromaticCodel Red Normal, WhiteCodel, WhiteCodel]
                              ]

whiteAverageCodels :: Vector (Vector Codel)
whiteAverageCodels = toVector2D [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
                                , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
                                , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
                                , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                                , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                                , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                                , [WhiteCodel, WhiteCodel, AchromaticCodel Magenta Dark, WhiteCodel, WhiteCodel, WhiteCodel]
                                ]

nearestWhiteCodels :: Vector (Vector Codel)
nearestWhiteCodels = toVector2D [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
                                , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
                                , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
                                , [BlackCodel, AchromaticCodel Red Light, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                                , [AchromaticCodel Red Normal, AchromaticCodel Red Dark, AchromaticCodel Green Dark, BlackCodel, WhiteCodel, AchromaticCodel Blue Light]
                                , [AchromaticCodel Yellow Normal, AchromaticCodel Cyan Normal, AchromaticCodel Magenta Normal, AchromaticCodel Magenta Normal, AchromaticCodel Yellow Normal, AchromaticCodel Cyan Normal]
                                , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
                                ]
