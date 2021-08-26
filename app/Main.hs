{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
module Main where

import Control.Monad
import Control.Monad.Reader
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text.Conversions (ToText(..))
import Data.Text (Text)
import Options.Applicative hiding (columns)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.InterpolatedString.Perl6 (qq,qc)


data VFSItem = Figure VFSFigure | Block BlockNum
              deriving (Eq,Ord,Show)

data BlockNum = Block1
              | Block2
              | Block3
              | Block4
              | Block5
              | Block6
              | Block7
              | Block8
              | Block9
              | Block10
              | Block11
              | Block12
              | Block13
              | Block14
              | Block15
              | Block16
              | Block17
              | Block18
              | Block19
              | Block20
              | Block21
              | Block22
              deriving (Eq,Ord,Bounded,Enum,Show)


data VFSFigure = A | B | C | D | E | F | G | H | J | K | L | M | N | O | P | Q
                 deriving (Eq,Ord,Bounded,Enum,Show)

instance ToText BlockNum where
  toText Block1 = "1"
  toText Block2 = "2"
  toText Block3 = "3"
  toText Block4 = "4"
  toText Block5 = "5"
  toText Block6 = "6"
  toText Block7 = "7"
  toText Block8 = "8"
  toText Block9 = "9"
  toText Block10 = "10"
  toText Block11 = "11"
  toText Block12 = "12"
  toText Block13 = "13"
  toText Block14 = "14"
  toText Block15 = "15"
  toText Block16 = "16"
  toText Block17 = "17"
  toText Block18 = "18"
  toText Block19 = "19"
  toText Block20 = "20"
  toText Block21 = "21"
  toText Block22 = "22"

instance ToText VFSFigure where
  toText A = "A"
  toText B = "B"
  toText C = "C"
  toText D = "D"
  toText E = "E"
  toText F = "F"
  toText G = "G"
  toText H = "H"
  toText J = "J"
  toText K = "K"
  toText L = "L"
  toText M = "M"
  toText N = "N"
  toText O = "O"
  toText P = "P"
  toText Q = "Q"

instance ToText VFSItem where
  toText (Block x ) = toText x
  toText (Figure x) = toText x

instance ToText (VFSItem,VFSItem) where
  toText (a,b) = toText a <> "-" <> toText b

instance ToText [VFSItem] where
  toText s = Text.intercalate "-" (fmap toText s)


vfsItems :: Map Text VFSItem
vfsItems =
  Map.fromList
    [ ("A", Figure A),  ("B", Figure B), ("C", Figure C), ("D",Figure D)
    , ("E", Figure E),  ("F", Figure F), ("G", Figure G), ("H",Figure H), ("J", Figure J)
    , ("K", Figure K),  ("L", Figure L), ("M", Figure M), ("N",Figure N)
    , ("O", Figure O),  ("P", Figure P), ("Q", Figure Q)
    , ("1",  Block Block1),  ("2", Block Block2),  ("3", Block Block3), ("4",  Block Block4)
    , ("5",  Block Block5),  ("6", Block Block6),  ("7", Block Block7), ("8",  Block Block8)
    , ("9",  Block Block9),  ("10",Block Block10), ("11",Block Block11),("12", Block Block12)
    , ("13", Block Block13), ("14",Block Block14), ("15",Block Block15),("16", Block Block16)
    , ("17", Block Block17), ("18",Block Block18), ("19",Block Block19),("20", Block Block20)
    , ("21", Block Block21), ("22",Block Block22)
    ]


-- parseRound :: Text -> [VFSItem]
parseRound s = res
  where
    chunks' = filter (not . Text.null)
              $ Text.split (\x -> not (Char.isLetter x || Char.isDigit x)) s
    chunks = catMaybes $ fmap (flip Map.lookup vfsItems) chunks'
    res = if length chunks /= length chunks' then mempty else chunks

makeInters :: [VFSItem] -> [(VFSItem, VFSItem)]
makeInters [Block a] = [(Block a, Block a)]
makeInters [Figure f] = []
makeInters items = go items
  where
    go (a@(Block{}):b:rs) = (a,a) : (a,b) : go (b:rs)
    go (a@(Figure{}):b:rs) = (a,b) : go (b:rs)
    go (a@(Block{}):[])   = (a,a) : (a, head items) : []
    go (a@(Figure{}):[])  = (a, head items) : []

    go []       = []



newtype RepeatNum = RepeatNum Int
                    deriving stock (Eq,Ord,Show)
                    deriving newtype Read
                    deriving newtype Num

type Opts = ()

optsDef = ()

newtype CommandT m a = CommandT { unCmd :: ReaderT Opts m a }
                       deriving newtype ( Functor
                                        , Applicative
                                        , Monad
                                        , MonadIO
                                        , MonadReader Opts
                                        )

newtype Command = Command { run :: CommandT IO () }

data CLI = CLI Command

pCLI :: Parser CLI
pCLI = CLI <$> pCmd
  where

    pCmd = hsubparser (  command "inters"  (info pGenCmd (progDesc  "generate inters"))
                      )

    pGenCmd = do
      s <- argument str (metavar "ROUND")
      n <- option auto ( short 'n' <> long "number" <> metavar "REPEAT NUM" <> value 1)
      m <- switch ( short 'm' <> long "measures" )
      t <- switch ( short 't' <> long "task" )
      pure $ Command (genInters t m n s)


runCommad :: (Monad m, MonadIO m) => CommandT m a -> m a
runCommad m = do
  runReaderT (unCmd m) optsDef

runCLI :: CLI -> IO ()
runCLI (CLI (Command cmd)) = runCommad cmd


genInters :: Bool -> Bool -> RepeatNum -> Text -> CommandT IO ()
genInters task measures (RepeatNum n) s = do
  let t = parseRound s
  let is = fmap toText $ makeInters t

  when (null t) $ do error "Bad round"

  liftIO $ do
    when task $ do
      Text.putStrLn [qc|task {toText t}|]
      Text.putStrLn ""

  replicateM_ n $ do

    if (task || measures) then do
      liftIO $ do
        mapM_ (\x -> Text.putStrLn [qc|measure {x} 0.00|]) is
        Text.putStrLn ""
    else
      liftIO $ do
        mapM_ Text.putStrLn is
        Text.putStrLn ""



main :: IO ()
main = execParser opts >>= runCLI

 where
  opts = info (pCLI <**> helper)
        (    fullDesc
          <> progDesc "vfs"
        )
