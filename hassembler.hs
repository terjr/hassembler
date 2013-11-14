module Main where

import Data.Char
import Data.Functor.Identity
import Numeric
import System.Environment
import Text.Parsec
import Text.Parsec.String

import qualified Data.Map as Map

startState :: (Integer, Map.Map String Integer)
startState = (0, Map.empty)

type ParserState = (Integer, Map.Map String Integer)

pMain   :: ParsecT String ParserState Identity ([Instruction], ParserState)
pMain = do
  instr <- asmFile
  eof
  st    <- getState
  return (instr, st)

asmFile :: ParsecT String ParserState Identity [Instruction]
asmFile = instr `endBy` eol
instr = regInstr
    <|> ldImmInstr
    <|> ldStInstr
    <|> asmLabel

data Instruction = RegInstr String Operand Operand
                 | LdImmInstr Integer
                 | Label
--                 | LdStInstr String Operand Operand
                 | BranchInstr Integer
  deriving (Eq, Show)

data Operand = Reg Char Integer
             | Imm Integer
  deriving (Eq, Show)

regOrImm :: String -> Operand
regOrImm x  = case x of
                '#':t -> Imm $ read t
                v:vs  -> Reg v (read vs)


opcode = choice . map (try . string) $
         ["addi", "fadd", "add", "sub", "fsub", "cmp", "mul", "fmul", "fmla", "fmls", "shl", "shr",
         "and", "nand", "or", "nor", "xor", "mov", "mvn", "i2f", "f2i"]

ldStOpcode = choice . map (try . string) $
             ["lda", "ldb", "ldc", "stb"]

putLabel :: String -> ParserState -> ParserState
putLabel k (n, m) = (n, Map.insert k (n*16) m)

bumpInstrCount :: ParserState -> ParserState
bumpInstrCount (n, r) = (n+1, r)

asmLabel = do
  labelName <- try (many (noneOf ":"))
  char ':'
  modifyState (putLabel labelName)
  return Label

regInstr = do
  op    <- opcode
  spaces
  reg1  <- count 2 alphaNum
  char ','
  spaces
  reg2  <- count 2 alphaNum
  modifyState bumpInstrCount
  return $ RegInstr op (regOrImm reg1) (regOrImm reg2)

ldImmInstr = do
  try (string "ldi")
  spaces
  char '#'
  imm   <- many digit
  modifyState bumpInstrCount
  return $ LdImmInstr $ read imm

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = between (char '[') (char ']')

ldStInstr = do
  op    <- ldStOpcode
  spaces
  reg1  <- count 2 alphaNum
  char ','
  spaces
  reg2  <- brackets (count 2 alphaNum)
  modifyState bumpInstrCount
  return $ RegInstr op (regOrImm reg1) (regOrImm reg2)


eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"


firstSixBits :: String -> String
firstSixBits "add"  = "000000"
firstSixBits "addi" = "000001"
firstSixBits "fadd" = "000010"
firstSixBits "sub"  = "000100"
firstSixBits "fsub" = "000101"
firstSixBits "cmp"  = "000110"
firstSixBits "mul"  = "001000"
firstSixBits "fmul" = "001001"
firstSixBits "fmla" = "001010"
firstSixBits "fmls" = "001011"
firstSixBits "shl"  = "001100"
firstSixBits "shr"  = "001101"

firstSixBits "and"  = "010000"
firstSixBits "nand" = "010001"
firstSixBits "or"   = "010100"
firstSixBits "nor"  = "010101"
firstSixBits "xor"  = "010110"
firstSixBits "mov"  = "011000"
firstSixBits "mvn"  = "011001"
firstSixBits "i2f"  = "011010"
firstSixBits "f2i"  = "011011"
firstSixBits "lda"  = "011100"
firstSixBits "ldb"  = "011101"
firstSixBits "ldc"  = "011110"
firstSixBits "stb"  = "011111"


bin x = showIntAtBase 2 intToDigit x ""
binary `ofSize` n     = replicate (n - (length binary)) '0' ++ binary
fixedSizeBinary n x   = bin x `ofSize` n


translate :: Instruction -> String
translate (RegInstr op r1@(Reg _ n1) r2) =
  firstSixBits op ++ firstOp ++ secondOp
    where  firstOp   = fixedSizeBinary 5 n1
           secondOp  = case r2 of
                         (Reg b2 n2) -> fixedSizeBinary 5 n2
                         (Imm n2)    -> fixedSizeBinary 5 n2

translate (LdImmInstr imm) =
  "11" ++ fixedSizeBinary 14 imm

translate Label = ""

parseFile p fname = do
  input <- readFile fname
  return $ runParser p startState fname input

main = do
  args    <- getArgs
  results <- parseFile pMain $ head args
  case results of
    Left err -> print err
    Right (instrs, (count, labelMap)) -> do
      putStrLn $ "Instructions assembled: " ++ show count
      putStrLn $ concat $ map translate instrs

