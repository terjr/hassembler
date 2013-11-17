module Main where

import Control.Applicative ((*>))
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
asmFile = instr `endBy` (optional comment *> eol)
instr = blankLine
    <|> regInstr
    <|> ldImmInstr
    <|> ldStInstr
    <|> branchInstr
    <|> nop
    <|> halt
    <|> asmLabel

comment = do
  skipMany (char ' ')
  char ';'
  skipMany (noneOf "\n")
  return ()

data Instruction = RegInstr String Operand Operand
                 | LdImmInstr Integer
                 | Label
                 | BranchInstr String Integer
                 | Nop
                 | Halt
                 | BlankLine
  deriving (Eq, Show)

data Operand = Reg Char Integer
             | Imm Integer
  deriving (Eq, Show)

regOrImm :: String -> Operand
regOrImm x  = case x of
                '#':t -> Imm $ read t
                v:vs  -> Reg v (read vs)


opcode = choice . map (try . string) $
         ["addi", "fadd", "add", "sub",
         "fsub", "cmp", "mul", "fmul",
         "fmla", "fmls", "shl", "shr",
         "and", "nand", "or", "nor",
         "xor", "mov", "mvn", "i2f", "f2i"]

ldStOpcode    = choice . map (try . string) $
                ["lda", "ldb", "ldc", "stb"]

branchOpcode  = choice . map (try . string) $
                ["beq", "bne", "blt", "bgt", "jmp"]


putLabel :: String -> ParserState -> ParserState
putLabel k (n, m) = (n, Map.insert k n m)

bumpInstrCount :: ParserState -> ParserState
bumpInstrCount (n, r) = (n+1, r)

blankLine = do
  lookAhead (char '\n')
  return BlankLine

halt = do
  try (string "halt")
  modifyState bumpInstrCount
  return Halt

nop = do
  try (string "nop")
  modifyState bumpInstrCount
  return Nop

branchInstr = do
  op      <- try branchOpcode
  skipMany (char ' ')
  char '@'
  label   <- (many (noneOf "\n"))
  modifyState bumpInstrCount
  (_, m)  <- getState
  case (Map.lookup label m) of
    Just x  -> return (BranchInstr op x)
    Nothing -> fail "Invalid branch"

asmLabel = do
  labelName <- try (many (noneOf ":\n"))
  char ':'
  modifyState (putLabel labelName)
  return Label

regInstr = do
  op    <- opcode
  skipMany (char ' ')
  reg1  <- count 2 alphaNum
  char ','
  skipMany (char ' ')
  reg2  <- count 2 alphaNum
  modifyState bumpInstrCount
  return $ RegInstr op (regOrImm reg1) (regOrImm reg2)

ldImmInstr = do
  try (string "ldi")
  skipMany (char ' ')
  char '#'
  imm   <- many digit
  modifyState bumpInstrCount
  return $ LdImmInstr $ read imm

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = between (char '[') (char ']')

ldStInstr = do
  op    <- ldStOpcode
  skipMany (char ' ')
  reg1  <- count 2 alphaNum
  char ','
  skipMany (char ' ')
  reg2  <- brackets (many (noneOf "]"))
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
firstSixBits "shl"  = "001100" -- Not used
firstSixBits "shr"  = "001101" -- Not used

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

loadImmGroup = "10"
branchGroup  = "11"

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

translate (LdImmInstr imm)  =
  loadImmGroup ++ fixedSizeBinary 14 imm

translate Label             = ""
translate BlankLine         = ""

translate (BranchInstr op adr) =
  branchGroup ++ flags ++ jumpAdr
    where flags = case op of
                    "beq" -> "1000"
                    "ble" -> "0001"
                    "jmp" -> "0000"
          jumpAdr = fixedSizeBinary 10 adr

translate Nop = replicate 16 '0'


-- Note: Shifting is redefined to be halt
translate Halt = "0011" ++ fixedSizeBinary 12 0

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

