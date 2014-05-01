module Idris.CmdOptions where

import Idris.AbsSyntaxTree

import IRTS.CodegenCommon

import Idris.Core.TT

import Data.Maybe
import Data.Word (Word)

getFile :: Opt -> Maybe String
getFile (Filename str) = Just str
getFile _ = Nothing

getBC :: Opt -> Maybe String
getBC (BCAsm str) = Just str
getBC _ = Nothing

getOutput :: Opt -> Maybe String
getOutput (Output str) = Just str
getOutput _ = Nothing

getIBCSubDir :: Opt -> Maybe String
getIBCSubDir (IBCSubDir str) = Just str
getIBCSubDir _ = Nothing

getImportDir :: Opt -> Maybe String
getImportDir (ImportDir str) = Just str
getImportDir _ = Nothing

getPkgDir :: Opt -> Maybe String
getPkgDir (Pkg str) = Just str
getPkgDir _ = Nothing

getPkg :: Opt -> Maybe (Bool, String)
getPkg (PkgBuild str) = Just (False, str)
getPkg (PkgInstall str) = Just (True, str)
getPkg _ = Nothing

getPkgClean :: Opt -> Maybe String
getPkgClean (PkgClean str) = Just str
getPkgClean _ = Nothing

getPkgREPL :: Opt -> Maybe String
getPkgREPL (PkgREPL str) = Just str
getPkgREPL _ = Nothing

getPkgCheck :: Opt -> Maybe String
getPkgCheck (PkgCheck str) = Just str
getPkgCheck _              = Nothing

-- | Returns None if given an Opt which is not PkgMkDoc
--   Otherwise returns Just x, where x is the contents of PkgMkDoc
getPkgMkDoc :: Opt          -- ^ Opt to extract
            -> Maybe String -- ^ Result
getPkgMkDoc (PkgMkDoc str) = Just str
getPkgMkDoc _              = Nothing

getCodegen :: Opt -> Maybe Codegen
getCodegen (UseCodegen x) = Just x
getCodegen _ = Nothing

getExecScript :: Opt -> Maybe String
getExecScript (InterpretScript expr) = Just expr
getExecScript _ = Nothing

getEvalExpr :: Opt -> Maybe String
getEvalExpr (EvalExpr expr) = Just expr
getEvalExpr _ = Nothing

getOutputTy :: Opt -> Maybe OutputType
getOutputTy (OutputTy t) = Just t
getOutputTy _ = Nothing

getLanguageExt :: Opt -> Maybe LanguageExt
getLanguageExt (Extension e) = Just e
getLanguageExt _ = Nothing

getTriple :: Opt -> Maybe String
getTriple (TargetTriple x) = Just x
getTriple _ = Nothing

getCPU :: Opt -> Maybe String
getCPU (TargetCPU x) = Just x
getCPU _ = Nothing

getOptLevel :: Opt -> Maybe Word
getOptLevel (OptLevel x) = Just x
getOptLevel _ = Nothing

getColour :: Opt -> Maybe Bool
getColour (ColourREPL b) = Just b
getColour _ = Nothing

parseCodegen :: String -> Codegen
parseCodegen "C" = ViaC
parseCodegen "Java" = ViaJava
parseCodegen "bytecode" = Bytecode
parseCodegen "javascript" = ViaJavaScript
parseCodegen "node" = ViaNode
parseCodegen "llvm" = ViaLLVM
parseCodegen _ = error "unknown codegen" -- FIXME: partial function

opt :: (Opt -> Maybe a) -> [Opt] -> [a]
opt = mapMaybe

parseArgs :: [String] -> [Opt]
parseArgs [] = []
parseArgs ("--nobanner":ns)      = NoBanner : (parseArgs ns)
parseArgs ("--quiet":ns)         = Quiet : (parseArgs ns)
parseArgs ("--ideslave":ns)      = Ideslave : (parseArgs ns)
parseArgs ("--client":ns)        = [Client (showSep " " ns)]
parseArgs ("--log":lvl:ns)       = OLogging (read lvl) : (parseArgs ns)
parseArgs ("--nobasepkgs":ns)    = NoBasePkgs : (parseArgs ns)
parseArgs ("--noprelude":ns)     = NoPrelude : (parseArgs ns)
parseArgs ("--nobuiltins":ns)    = NoBuiltins : NoPrelude : (parseArgs ns)
parseArgs ("--check":ns)         = NoREPL : (parseArgs ns)
parseArgs ("-o":n:ns)            = NoREPL : Output n : (parseArgs ns)
parseArgs ("--typecase":ns)      = TypeCase : (parseArgs ns)
parseArgs ("--typeintype":ns)    = TypeInType : (parseArgs ns)
parseArgs ("--total":ns)         = DefaultTotal : (parseArgs ns)
parseArgs ("--partial":ns)       = DefaultPartial : (parseArgs ns)
parseArgs ("--warnpartial":ns)   = WarnPartial : (parseArgs ns)
parseArgs ("--warnreach":ns)     = WarnReach : (parseArgs ns)
parseArgs ("--nocoverage":ns)    = NoCoverage : (parseArgs ns)
parseArgs ("--errorcontext":ns)  = ErrContext : (parseArgs ns)
parseArgs ("--help":ns)          = Usage : (parseArgs ns)
parseArgs ("--link":ns)          = ShowLibs : (parseArgs ns)
parseArgs ("--libdir":ns)        = ShowLibdir : (parseArgs ns)
parseArgs ("--include":ns)       = ShowIncs : (parseArgs ns)
parseArgs ("--version":ns)       = Ver : (parseArgs ns)
parseArgs ("--verbose":ns)       = Verbose : (parseArgs ns)
parseArgs ("--ibcsubdir":n:ns)   = IBCSubDir n : (parseArgs ns)
parseArgs ("-i":n:ns)            = ImportDir n : (parseArgs ns)
parseArgs ("--warn":ns)          = WarnOnly : (parseArgs ns)
-- Package Related options
parseArgs ("--package":n:ns)     = Pkg n : (parseArgs ns)
parseArgs ("-p":n:ns)            = Pkg n : (parseArgs ns)
parseArgs ("--build":n:ns)       = PkgBuild n : (parseArgs ns)
parseArgs ("--install":n:ns)     = PkgInstall n : (parseArgs ns)
parseArgs ("--repl":n:ns)        = PkgREPL n : (parseArgs ns)
parseArgs ("--clean":n:ns)       = PkgClean n : (parseArgs ns)
parseArgs ("--mkdoc":n:ns)       = PkgMkDoc n : (parseArgs ns)        -- IdrisDoc
parseArgs ("--checkpkg":n:ns)    = PkgCheck n : (parseArgs ns)
-- Misc Options
parseArgs ("--bytecode":n:ns)    = NoREPL : BCAsm n : (parseArgs ns)
parseArgs ("-S":ns)              = OutputTy Raw : (parseArgs ns)
parseArgs ("-c":ns)              = OutputTy Object : (parseArgs ns)
parseArgs ("--mvn":ns)           = OutputTy MavenProject : (parseArgs ns)
parseArgs ("--dumpdefuns":n:ns)  = DumpDefun n : (parseArgs ns)
parseArgs ("--dumpcases":n:ns)   = DumpCases n : (parseArgs ns)
parseArgs ("--codegen":n:ns)     = UseCodegen (parseCodegen n) : (parseArgs ns)
parseArgs ("--eval":expr:ns)     = EvalExpr expr : parseArgs ns
parseArgs ("-e":expr:ns)         = EvalExpr expr : parseArgs ns
parseArgs ["--exec"]             = InterpretScript "Main.main" : []
parseArgs ("--exec":expr:ns)     = InterpretScript expr : parseArgs ns
parseArgs (('-':'X':extName):ns) = case maybeRead extName of
  Just ext -> Extension ext : parseArgs ns
  -- Not sure what to do for the Nothing case
  Nothing -> error ("Unknown extension " ++ extName)
  where maybeRead = fmap fst . listToMaybe . reads
parseArgs ("-O3":ns)             = OptLevel 3 : parseArgs ns
parseArgs ("-O2":ns)             = OptLevel 2 : parseArgs ns
parseArgs ("-O1":ns)             = OptLevel 1 : parseArgs ns
parseArgs ("-O0":ns)             = OptLevel 0 : parseArgs ns
parseArgs ("-O":n:ns)            = OptLevel (read n) : parseArgs ns
parseArgs ("--target":n:ns)      = TargetTriple n : parseArgs ns
parseArgs ("--cpu":n:ns)         = TargetCPU n : parseArgs ns
parseArgs ("--colour":ns)        = ColourREPL True : parseArgs ns
parseArgs ("--color":ns)         = ColourREPL True : parseArgs ns
parseArgs ("--nocolour":ns)      = ColourREPL False : parseArgs ns
parseArgs ("--nocolor":ns)       = ColourREPL False : parseArgs ns
parseArgs (n:ns)                 = Filename n : (parseArgs ns)
