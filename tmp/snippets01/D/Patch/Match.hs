--  Copyright (C) 2004 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

#include "gadts.h"

module Darcs.Patch.Match ( PatchMatch, Matcher, MatchFun,
                    patchMatch, matchPattern,
                    applyMatcher, makeMatcher,
                    parseMatch,
                    matchParser, helpOnMatchers,
                  ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Regex ( mkRegex, matchRegex )
import Data.Maybe ( isJust )
import System.IO.Unsafe ( unsafePerformIO )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info )
import Darcs.Patch ( Patchy, hunkMatches, listTouchedFiles, patchcontents )
import Darcs.Patch.Info ( justName, justAuthor, justLog, makeFilename,
                          piDate )
import Darcs.Witnesses.Sealed ( Sealed2(..), seal2 )
import DateMatcher ( parseDateMatcher )

import Darcs.Patch.MatchData ( PatchMatch(..), patchMatch )
import qualified Data.ByteString.Char8 as BC

import Darcs.Patch.Dummy ( DummyPatch )

-- | A type for predicates over patches which do not care about
-- contexts
type MatchFun p = Sealed2 (PatchInfoAnd p) -> Bool

-- | A @Matcher@ is made of a 'MatchFun' which we will use to match
-- patches and a @String@ representing it.
data Matcher p = MATCH String (MatchFun p)

instance Show (Matcher p) where
    show (MATCH s _) = '"':s ++ "\""

makeMatcher :: String -> (Sealed2 (PatchInfoAnd p) -> Bool) -> Matcher p
makeMatcher s m = MATCH s m

-- | @applyMatcher@ applies a matcher to a patch.
applyMatcher :: Matcher p -> PatchInfoAnd p C(x y) -> Bool
applyMatcher (MATCH _ m) = m . seal2

parseMatch :: Patchy p => PatchMatch -> Either String (MatchFun p)
parseMatch (PatternMatch s) =
    case parse matchParser "match" s of
    Left err -> Left $ "Invalid -"++"-match pattern '"++s++
                "'.\n"++ unlines (map ("    "++) $ lines $ show err) -- indent
    Right m -> Right m

matchPattern :: Patchy p => PatchMatch -> Matcher p
matchPattern p@(PatternMatch s) =
    case parseMatch p of
    Left err -> error err
    Right m -> makeMatcher s m

trivial :: Patchy p => MatchFun p
trivial = const True

matchParser :: Patchy p => CharParser st (MatchFun p)
matchParser =  do m <- option trivial submatch
                  eof
                  return m

submatch :: Patchy p => CharParser st (MatchFun p)
submatch = buildExpressionParser table match <?> "match rule"

table :: OperatorTable Char st (MatchFun p)
table   = [ [prefix "not" negate_match,
             prefix "!" negate_match ]
          , [binary "||" or_match,
             binary "or" or_match,
             binary "&&" and_match,
            binary "and" and_match ]
          ]
    where binary name fun =
              Infix (do _ <- trystring name
                        spaces
                        return fun) AssocLeft
          prefix  name fun = Prefix $ do _ <- trystring name
                                         spaces
                                         return fun
          negate_match a p = not (a p)
          or_match m1 m2 p = (m1 p) || (m2 p)
          and_match m1 m2 p = (m1 p) && (m2 p)

trystring :: String -> CharParser st String
trystring s = try $ string s

match :: Patchy p => CharParser st (MatchFun p)
match = between spaces spaces
        (parens submatch
         <|> choice matchers_
         <?> "simple match")
        where matchers_ = map createMatchHelper primitiveMatchers


createMatchHelper :: (String, String, [String], String -> MatchFun p)
                  -> CharParser st (MatchFun p)
createMatchHelper (key,_,_,matcher) =
  do _ <- trystring key
     spaces
     q <- quoted
     return $ matcher q

-- FIXME: would this be better defined in Darcs.Commands.Help?
-- | The string that is emitted when the user runs @darcs help --match@.
helpOnMatchers :: String
helpOnMatchers = unlines $
  ["Selecting Patches:",
   "",
   "The --patches option yields patches with names matching an `extended'",
   "regular expression.  See regex(7) for details.  The --matches option",
   "yields patches that match a logical (Boolean) expression: one or more",
   "primitive expressions combined by grouping (parentheses) and the",
   "complement (not), conjunction (and) and disjunction (or) operators.",
   "The C notation for logic operators (!, && and ||) can also be used.",
   "",
   " --patches=regex is a synonym for --matches='name regex'",
   " --from-patch and --to-patch are synonyms for --from-match='name... and --to-match='name...",
   " --from-patch and --to-match can be unproblematically combined:",
   " darcs changes --from-patch='html.*documentation' --to-match='date 20040212'",
   "",
   "The following primitive Boolean expressions are supported:"]
  ++ keywords
  ++ ["", "Here are some examples:"]
  ++ examples
  where -- This type signature exists to appease GHC.
        ps :: [(String, String, [String], String -> MatchFun DummyPatch)]
        ps = primitiveMatchers
        keywords = [showKeyword k d | (k,d,_,_) <- ps]
        examples = [showExample k e | (k,_,es,_) <- ps, e <- es]
        showKeyword keyword description =
            -- FIXME: it would be nice to have a variable name here:
            -- "author REGEX - match against author (email address)"
            -- or "exact STRING - match against exact patch name".
            "  " ++ keyword ++ " - " ++ description ++ "."
        showExample keyword example =
            -- FIXME: this string is long, and its not a use case I've
            -- ever seen in practice.  Can we use something else,
            -- like "darcs changes --matches"? --twb, 2008-12-28
            "  darcs annotate --summary --match "
            ++ "'" ++ keyword ++ " " ++ example ++ "'"

primitiveMatchers :: Patchy p => [(String, String, [String], String -> MatchFun p)]
                     -- ^ keyword (operator), help description, list
                     -- of examples, matcher function
primitiveMatchers =
 [ ("exact", "check a literal string against the patch name"
           , ["\"Resolve issue17: use dynamic memory allocation.\""]
           , exactmatch )
 , ("name", "check a regular expression against the patch name"
          , ["issue17", "\"^[Rr]esolve issue17\\>\""]
          , mymatch )
 , ("author", "check a regular expression against the author name"
            , ["\"David Roundy\"", "droundy", "droundy@darcs.net"]
            , authormatch )
 , ("hunk", "check a regular expression against the contents of a hunk patch"
            , ["\"foo = 2\"", "\"^instance .* Foo where$\""]
            , hunkmatch )
 , ("comment", "check a regular expression against the log message"
         , ["\"prevent deadlocks\""]
         , logmatch )
 , ("hash",  "match the darcs hash for a patch"
          ,  ["20040403105958-53a90-c719567e92c3b0ab9eddd5290b705712b8b918ef"]
          ,  hashmatch )
 , ("date", "match the patch date"
          , ["\"2006-04-02 22:41\"", "\"tea time yesterday\""]
          , datematch )
 , ("touch", "match file paths for a patch"
          , ["src/foo.c", "src/", "\"src/*.(c|h)\""]
          , touchmatch ) ]

parens :: CharParser st (MatchFun p)
       -> CharParser st (MatchFun p)
parens p  = between (string "(") (string ")") p

quoted :: CharParser st String
quoted = between (char '"') (char '"')
                 (many $ do { _ <- char '\\' -- allow escapes
                            ; try (oneOf ['\\', '"']) <|> return '\\'
                            }
                         <|>  noneOf ['"'])
         <|> between spaces spaces (many $ noneOf " ()")
         <?> "string"

mymatch, exactmatch, authormatch, hunkmatch, hashmatch, datematch, touchmatch :: Patchy p => String -> MatchFun p

mymatch r (Sealed2 hp) = isJust $ matchRegex (mkRegex r) $ justName (info hp)

exactmatch r (Sealed2 hp) = r == (justName (info hp))

authormatch a (Sealed2 hp) = isJust $ matchRegex (mkRegex a) $ justAuthor (info hp)

logmatch :: Patchy p => String -> MatchFun p
logmatch l (Sealed2 hp) = isJust $ matchRegex (mkRegex l) $ justLog (info hp)

hunkmatch r (Sealed2 hp) = let patch = patchcontents $ hopefully hp
                               regexMatcher = isJust . (matchRegex (mkRegex r) . BC.unpack)
                           in hunkMatches regexMatcher patch

hashmatch h (Sealed2 hp) = let rh = makeFilename (info hp) in
                                  (rh == h) || (rh == h++".gz")

datematch d (Sealed2 hp) = let dm = unsafePerformIO $ parseDateMatcher d
                                  in dm $ piDate (info hp)

touchmatch r (Sealed2 hp) = let files = listTouchedFiles $ patchcontents $ hopefully hp
                            in or $ map (isJust . matchRegex (mkRegex r)) files
