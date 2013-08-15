import Data.List
import Text.HTML.TagSoup

is_tag_text :: Tag t -> Bool
is_tag_text (TagText _) = True
is_tag_text _           = False

extract_meaningful_text_from_one_tag :: Tag String -> String
extract_meaningful_text_from_one_tag (TagText str) = str
extract_meaningful_text_from_one_tag _             = ""

bulk_extract_meaningful_text :: String -> [String]
bulk_extract_meaningful_text xml =
  filter (not . isPrefixOf "\n") $
  map extract_meaningful_text_from_one_tag $
  filter is_tag_text $
  parseTags xml

main :: IO ()
main = do
  contents_of_xml_file <- readFile "t1.xml"

  mapM_ putStrLn $ bulk_extract_meaningful_text contents_of_xml_file

