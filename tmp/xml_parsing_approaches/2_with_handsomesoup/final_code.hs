import Text.HandsomeSoup
import Text.XML.HXT.Core
import Control.Monad

main = do
  contents_of_xml_file <- readFile "t1_with_corrections_and_additions.xml"

  let parsed_result = parseHtml contents_of_xml_file

  all_c10_attributes <- runX $
    parsed_result >>> css "c" ! "c10"

  only_deeply_nested_c10_attributes <- runX $
    parsed_result >>> css "z c" ! "c10"

  putStrLn ""
  putStrLn "All c10 attributes:"
  putStrLn ""

  mapM_ putStrLn all_c10_attributes

  putStrLn ""
  putStrLn "Only deeply nested c10 attributes:"
  putStrLn ""

  mapM_ putStrLn only_deeply_nested_c10_attributes

  putStrLn ""

