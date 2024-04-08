module Audio (scanAudio) where

import Data.List (isInfixOf, transpose)
import Data.List.Extra (dropPrefix, split)
import System.Process (readProcess)

extractDeviceOutput :: String -> [String]
extractDeviceOutput text =
  let appNames = extractPactlData text "\t\tapplication.name = "
      appIds = extractPactlData text "\t\tobject.serial = "
      windows = zipWith (++) (zipWith (++) appNames (repText (length appNames) " - ")) appIds
      medias = extractPactlData text "\t\tmedia.name = "
      volumes = extractVolumeInfo text
   in zipText " | " volumes (zipText " | " windows medias)

extractVolumeInfo :: String -> [String]
extractVolumeInfo text =
  let pattern = "Volume: "
      searchLines = filter (isInfixOf pattern) (lines text)
      rawData = filter (/= '"') . dropPrefix pattern <$> searchLines
   in (head . filter (isInfixOf "%")) . words <$> rawData

extractPactlData :: String -> String -> [String]
extractPactlData text pattern =
  let searchLines = filter (isInfixOf pattern) (lines text)
   in filter (/= '"') . dropPrefix pattern <$> searchLines

formatTablePretty :: [[String]] -> [[String]]
formatTablePretty xs = do
  let rowsizesOfb = map (map length) xs
      rowsizes = map maximum (transpose rowsizesOfb)
  map (zipWith equaliseString rowsizes) xs
 where
  equaliseString :: Int -> String -> String
  equaliseString i str =
    if length str >= i
      then str
      else foldl (++) str (" " <$ [1 .. (i - length str)])

scanAudio :: IO [[String]]
scanAudio = do
  audioDeviceData <- readProcess "pactl" ["list", "sink-inputs"] ""
  let audioDeviceList =
        formatTablePretty $
          split (== '|')
            <$> extractDeviceOutput
              audioDeviceData
  return audioDeviceList

-- Helper functions

repText :: Int -> String -> [String]
repText n text = text <$ [1 .. n]

zipText :: String -> [String] -> [String] -> [String]
zipText spacers text1 text2 =
  let part1 = zipWith (++) text1 (repText (length text1) spacers)
   in zipWith (++) part1 text2
