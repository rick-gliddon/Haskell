{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Richard Gliddon
-- Stability   :  Reasonable
-- Portability :  Probably Little
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Data.Time
import Data.Time.Calendar.WeekDate
import System.IO
import System.Directory
import System.Environment
import Data.Char
import Data.List
import Data.List.Split

-- Constants
--notesDir = "H:/Notes/"
--notesDir = "F:/Haskell/"
username = "Gliddon, Richard"
team = "Ajilon Online Services"
notesDir = ""
destDir = ""
destPrefix = "StatusReport_"
weekDays = ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]
months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
reportDayNum = dayIndex + 1
    where (Just dayIndex) = elemIndex "Wed" weekDays
dayFormatError = "Date parameter must have form dd-mm-yyyy"

daysToReportDay :: Day -> Int
daysToReportDay x
    | weekDayNum < reportDayNum = lastReportDayNum - weekDayNum
    | otherwise = reportDayNum - weekDayNum
    where (_,_,weekDayNum) = toWeekDate x
          lastReportDayNum = reportDayNum - 7

reportDayOfWeek :: Day -> Day
reportDayOfWeek x = addDays (toInteger $ daysToReportDay x) x

padShow :: Int -> String
padShow x
    | strLen == 0 = "00"
    | strLen == 1 = '0':str
    | otherwise = str
    where str = show x
          strLen = length str

notesFileOfMonth :: String -> Day -> FilePath
notesFileOfMonth path x = path ++ (show yyyy) ++ (padShow mm) ++ ".txt"
    where (yyyy,mm,_) = toGregorian x
--    where (yyyy,mm,_) = toGregorian $ reportDayOfWeek x

isDay :: String -> Bool
isDay [] = False
isDay d = all isDigit d && (inDayRange $ toInt d)
    where toInt x = read d :: Int
          inDayRange x = x > 0 && x <= 31

isMonth :: String -> Bool
isMonth [] = False
isMonth x = x `elem` months

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

isDateFormat :: String -> Bool
isDateFormat s = startsWithDay s && endsWithMonth s
    where startsWithDay = isDay . takeWhile (/=' ')
          endsWithMonth = isMonth . tail' . dropWhile (/=' ')

dropToDayEntry :: [String] -> [String]
dropToDayEntry = dropWhile (not . isDateFormat)

calcDateRanges :: (Day, Day) -> [(Day, Day)]
calcDateRanges (start, end)
    | start > end = error "Start day cannot be greater than end day"
    | sM /= eM = (start, monthEnd $ toGregorian start):(calcDateRanges (monthStart $ toGregorian $ addGregorianMonthsClip 1 start, end))
    | otherwise = [(start, end)]
    where (_, sM, _) = toGregorian start
          (_, eM, _) = toGregorian end
          monthStart (y, m, _) = fromGregorian y m 1
          monthEnd (y, m, _) = fromGregorian y m 31

-- We're assuming here that the String is in valid date format
dateStrToDay :: String -> Integer -> Day
dateStrToDay dateStr year = fromGregorian year (monIndex + 1) day
    where day = read $ takeWhile (/=' ') dateStr
          monStr = tail' $ dropWhile (/=' ') dateStr
          (Just monIndex) = elemIndex monStr months

-- We're assuming here that the String is in valid date format
-- and the first Day is before or equal to the second Day
-- Plus this will only be used for days within a single year.  Because it would break otherwise.
isDayInRange :: String -> Day -> Day -> Bool
isDayInRange dateStr s e = s <= d && d <= e
    where d = dateStrToDay dateStr y
          (y,_,_) = toGregorian s


-- A timesheet line is a floating point number, followed directly by "hr", followed by two or more words
-- Param 1: A string that is or is not in timesheet line format
isTimeSheetLine :: String -> Bool
isTimeSheetLine [] = False
isTimeSheetLine l = length allWords >= 3 && wHasHr && wHasFloat
    where allWords@(w:ws) = words l
          wHasHr = "hr" `isSuffixOf` w
          wHasFloat = length w > 2 && all hasFloatChars w'sNum && head w'sNum /= '.' && last w'sNum /= '.'
          hasFloatChars x = isDigit x || x == '.'
          w'sNum = take (length w - 2) w

-- Param 1: List of String starting with a double-quote enclosed project name followed by a description
makeMultiWordProj :: [String] -> (String,String)
makeMultiWordProj [] = ("","")
makeMultiWordProj ws = (proj,dropWhile (==' ') desc')
    where chars@(c:cs) = unwords ws
          spanning = if c == '"' then cs else chars
          (proj,desc) = span (/='"') spanning
          desc' = dropWhile (=='"') desc

-- Returns the type of task and the task code for the given task
-- Param 1: The name of the project or task
taskDescriptors :: String -> (String, String)
taskDescriptors proj
    | length proj < 10 = ("Project","Some_Code")
    | code == "INC" = ("Incident","INC_Code")
    | code == "REQ" = ("Request","REQ_Code")
    | code == "CHG" = ("Change","CHG_Code")
    | code == "PRB" = ("Problem","PRB_Code")
    | otherwise = ("Project","Some_Code")
    where code = map toUpper $ take 3 proj

-- In the format yyyy-mm-dd,"Gliddon, Richard",Project,"Description",Hours
formatTimeSheetLine :: String -> Day -> String
formatTimeSheetLine [] _ = []
formatTimeSheetLine l d
    | isTimeSheetLine l = formatted
    | otherwise = []
    where formatted = show d ++ ",\"" ++ username ++ "\"," ++ team ++ "," ++ proj ++ "," ++ taskType ++ "," ++ taskCode ++ ",\"" ++ desc ++ "\"," ++ hours
          proj = if p == '"' then multiWordProj else proj'
          desc = if p == '"' then desc' else unwords ws
          hours = take (length numhr - 2) numhr
          (numhr:proj'@(p:_):ws) = words l
          (multiWordProj,desc') = makeMultiWordProj (proj':ws)
          (taskType,taskCode) = taskDescriptors proj

-- Converts timesheet lines into csv format status report entries
-- Param 1: Notes contents;  Param 2: log entry date;  Param 3: range start date;  Param 4: range end date
parseTimeSheetLines :: [String] -> Day -> Day -> Day -> [String]
parseTimeSheetLines [] _ _ _ = []
parseTimeSheetLines (l:ls) d s e
    | l == "}" = dropToDayInRange_parse ls s e
    | null formattedLine = parseTimeSheetLines ls d s e
    | otherwise = formattedLine:parseTimeSheetLines ls d s e
    where formattedLine = formatTimeSheetLine l d

-- If line is an open brace then start parsing timesheet lines
-- Else, return to finding a log entry date
-- Param 1: Notes contents;  Param 2: log entry date;  Param 3: range start date;  Param 4: range end date
skipOpenBrace_parse :: [String] -> Day -> Day -> Day -> [String]
skipOpenBrace_parse [] _ _ _ = []
skipOpenBrace_parse lls@(l:ls) d s e
    | l == "{" = parseTimeSheetLines ls d s e
    | otherwise = dropToDayInRange_parse lls s e

-- Drops lines until we reach the timesheet entry and continues parsing
-- Param 1: Notes contents;  Param 2: log entry date;  Param 3: range start date;  Param 4: range end date
dropToTimeSheetLines_parse :: [String] -> Day -> Day -> Day -> [String]
dropToTimeSheetLines_parse [] _ _ _ = []
dropToTimeSheetLines_parse lls@(l:ls) d s e
    | l == "== TIMESHEET" = skipOpenBrace_parse ls d s e
    | isDateFormat l = dropToDayInRange_parse lls s e
    | otherwise = dropToTimeSheetLines_parse ls d s e

-- Skips the underline of the log entry date and continues parsing
-- Param 1: Notes contents;  Param 2: log entry date;  Param 3: range start date;  Param 4: range end date
skipDashes_parse :: [String] -> Day -> Day -> Day -> [String]
skipDashes_parse [] _ _ _ = []
skipDashes_parse lls@(l:ls) d s e
    | all (=='-') l = dropToTimeSheetLines_parse ls d s e
    | otherwise = dropToDayInRange_parse lls s e

-- Assuming that the first Day is before or equal to the second Day
-- Param 1: Notes contents;  Param 3: range start date;  Param 4: range end date
dropToDayInRange_parse :: [String] -> Day -> Day -> [String]
dropToDayInRange_parse [] _ _ = []
dropToDayInRange_parse ls s e
    | null newLs = []
    | isDayInRange l s e = skipDashes_parse ls' (dateStrToDay l y) s e
    | otherwise = dropToDayInRange_parse ls' s e
    where newLs = dropToDayEntry ls
          (l:ls') = newLs
          (y,_,_) = toGregorian s

-- Creates a status report using the given base pass file and a list of date ranges for each month
-- Param 1: path to the directory of notes, "" for current dir;
-- Param 2: A list of pairs representing the start and end date ranges to process for each month
createReportFromPath :: String -> [(Day, Day)] -> IO [String]
createReportFromPath _ [] = return []
createReportFromPath path ((s,e):ds) = do
    let filename = notesFileOfMonth path s
    doesExist <- doesFileExist filename
    if doesExist
        then do
            notes <- readFile filename
            restOfReport <- createReportFromPath path ds
            return $ (dropToDayInRange_parse (lines notes) s e) ++ restOfReport
        else return []

-- Converts the given string to a day and returns it.  If String is not in the expected format
-- (dd-mm-yyyy) then the second param is returned.
getDay :: String -> Day -> Day
getDay [] d = d
getDay dayStr d
    | length dayStr /= 10 = error dayFormatError
    | length ddmmyyyy /= 3 = error dayFormatError
    | length dd /= 2 = error dayFormatError
    | length mm /= 2 = error dayFormatError
    | length yyyy /= 4 = error dayFormatError
    | not (all isDigit dd && all isDigit mm && all isDigit yyyy) = error dayFormatError
    | otherwise = fromGregorian (read yyyy :: Integer) (read mm :: Int) (read dd :: Int)
    where ddmmyyyy = splitOn "-" dayStr
          (dd:mm:yyyy:[]) = ddmmyyyy

-- The main program.  Calculates the report's date range launches the data extraction and prints the results
createStatusReport :: IO ()
createStatusReport = do
    ct <- getCurrentTime
    args <- getArgs
    let today = utctDay ct
        day = if null args then today else getDay (head args) today
        endDay = reportDayOfWeek day
        startDay = addDays (-6) endDay
        dateRanges = calcDateRanges (startDay, endDay)
    reportLines <- createReportFromPath notesDir dateRanges
    let reportStr = unlines reportLines
        destFilename = destDir ++ destPrefix ++ show endDay ++ ".csv"
    putStrLn reportStr
    writeFile destFilename reportStr
    putStrLn $ "\nWrote File, " ++ destFilename
    _ <- getLine
    return ()

exeMain = createStatusReport


-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION


--------------------
--------------------
-- TEST FUNCTIONS --
--------------------
--------------------

printResults f xs = print $ zip xs results
    where results = map f xs

assertIt :: (Eq a) => String -> a -> a -> IO ()
assertIt msg x y = if x == y then return () else putStrLn msg

assertEqual :: (Eq a, Show a) => [a] -> [a] -> String
assertEqual [] [] = "Success!"
assertEqual [] _ = "More actuals than expecteds"
assertEqual _ [] = "Less actuals than expecteds"
assertEqual (e:expecteds) (a:actuals)
    | e /= a = "Expected " ++ (show e) ++ ", Got " ++ (show a)
    | otherwise = assertEqual expecteds actuals

testReportDayOfWeek = do
    c <-getCurrentTime
    let today = utctDay c
        weekbefore = addDays (-7) today
        daybefore = addDays (-2) today
        yesterday = addDays (-1) today
        tomorrow = addDays 1 today
        nextday = addDays 2 today
        nextweek = addDays 7 today
    printResults (toGregorian . reportDayOfWeek) [weekbefore,daybefore,today,yesterday,tomorrow,nextday,nextweek]

--testNotesFileOfWeek = do
--    c <-getCurrentTime
--    let today = utctDay c
--        lastMonth = addGregorianMonthsClip (-1) today
--        nextMonth = addGregorianMonthsClip 1 today
--        nextYear = addGregorianMonthsClip 12 today
--    printResults notesFileOfWeek [today,lastMonth,nextMonth,nextYear]

testIsDay = do
    print $ assertEqual [False,True,False,True,False,True,False,False,False,False] $ map isDay ["0","1","a3","23","//","31","32","-1",""," "]

testIsMonth = do
    printResults isMonth ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Ban","Blah",""," "]

testIsDateFormat = do
    print $ assertEqual [True,False,False,False,False,False,False,True,False,False,False] $ map isDateFormat ["1 Apr","x Jun","7 Blah","Jan"," Feb","23","24 ","31 Dec","32 Nov","12 May Yo","4  Jul"]

testCalcDateRanges = do
    printResults calcDateRanges [(fromGregorian 2014 3 15, fromGregorian 2014 4 15),(fromGregorian 2014 6 2, fromGregorian 2014 6 3),(fromGregorian 2014 2 20, fromGregorian 2014 3 1),(fromGregorian 2014 1 8, fromGregorian 2014 5 30),(fromGregorian 2014 12 25, fromGregorian 2015 1 31)]

--testDropToDayEntry = do
--    c <- getCurrentTime
--    let today = utctDay c
--        reportDay = reportDayOfWeek today
--        startDay = addDays (-6) reportDay
--        dateRanges = calcDateRanges (startDay, reportDay)
--    timesheet <- createTimeSheet dateRanges
--    print timesheet

testIsDayInRange = do
    putStrLn "testIsDayInRange"
    if (isDayInRange "7 Mar" (fromGregorian 2014 3 7) (fromGregorian 2014 3 9)) == True then return () else putStrLn "1 Fail"
    if (isDayInRange "8 Mar" (fromGregorian 2014 3 7) (fromGregorian 2014 3 9)) == True then return () else putStrLn "2 Fail"
    if (isDayInRange "9 Mar" (fromGregorian 2014 3 7) (fromGregorian 2014 3 9)) == True then return () else putStrLn "3 Fail"
    if (isDayInRange "6 Mar" (fromGregorian 2014 3 7) (fromGregorian 2014 3 9)) == False then return () else putStrLn "4 Fail"
    if (isDayInRange "10 Mar" (fromGregorian 2014 3 7) (fromGregorian 2014 3 9)) == False then return () else putStrLn "5 Fail"

testIsTimeSheetLine = do
    putStrLn "testIsTimeSheetLine"
    if isTimeSheetLine "3hr x y" == True then return () else putStrLn "1 Fail"
    if isTimeSheetLine "hr x y" == False then return () else putStrLn "2 Fail"
    if isTimeSheetLine "3 x y" == False then return () else putStrLn "3 Fail"
    if isTimeSheetLine "3h x y" == False then return () else putStrLn "4 Fail"
    if isTimeSheetLine "3.5hr x y" == True then return () else putStrLn "5 Fail"
    if isTimeSheetLine ".5hr x y" == False then return () else putStrLn "6 Fail"
    if isTimeSheetLine "3.hr x y" == False then return () else putStrLn "7 Fail"
    if isTimeSheetLine "10hr x" == False then return () else putStrLn "8 Fail"
    if isTimeSheetLine "10hr" == False then return () else putStrLn "9 Fail"
    if isTimeSheetLine "10hr x y z" == True then return () else putStrLn "10 Fail"
    if isTimeSheetLine "10hr " == False then return () else putStrLn "11 Fail"

testMakeMultiWordProj = do
    putStrLn "testMakeMulitWordProj"
    if makeMultiWordProj ["\"Project","Name","Guy\"","Here's","the","description"] == ("Project Name Guy","Here's the description") then return () else putStrLn "1 Fail"
    if makeMultiWordProj ["\"Project","Name","Guy","Here's","the","description"]  == ("Project Name Guy Here's the description","") then return () else putStrLn "2 Fail"
    if makeMultiWordProj ["Project","Name","Guy\"","Here's","the","description"]  == ("Project Name Guy","Here's the description") then return () else putStrLn "3 Fail"
    if makeMultiWordProj ["Project","Name","Guy","Here's","the","description"]  == ("Project Name Guy Here's the description","") then return () else putStrLn "4 Fail"
    if makeMultiWordProj ["\"Project\""] == ("Project","") then return () else putStrLn "5 Fail"
    if makeMultiWordProj ["\"Project"] == ("Project","") then return () else putStrLn "6 Fail"
    if makeMultiWordProj ["Project\""] == ("Project","") then return () else putStrLn "7 Fail"
    if makeMultiWordProj ["Project"] == ("Project","") then return () else putStrLn "8 Fail"
    if makeMultiWordProj ["\"Project\"","Description"] == ("Project","Description") then return () else putStrLn "9 Fail"

testFormatTimeSheetLine = do
     putStrLn "testFormatTimeSheetLine"
     assertIt "Test 1" (formatTimeSheetLine "1hr CHG0040340 Change this and that buddy" $ fromGregorian 2014 4 13) "2014-04-13, Richard Gliddon, CHG0040340, Change this and that buddy, 1"
     assertIt "Test 2" (formatTimeSheetLine "hr CHG0040340 Change this and that buddy" $ fromGregorian 2014 4 13) ""
     assertIt "Test 3" (formatTimeSheetLine "1hr CHG0040340" $ fromGregorian 2014 4 13) ""
     assertIt "Test 4" (formatTimeSheetLine "1hr \"EBIS Project\" Change this and that buddy" $ fromGregorian 2014 4 13) "2014-04-13, Richard Gliddon, EBIS Project, Change this and that buddy, 1"
     assertIt "Test 5" (formatTimeSheetLine "1hr \"EBIS Project Change this and that buddy" $ fromGregorian 2014 4 13) "2014-04-13, Richard Gliddon, EBIS Project Change this and that buddy, , 1"
     assertIt "Test 6" (formatTimeSheetLine "1.5hr \"EBIS Project Change this and that\" buddy" $ fromGregorian 2014 4 13) "2014-04-13, Richard Gliddon, EBIS Project Change this and that, buddy, 1.5"
     assertIt "Test 7" (formatTimeSheetLine "" $ fromGregorian 2014 4 13) ""

testGetDay = do
    ct <- getCurrentTime
    let today = utctDay ct
    putStrLn "testGetDay"
    assertIt "Test 1" (getDay "" today) today
    assertIt "Test 2" (getDay "24-03-2014" today) $ fromGregorian 2014 3 24

testDropToDayInRange_parse = do
    print $ dropToDayInRange_parse
        ["Text"
        ,""
        ,"5 Jan"
        ,"-----"
        ,"== TIMESHEET"
        ,"{"
        ,""
        ,"1hr CHG0040444 Something needed to be fixed"
        ,""
        ,"}"
        ,""
        ,"9 Jan"
        ,"-----"
        ,"== TIMESHEET"
        ,"{"
        ,""
        ,"1.5hr CHG0040567 Some change to do stuff yo"
        ,"2hr PRB0080567 Some big ass problem"
        ,"A Comment"
        ,"2hr \"EBIS Project yo\" Some project work"
        ,""
        ,"}"]
        (fromGregorian 2014 1 5)
        (fromGregorian 2014 1 12)

testAll = do
    testIsDay
    testIsDateFormat
    testIsDayInRange
    testIsTimeSheetLine
    testMakeMultiWordProj
    testFormatTimeSheetLine
    testGetDay

