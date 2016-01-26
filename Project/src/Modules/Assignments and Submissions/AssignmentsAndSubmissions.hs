import Control.Exception
import Data.Time
import Data.List
import Data.List.Split
import Control.Monad
import System.IO
import System.Directory
import System.IO.Error
import System.Environment
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as O

-- | Academic year shorthand (e.g. 2015 for 2015/16)
type Year = Integer

ass    = Assignment 2015 Homework 5

config = Configuration (UTCTime (fromGregorian 2015 20 1) 0) (UTCTime (fromGregorian 2015 27 1) 0) (UTCTime (fromGregorian 2015 28 1) 0) [] 0.0 10.0 5.0

rootFolder = "C:\\Users\\Filip\\Documents\\Database"
pdfName = "Assignment.pdf"
configName = ".configuration"

-- | An assignment type
data Type = Homework | Exam | Project deriving (Read, Show)

-- | Unique identificator of a user who can make submissions.
type UserIdentifier = String

-- | An assignment configuration data structure
-- | If files is an empty list, any number of  files can be uploaded.
data Configuration = Configuration 
                     { published    :: UTCTime  -- When to publish
                     , deadline     :: UTCTime  -- Submisssion deadline
                     , lateDeadline :: UTCTime  -- Late submission deadline
                     , files        :: [String] -- Expected file names
                     , minScore     :: Double   -- Minimum achievable score
                     , maxScore     :: Double   -- Maximum achievable score
                     , required     :: Double   -- Score required to pass
                     } deriving (Read, Show)


-- | An assignment descriptor
data Assignment = Assignment
                  { year   :: Year
                  , aType   :: Type
                  , number :: Int
                  } deriving Show

-- | A submission data structure
data Submission = Submission
                  { assignment      :: Assignment     -- associated assignment
                  , submittedFiles  :: [FilePath]
                  , userID          :: UserIdentifier -- who is submitting the files
                  } deriving Show

listSubmissions :: Assignment -> IO [UserIdentifier]
listSubmissions a = do
   let filePath = getAssignmentPath a
   e <- doesDirectoryExist filePath
   if (not e) then error "Assignment does not exist"
              else do
                  let myFilter x = snd x && (not $ isPrefixOf "." $ fst x)
                  xs <- getDirectoryContents filePath
                  dirs <- mapM doesDirectoryExist (map (filePath </>) xs)
                  return $ map fst $ filter (myFilter) $ zip xs dirs

getSubmissionPath :: Submission -> FilePath
getSubmissionPath sub = (getAssignmentPath $ assignment sub) </> userID sub

getSubmission :: Assignment -> UserIdentifier -> IO Submission
getSubmission a uid = do
   let filePath = getAssignmentPath a </> uid
   e <- doesDirectoryExist filePath
   if (not e) then error "Assignment does not exist"
              else do
                  filess <- getFileContents filePath
                  return $ Submission a filess uid

createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment a conf f = do
   let filePath = getAssignmentPath a
   e <- doesDirectoryExist filePath
   if e then putStrLn "Assignment already exists"
        else do
            createDirectoryIfMissing True filePath
            copyFile f (filePath </> pdfName)
            writeFile (filePath </> configName) $ show conf

getAssignmentFromPath :: FilePath -> Assignment
getAssignmentFromPath path = Assignment year atype number
            where  xs      = reverse $ take 3 $ reverse $ splitOn (pathSeparator:[]) path
                   year    = read $ xs!!0
                   atype   = read $ xs!!1
                   number  = read $ xs!!2


getAssignmentPath :: Assignment -> FilePath
getAssignmentPath a = rootFolder </> (show $ year a) </> (show $ aType a) </> (show $ number a)



getConfiguration :: Assignment -> IO Configuration
getConfiguration a = do
   let filePath = getAssignmentPath a
   e <- doesDirectoryExist filePath
   if (not e) then error "Assignment does not exist"
              else do
                 result <- try (readFile $ filePath </> configName)
                 case result of
                     Left  e   -> error $ "Error: " ++ ioeGetErrorString e
                     Right e   -> return $ read e

getFileContents :: FilePath -> IO [FilePath]
getFileContents dir = do
   names <- getDirectoryContents dir
   let filteredNames = filter (`notElem` [".",".."]) names
   paths <- forM filteredNames recurseDown
   return (concat paths)
   where recurseDown name = do
            let newPath = dir </> name
            e <- doesDirectoryExist newPath
            if e then getFileContents newPath
                           else return [newPath]


listFiles :: Submission -> IO [FilePath]
listFiles = return . submittedFiles

upload :: Assignment -> UserIdentifier -> T.Text -> String -> IO Submission
upload a uid fileText fileName = do
   let filePath = getAssignmentPath a
   e <- doesDirectoryExist filePath
   if (not e) then error "Assignment does not exist"
              else do
                  let workingDir = filePath </> uid
                  e2 <- doesDirectoryExist workingDir
                  if (not e2) then do 
                                    createDirectory workingDir
                                    updateSubmission a workingDir uid fileText fileName
                              else  updateSubmission a workingDir uid fileText fileName


updateSubmission :: Assignment -> FilePath -> UserIdentifier -> T.Text -> String -> IO Submission
updateSubmission a path uid fileText fileName = do
   cs <- getConfiguration a
   let filez = files cs
   if (null filez) then makeFile a path uid fileText fileName
                   else do
                     if (fileName `notElem` filez) then error $ "Filename " ++ fileName ++ " is not allowed."
                                                   else makeFile a path uid fileText fileName


makeFile :: Assignment -> FilePath -> UserIdentifier -> T.Text -> String -> IO Submission
makeFile a path uid fileText fileName = do
   O.writeFile (path </> fileName) fileText
   getSubmission a uid