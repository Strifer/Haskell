module Modules.AssignmentsAndSubmissions.AssignmentsAndSubmissions ( Type (Homework, Exam, Project)
                                                                   , UserIdentifier
                                                                   , Configuration (Configuration, files, minScore, maxScore, required)
                                                                   , Assignment (Assignment, year, aType, number)
                                                                   , Submission (Submission)
                                                                   , assignmentsByYear
                                                                   , assignmentsByYearAndType
                                                                   , listSubmissions
                                                                   , getSubmission
                                                                   , getSubmissionPath
                                                                   , getConfiguration
                                                                   , getAssignmentPath
                                                                   , getFileContents
                                                                   , createAssignment
                                                                   , listFiles
                                                                   , upload
                                                                   ) where

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

-- Helper constants used for testing.
ass    = Assignment 2015 Homework 5
config = Configuration (UTCTime (fromGregorian 2015 20 1) 0) (UTCTime (fromGregorian 2015 27 1) 0) (UTCTime (fromGregorian 2015 28 1) 0) [] 0.0 10.0 5.0

-- The rootfolder of our "database"
rootFolder = "C:\\Users\\Filip\\Documents\\Database"
-- The default pdf name of an assignment .pdf.
pdfName = "Assignment.pdf"
-- The default name of an assignment's configuration file.
configName = ".configuration"

---------------------------------------------------------------------------------------------------------------------------------------------------------
-- | Academic year shorthand (e.g. 2015 for 2015/16)
type Year = Integer

-- | An assignment type
data Type = Homework | Exam | Project deriving (Read, Show, Eq)

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
                  } deriving (Eq, Read, Show)

-- | A submission data structure
data Submission = Submission
                  { assignment      :: Assignment     -- associated assignment
                  , submittedFiles  :: [FilePath]     -- submitted assignment files
                  , userID          :: UserIdentifier -- who is submitting the files
                  } deriving Show
---------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Retrieves all assignments in a given academic year.
assignmentsByYear :: Year -> IO [Assignment]
assignmentsByYear y = do
  let filePath      = rootFolder </> (show y)
  names            <- getDirectoryContents filePath
  let filteredNames = filter (`notElem` [".",".."]) names
  assignments      <- mapM (assignmentsByYearAndType y) ((map read filteredNames) :: [Type])
  return $ concat assignments

-- | Retrieves all assignments of a certain type in a given year.
assignmentsByYearAndType :: Year -> Type -> IO [Assignment]
assignmentsByYearAndType y t = do
  let filePath      = rootFolder </> (show y) </> (show t)
  names            <- getDirectoryContents filePath
  let filteredNames = filter (`notElem` [".",".."]) names
  return $ map (Assignment y t) ((map read filteredNames) :: [Int])


-- | Lists all the user names of people who've made submissions in a given assignment.
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

-- | Returns the filepath of the provided submissions.
getSubmissionPath :: Submission -> FilePath
getSubmissionPath sub = (getAssignmentPath $ assignment sub) </> userID sub

-- | Builds a submission based on a provided assignment and user id.
getSubmission :: Assignment -> UserIdentifier -> IO Submission
getSubmission a uid = do
   let filePath = getAssignmentPath a </> uid
   e <- doesDirectoryExist filePath
   if (not e) then error "Assignment does not exist"
              else do
                  filess <- getFileContents filePath
                  return $ Submission a filess uid

-- | Creates and saves an assignment to the database based on its config file and descriptor.
createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment a conf f = do
   let filePath = getAssignmentPath a
   e <- doesDirectoryExist filePath
   if e then putStrLn "Assignment already exists"
        else do
            createDirectoryIfMissing True filePath
            copyFile f (filePath </> pdfName)
            writeFile (filePath </> configName) $ show conf

-- | Returns an assignment descriptor based on its (possible) filepath.
getAssignmentFromPath :: FilePath -> Assignment
getAssignmentFromPath path = Assignment year atype number
            where  xs      = reverse $ take 3 $ reverse $ splitOn (pathSeparator:[]) path
                   year    = read $ xs!!0
                   atype   = read $ xs!!1
                   number  = read $ xs!!2

-- | Calculates a filepath based on an assignment descriptor.
getAssignmentPath :: Assignment -> FilePath
getAssignmentPath a = rootFolder </> (show $ year a) </> (show $ aType a) </> (show $ number a)


-- | Retrieves the config file of an assignment if it exists.
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

-- | Retrieves all of the files contained in some provided directory as a list of filepaths.
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


-- | Lists all the files contained in a submission.
listFiles :: Submission -> IO [FilePath]
listFiles = return . submittedFiles

-- | Uploads a single user submitted file to the database.
-- | The filename must be allowed and the assignment must exist within the database.
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

-- | Updates a submission by saving the file to the database.
updateSubmission :: Assignment -> FilePath -> UserIdentifier -> T.Text -> String -> IO Submission
updateSubmission a path uid fileText fileName = do
   cs <- getConfiguration a
   let filez = files cs
   if (null filez) then makeFile a path uid fileText fileName
                   else do
                     if (fileName `notElem` filez) then error $ "Filename " ++ fileName ++ " is not allowed."
                                                   else makeFile a path uid fileText fileName

-- | Saves a file to the database and retrieves a new submission descriptor.
makeFile :: Assignment -> FilePath -> UserIdentifier -> T.Text -> String -> IO Submission
makeFile a path uid fileText fileName = do
   O.writeFile (path </> fileName) fileText
   getSubmission a uid