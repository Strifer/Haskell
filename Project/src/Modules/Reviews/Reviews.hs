module Modules.Reviews.Reviews ( Role (Student, Staff)
                               , ReviewAssignment (ReviewAssignment)
                               , Review (Review, revScore, text)
                               , saveReview
                               , reviews
                               , reviewsByRole
                               , reviewsFor
                               , reviewsForUserByRole
                               ) where
import Modules.AssignmentsAndSubmissions.AssignmentsAndSubmissions
import qualified Data.Text as T
import Control.Exception
import Control.Monad
import System.IO
import System.Directory
import System.IO.Error
import System.Environment
import System.FilePath

-- | Constants used for testing
testAssignment = Assignment 2015 Homework 5
testRA         = ReviewAssignment "Janko" "Branko" Staff testAssignment
testReview     = Review testRA 10.0 (T.pack "good job")

-- | Default extension of saved review files.
reviewExtension = ".review"
--------------------------------------------------------------------------------
-- | Reviewers can have two roles, either regular students or staff members.
data Role = Student | Staff deriving (Eq, Ord, Show, Read)

-- | ReviewAssignment descriptor
data ReviewAssignment = ReviewAssignment { reviewer :: UserIdentifier -- the person writing the review
                                         , reviewee :: UserIdentifier -- the person being reviewed
                                         , role :: Role               -- the reviewer's role
                                         , assignment :: Assignment   -- the assignment being reviewed
                                         } deriving (Eq, Show, Read)

-- | Review descriptor which defines a written review.
data Review = Review { reviewAssignment :: ReviewAssignment -- descriptor containing data about who's reviewing who
                     , revScore :: Double                   -- the grade
                     , text :: T.Text                       -- the critique
                     } deriving (Show, Read)
--------------------------------------------------------------------------------

-- | Saves a given review to the database.
saveReview :: Review -> IO ()
saveReview r = do
    let revAss   = reviewAssignment r
    let filePath = (getAssignmentPath $ assignment $ revAss) </> (reviewee $ revAss)
    let fileName = "review-" ++ (reviewer revAss) ++ reviewExtension
    writeFile (filePath </> fileName) $ show r

-- | Provides a review descriptor based on its filepath if it exists.
reviewFromFilepath :: FilePath -> IO Review
reviewFromFilepath f = do
    e1 <- doesFileExist f
    let e2 = takeExtension f == reviewExtension
    case (e1, e2) of
        (False, _)          -> error "File does not exist"
        (True, False)       -> error "File is not review type"
        _                   -> do
                                s <- readFile f
                                let rs = read s :: Review
                                return rs

-- | Retrieves all reviews from some specific assignment based on the role of the reivewers.
reviewsByRole :: Assignment -> Role -> IO [Review]
reviewsByRole a r = do
    revs <- reviews a
    return $ filter ((==) r . role . reviewAssignment) revs

-- | Retrieves all reviews from some specific assignment based on the role of the reviewers and the person being reviewed.
reviewsForUserByRole :: Assignment -> UserIdentifier -> Role -> IO [Review]
reviewsForUserByRole a uid r = do
    revs <- reviewsFor a uid
    return $ filter ((==) r. role . reviewAssignment) revs

-- | Retrieves all reviews in an assigmet.
reviews :: Assignment -> IO [Review]
reviews a = do
    users <- listSubmissions a
    revs  <- mapM (reviewsFor a) users
    return $ concat revs


-- | Retrieves all reviews in an assignment for a given user.
-- | The assignment must be defined and the user has to have a submission.
reviewsFor :: Assignment -> UserIdentifier -> IO [Review]
reviewsFor a uid = do
    let filePath = getAssignmentPath a
    let filePathUID = filePath </> uid
    e1 <- doesDirectoryExist filePath
    e2 <- doesDirectoryExist filePathUID
    case (e1, e2) of 
        (False, False)      -> error "Assignment does not exist"
        (True,  False)      -> error "User hasn't uploaded assignment yet"
        _                   -> do
                                 filez <- getFileContents filePathUID
                                 mapM reviewFromFilepath (filter ((==) reviewExtension . takeExtension) filez)






