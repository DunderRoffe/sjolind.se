module ArbitraryDatabase where

import AbsDatabase
import Test.QuickCheck

import Data.Text as Text
import Data.ByteString as ByteString

instance Arbitrary Database where
    arbitrary = do
        projMap <- arbitrary
        return $ Database projMap

instance Arbitrary Project where
    arbitrary = do
        pName  <- arbitrary
        pAbout <- arbitrary
        pPostM <- arbitrary
        pFileM <- arbitrary
        pSubP  <- arbitrary
        return $ Project pName pAbout pPostM pFileM pSubP

instance Arbitrary Post where
    arbitrary = do
        pAuthor <- arbitrary
        pHead   <- arbitrary
        pDate   <- arbitrary
        pCont   <- arbitrary
        pComm   <- arbitrary
        return $ Post pAuthor pHead pDate pCont pComm

instance Arbitrary Comment where
    arbitrary = do
        cAuthor <- arbitrary
        cDate   <- arbitrary
        cCont   <- arbitrary
        cComm   <- arbitrary
        return $ Comment cAuthor cDate cCont cComm

instance Arbitrary File where
    arbitrary = do
        fName <- arbitrary
        fData <- arbitrary
        return $ File fName fData

instance Arbitrary Author where
    arbitrary = do
        aName <- arbitrary
        aImg  <- arbitrary
        aUri  <- arbitrary
        return $ Author aName aImg aUri

instance Arbitrary Text where
    arbitrary = do
        str  <- arbitrary
        return $ Text.pack str

instance Arbitrary ByteString where
    arbitrary = do
        str <- arbitrary
        return $ ByteString.pack str
