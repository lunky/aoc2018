
module Day4Spec where
import Day4
import Test.Hspec

spec :: Spec
spec =  do
    describe "test harness" $
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "day4" $
        it "should run test case" $ do
            let input = "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up"
            let expected = 240
            day4 input `shouldBe` expected
    -- describe "parseDate" $ do 
    --     it "should parse a date" $ do
    --         let input = "[1518-11-01 00:00] Guard #10 begins shift"
    --         1 `shouldBe` 1
    
                    