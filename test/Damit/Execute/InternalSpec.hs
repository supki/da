module Damit.Execute.InternalSpec (spec) where

import Test.Hspec

import Damit.Execute.Internal


spec :: Spec
spec = do
  context "local commands" $ do
    it "handle spaces in the arguments correctly" $
      pretty (run "echo" (args ["foo bar", "baz"])) `shouldBe` "echo foo\\ bar baz"

    it "handle fancy shell related symbols such as ‘&’ adequately" $
      pretty (run "sh" (args ["-c", "true && false"])) `shouldBe` "sh -c true\\ \\&\\&\\ false"

    it "handle spaces in the environment variables" $
      pretty (run "true" (env ["foo" .= "bar baz"]))
     `shouldBe`
      "env foo=bar\\ baz true"

  context "external commands" $ do
    it "escapes the 1st hop's arguments twice" $ do
      pretty (hop "remote" mempty (run "echo" (args ["foo bar", "baz &"])))
     `shouldBe`
      "ssh remote -t echo foo\\\\ bar baz\\\\ \\\\&"

    it "escapes the 2nd hop's arguments thrice" $ do
      pretty (hop "remote1" mempty (hop "remote2" mempty (run "echo" (args ["foo bar", "baz &"]))))
     `shouldBe`
      "ssh remote1 -t ssh remote2 -t echo foo\\\\\\\\ bar baz\\\\\\\\ \\\\\\\\&"

  context "ssh" $ do
    it "supports -l option" $
      pretty (hop "remote" (login "root") (run "/bin/true" mempty))
     `shouldBe`
      "ssh remote -l root -t /bin/true"

    it "supports -i option" $
      pretty (hop "remote" (identity "work_rsa") (run "/bin/true" mempty))
     `shouldBe`
      "ssh remote -i work_rsa -t /bin/true"
