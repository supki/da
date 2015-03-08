module Damit.Execute.TmuxSpec (spec) where

import Test.Hspec

import Damit.Execute
import Damit.Execute.Tmux


spec :: Spec
spec = do
  it "supports -S option" $
    pretty (tmux "name" (socket "/home/user/default"))
   `shouldBe`
    "tmux -S /home/user/default new-session -AD -s name"

  it "supports working directory customization" $
    pretty (tmux "name" (directory "/tmp"))
   `shouldBe`
    "tmux new-session -AD -s name -c /tmp"

  it "can run custom commands" $
    pretty (tmux "name" (cmd "/bin/true"))
   `shouldBe`
    "tmux new-session -AD -s name /bin/true"

  it "can run custom tmux binary" $
    pretty (tmux "name" (binary "/home/user/bin/tmux"))
   `shouldBe`
    "/home/user/bin/tmux new-session -AD -s name"
