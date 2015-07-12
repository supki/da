guard :haskell, all_on_start: true, all_on_pass: true, cmd: "cabal exec -- ghci -itest -ilib -ignore-dot-ghci -Wall test/Spec.hs" do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{lib/.+\.l?hs$})
end
