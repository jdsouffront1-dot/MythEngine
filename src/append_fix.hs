appendLog :: FilePath -> Int -> UTCTime -> String -> String -> String -> IO ()
appendLog path c ts param oldV newV = do
  let line = unwords [show ts, show c, param, oldV, "->", newV] ++ "\n"
  appendFile path line


