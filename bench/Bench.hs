import Control.Monad (void)
import Criterion.Main
import System.Process (readProcess)

main :: IO ()
main = defaultMain [
        bgroup "Powerline"    $ cases "powerline",
        bgroup "Powerline-hs" $ cases "powerline-hs",
        bench "Python Hello World" . whnfIO . void $ readProcess "python2" ["-c", "print('Hello')"] ""
    ]
  where
      cases exe = [
              bench "Left prompt"  . whnfIO $ runPowerline exe "aboveleft",
              bench "Right prompt" . whnfIO $ runPowerline exe "right"
          ]

runPowerline :: String -> String -> IO ()
runPowerline exe side = void $ readProcess exe args "" where
    args = [
        "--last-exit-code=0",
        "--last-pipe-status=1 2 3",
        "--renderer-arg=client_id=7822",
        "--renderer-arg=shortened_path=/usr/bin",
        "--jobnum=0",
        "--renderer-arg=mode=viins",
        "--renderer-arg=default_mode=emacs",
        "--width=171",
        "shell",
        side
        ]

