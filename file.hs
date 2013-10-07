import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch::[(String,[String]->IO ())]
dispatch = [	("add", add),
		("view", view),
		("delete", delete)	]

main = do
	command:args <- getArgs
	let (Just action) = lookup command dispatch
	action args

add::[String] -> IO ()
add ([filename, line]) = do
	appendFile filename (line ++ "\n")
	return IO ()
