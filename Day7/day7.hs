import Data.List
import Data.Char
import Debug.Trace

main = do 
    readFile "input.txt" >>= print . solve1 . parse . lines
    readFile "input.txt" >>= print . solve2 . parse . lines

type Path = [String]

data DirItem = Dir { path::Path, children::[DirItem]}  
    | File { path::Path, size:: Int} deriving (Show, Eq)

root :: DirItem 
root = Dir { path = ["/"], children=[] }

-- Add directory to file tree starting with DirItem @this, on path newPath with name newName 
addDir :: DirItem -> Path -> String -> DirItem
addDir this@(Dir dirPath children) newPath newName 
    | dirPath == newPath = 
        let newChild = Dir (newPath ++ [newName]) []
        in -- trace ("addDir " ++ show dirPath) $ 
            Dir dirPath (newChild:children)
    | otherwise = 
        let (modifyChild, otherChildren) = partition (\x -> (path x) `isPrefixOf` newPath) children 
            newChildDir = addDir (modifyChild!!0) newPath newName 
        in -- trace ("addDir_ " ++ show dirPath) $ 
            Dir dirPath (newChildDir:otherChildren)

-- Add files to file tree starting with DirItem @this, on path newPath with name newName and size newSize 
addFile :: DirItem -> Path -> String -> Int -> DirItem
addFile this@(Dir dirPath children) newPath newName newSize
    | dirPath == newPath = 
        let newFile = File (newPath ++ [newName]) newSize
        in  -- trace ("addFile " ++ show dirPath ++ " " ++ show newFile) $
            Dir dirPath (newFile:children)
    | otherwise = 
        let (modifyChild, otherChildren) = partition (\x -> (path x) `isPrefixOf` newPath) children 
            newChildDir = addFile (modifyChild!!0) newPath newName newSize 
        in  -- trace ("addFile_ " ++ show dirPath ++ " -> " ++ show newPath ++ "(" ++ show modifyChild ++ ")") $
            Dir dirPath (newChildDir:otherChildren)

getDir :: DirItem -> Path -> DirItem 
getDir d fullPath
    | path d == fullPath = d
    | otherwise =
        let child = case (find (\x -> (path x) `isPrefixOf` fullPath) (children d)) of 
                        Just c -> c
        in getDir child fullPath

addLine :: (DirItem,Path) -> String -> (DirItem, Path)
--addLine ctx@(files, cwd) "$ cd /" = Dir(files, []) 
addLine ctx@(files, cwd) cmd 
    | "$ cd .." == cmd = (files, init cwd)
    | "$ cd" `isPrefixOf` cmd = 
        let dirName = drop 5 cmd 
        in --trace ("cd: " ++ show cmd ++ " name: " ++ dirName ++ " " ++ show ctx)
            (files, cwd ++ [dirName] )
    | "$ ls" `isPrefixOf` cmd = --trace ("ls " ++ show ctx)
        ctx
    | "dir" `isPrefixOf` cmd = 
        let name = drop 4 cmd 
        in -- trace ("dir: " ++ show cmd ++ " " ++ show ctx)
            (addDir files cwd name, cwd)
    | otherwise = 
        let (size:name:_) = words cmd
            sizeInt :: Int = read size
        in  -- trace ("file: " ++ show cmd ++ " " ++ show ctx) 
            (addFile files cwd name sizeInt, cwd)

parse :: [String] -> (DirItem, Path)
parse s = foldl addLine (root, []) s

-- part 1
sumFiles :: DirItem -> Int
sumFiles (File _ size) = size
sumFiles (Dir _ []) = 0
sumFiles (Dir _ children) = sum $ map sumFiles children

directories :: DirItem -> [DirItem]
directories files@(Dir _ []) = [files]
directories files@(Dir _ children) = foldl (\acc x -> acc  ++ (directories x)) [files] children
directories (File _ _) = []

solve1 :: (DirItem, Path) -> String
solve1 (files, _) = 
    show $ sum $ filter (<100000) $ map sumFiles $ directories $ files

solve2 :: (DirItem, Path) -> String
solve2 (files, _) = 
    let amountNeeded = 30000000 - (70000000 - (sumFiles files)) 
        directorySizes = map sumFiles $ directories $ files
    in trace ("Directory sizes: " ++ show directorySizes ++ "Amount needed: " ++ show amountNeeded)
    show ( minimum $ filter (>amountNeeded) directorySizes )

sample = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n\
\$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"

