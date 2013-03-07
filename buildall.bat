ghc -fforce-recomp -o bin\ListFiles.exe --make ListFiles -outputdir bin -Wall -Werror
ghc -fforce-recomp -o bin\ListTokens.exe --make ListTokens -outputdir bin -Wall -Werror
ghc -fforce-recomp -o bin\CreateTree.exe --make  CreateTree -outputdir bin -Wall -Werror
ghc -fforce-recomp -o bin\Main.exe --make  Main -outputdir bin -Wall -Werror

hlint .
